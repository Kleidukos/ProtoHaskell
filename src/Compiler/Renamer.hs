-- |
-- The Renamer is tasked with:
-- 1. Turning 'ParsedName's into 'Name's.
-- 2. lexical analysis like:
--   * out-of-scope variables
--   * unused bindings
--   * unused imports
-- 3. Producing the type environment that will be given to the typechecker
module Compiler.Renamer where

import Compiler.BasicTypes.FastString
import Compiler.BasicTypes.Name
import Compiler.BasicTypes.OccName
import Compiler.BasicTypes.ParsedName
import Compiler.BasicTypes.SrcLoc
import Compiler.BasicTypes.Unique
import Compiler.PhSyn.PhExpr
import Compiler.PhSyn.PhSyn
import Compiler.PhSyn.PhType

import Control.Monad (forM, forM_, mapM)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Text.Pretty.Simple (pPrint)

data RenamerError
  = NoTopLevelSig
  | DuplicateBinding FastString -- (Vector SrcSpan)
  deriving stock (Eq, Show)

-- | This datastructure is only used for the purpose
-- of lexical analysis. It does not represent the reality
-- of the code as it is written.
data TypeSignature = TypeSignature
  { signatureHead :: Name
  , body :: PhType Name
  }
  deriving stock (Eq, Show)

data RenamerContext = RenamerContext
  { nameTypes :: Map Name TypeSignature
  , uniqueSupply :: UniqueSupply
  }
  deriving stock (Eq, Show)

type Renamer a = Eff [State RenamerContext, Error RenamerError, IOE] a

runRenamer
  :: Renamer a
  -> IO (Either RenamerError (a, RenamerContext))
runRenamer action =
  action
    & runState emptyRenamerContext
    & runErrorNoCallStack
    & runEff

rename :: PhModule ParsedName -> IO (Either RenamerError (PhModule Name, RenamerContext))
rename parsedModule =
  runRenamer $
    renamePhModule parsedModule
      >>= ensureTopLevelSignatures

emptyRenamerContext :: RenamerContext
emptyRenamerContext =
  RenamerContext
    { nameTypes = Map.empty
    , uniqueSupply = mkUniqueSupply RenameSection
    }

nextUnique :: Renamer Unique
nextUnique = do
  state
    ( \context ->
        let s = context.uniqueSupply.uniques
            (returnValue, newSupply) = (head s, UniqueSupply $ tail s)
            newState = context{uniqueSupply = newSupply}
         in (returnValue, newState)
    )

-- Top-level binding analysis: must have type signature --

-- We need to find the signatures with the same nameFS as the bindings.
-- Simply looking up 'Name's cannot work.
ensureTopLevelSignatures :: PhModule Name -> Renamer (PhModule Name)
ensureTopLevelSignatures mod = do
  let decls = fmap unLoc mod.modDecls
  let topLevelBindings = extractTopLevelBindings decls
  let topLevelSignatures = extractTopLevelSignatures decls
  Foldable.forM_ topLevelBindings $ \binding ->
    case MultiSet.occur (binding.occ.nameFS) topLevelSignatures of
      0 -> throwError NoTopLevelSig
      1 -> pure ()
      _ -> throwError $ DuplicateBinding binding.occ.nameFS
  pure mod

extractTopLevelBindings :: [PhDecl Name] -> [Name]
extractTopLevelBindings decls = bindings
  where
    bindingDecls = foldMap getBinding decls
    bindings =
      foldMap
        ( \case
            FunBind name _ -> [name]
            PatBind (Located _ (PVar name)) _ -> [name]
            _ -> []
        )
        bindingDecls

getBinding :: PhDecl Name -> [PhBind Name]
getBinding (Binding b) = [b]
getBinding _ = []

extractTopLevelSignatures :: [PhDecl Name] -> MultiSet FastString
extractTopLevelSignatures decls = foldMap getTypeSignature decls

getTypeSignature :: PhDecl Name -> MultiSet FastString
getTypeSignature (Signature (TypeSig names phType)) =
  MultiSet.fromList $ fmap (\n -> n.occ.nameFS) names
getTypeSignature _ = MultiSet.empty

-- Renaming ParsedName to Name --

renameParsedName :: ParsedName -> Renamer Name
renameParsedName parsedName = do
  unique <- nextUnique
  pure $
    Name
      { sort = Internal
      , occ = parsedNameOcc parsedName
      , uniq = unique
      }

renamePhModule :: PhModule ParsedName -> Renamer (PhModule Name)
renamePhModule mod = do
  renamedDecls <- traverse renameLocatedPhDecl mod.modDecls
  pure $! Module mod.modName renamedDecls

renameLocatedPhDecl :: LPhDecl ParsedName -> Renamer (LPhDecl Name)
renameLocatedPhDecl = mapLocM renamePhDecl

renamePhDecl :: PhDecl ParsedName -> Renamer (PhDecl Name)
renamePhDecl (Binding phBind) = Binding <$> renameBind phBind
renamePhDecl (Signature sig) = Signature <$> renameSignature sig
renamePhDecl (DataDecl parsedName parsedNames constructorDecls) =
  DataDecl
    <$> renameParsedName parsedName
    <*> traverse renameParsedName parsedNames
    <*> traverse renameConDecl constructorDecls
renamePhDecl (ClassDecl superclasses className target locatedLocalBinds) =
  ClassDecl
    <$> renamePredicates superclasses
    <*> renameParsedName className
    <*> renameParsedName target
    <*> renameLocatedLocalBinds locatedLocalBinds
renamePhDecl (InstDecl context className target locatedLocalBinds) =
  InstDecl
    <$> renamePredicates context
    <*> renameParsedName className
    <*> renameLocatedPhType target
    <*> renameLocatedLocalBinds locatedLocalBinds

renameConDecl :: ConDecl ParsedName -> Renamer (ConDecl Name)
renameConDecl (ConDecl parsedName phTypes) =
  ConDecl
    <$> renameParsedName parsedName
    <*> traverse renamePhType phTypes

renameBind :: PhBind ParsedName -> Renamer (PhBind Name)
renameBind (FunBind parsedName matchGroup) = do
  name <- renameParsedName parsedName
  renamedMatchGroup <- renameMatchGroup matchGroup
  pure $! FunBind name renamedMatchGroup
renameBind (PatBind lPat lRHS) =
  PatBind
    <$> renameLocatedPattern lPat
    <*> renameLRHS lRHS

renameMatchGroup :: MatchGroup ParsedName -> Renamer (MatchGroup Name)
renameMatchGroup mg = do
  renamedAlternatives <- traverse renameLMatch mg.alternatives
  pure MG{context = mg.context, alternatives = renamedAlternatives}

renameLMatch :: LMatch ParsedName -> Renamer (LMatch Name)
renameLMatch lMatch = mapLocM renameMatch lMatch

renameMatch :: Match ParsedName -> Renamer (Match Name)
renameMatch match = Match <$> renameLocatedPatterns match.matchPats <*> renameLRHS match.rhs

renameLRHS :: LRHS ParsedName -> Renamer (LRHS Name)
renameLRHS = mapLocM renameRHS

renameRHS :: RHS ParsedName -> Renamer (RHS Name)
renameRHS rhs = RHS <$> renameLGRHS rhs.grhs <*> renameLocatedLocalBinds rhs.localBinds

renameLocatedLocalBinds :: LPhLocalBinds ParsedName -> Renamer (LPhLocalBinds Name)
renameLocatedLocalBinds = mapLocM renameLocalBinds

renameLocalBinds :: PhLocalBinds ParsedName -> Renamer (PhLocalBinds Name)
renameLocalBinds (LocalBinds locatedBinds locatedSignatures) =
  LocalBinds
    <$> renameLocatedBinds locatedBinds
    <*> renameLocatedSignatures locatedSignatures

renameLocatedBinds :: [LPhBind ParsedName] -> Renamer [LPhBind Name]
renameLocatedBinds binds = traverse renameLocatedBind binds
  where
    renameLocatedBind :: LPhBind ParsedName -> Renamer (LPhBind Name)
    renameLocatedBind locatedBind = mapLocM renameBind locatedBind

renameLocatedSignatures :: [LSig ParsedName] -> Renamer [LSig Name]
renameLocatedSignatures locatedSigs = traverse renameLocatedSignature locatedSigs

renameLocatedSignature :: LSig ParsedName -> Renamer (LSig Name)
renameLocatedSignature lsig = mapLocM renameSignature lsig

renameSignatures :: [Sig ParsedName] -> Renamer [Sig Name]
renameSignatures sigs = traverse renameSignature sigs

renameSignature :: Sig ParsedName -> Renamer (Sig Name)
renameSignature (TypeSig parsedNames locatedPhType) =
  TypeSig <$> traverse renameParsedName parsedNames <*> renameLocatedPhType locatedPhType

-- renameSignature (FixitySig a i parsedNames) = do
--   renamedNames <- traverse renameParsedName parsedNames
--   pure $ FixitySig a i renamedNames

renameLocatedPhType :: LPhType ParsedName -> Renamer (LPhType Name)
renameLocatedPhType = mapLocM renamePhType

renamePhType :: PhType ParsedName -> Renamer (PhType Name)
renamePhType (PhVarTy parsedName) = PhVarTy <$> renameParsedName parsedName
renamePhType (PhBuiltInTyCon builtinTyCon) = pure $ PhBuiltInTyCon builtinTyCon
renamePhType (PhQualTy predicates lPhType) = PhQualTy <$> renamePredicates predicates <*> renameLocatedPhType lPhType
renamePhType (PhAppTy lPhType1 lPhType2) = PhAppTy <$> renameLocatedPhType lPhType1 <*> renameLocatedPhType lPhType2
renamePhType (PhFunTy lPhType1 lPhType2) = PhFunTy <$> renameLocatedPhType lPhType1 <*> renameLocatedPhType lPhType2
renamePhType (PhListTy lPhType) = PhListTy <$> renameLocatedPhType lPhType
renamePhType (PhTupleTy lPhTypes) = PhTupleTy <$> traverse renameLocatedPhType lPhTypes
renamePhType (PhParTy lPhType) = PhParTy <$> renameLocatedPhType lPhType

renamePredicates :: [Pred ParsedName] -> Renamer [Pred Name]
renamePredicates = traverse renamePredicate

renamePredicate :: Pred ParsedName -> Renamer (Pred Name)
renamePredicate (IsIn parsedName phType) = IsIn <$> renameParsedName parsedName <*> renamePhType phType

renameLGRHS :: LGRHS ParsedName -> Renamer (LGRHS Name)
renameLGRHS = mapLocM renameGRHS

renameGRHS :: GuardedRHS ParsedName -> Renamer (GuardedRHS Name)
renameGRHS (Unguarded lExpr) = Unguarded <$> renameLocatedExpr lExpr

renameLocatedExpr :: LPhExpr ParsedName -> Renamer (LPhExpr Name)
renameLocatedExpr = mapLocM renameExpr

renameExpr :: PhExpr ParsedName -> Renamer (PhExpr Name)
renameExpr (PhVar parsedName) = PhVar <$> renameParsedName parsedName
renameExpr (PhLit lit) = pure $! PhLit lit
renameExpr (PhLam matchGroup) = PhLam <$> renameMatchGroup matchGroup
renameExpr (PhApp lExpr1 lExpr2) = PhApp <$> renameLocatedExpr lExpr1 <*> renameLocatedExpr lExpr2
renameExpr (OpApp leftOperand operator rightOperand) =
  OpApp
    <$> renameLocatedExpr leftOperand
    <*> renameLocatedExpr operator
    <*> renameLocatedExpr rightOperand
renameExpr (NegApp lExpr) = NegApp <$> renameLocatedExpr lExpr
renameExpr (PhPar lExpr) = PhPar <$> renameLocatedExpr lExpr
renameExpr (PhCase lExpr matchGroup) =
  PhCase
    <$> renameLocatedExpr lExpr
    <*> renameMatchGroup matchGroup
renameExpr (PhIf predicate consequent alternative) =
  PhIf
    <$> renameLocatedExpr predicate
    <*> renameLocatedExpr consequent
    <*> renameLocatedExpr alternative
renameExpr (PhLet locatedLocalBinds lExpr) =
  PhLet
    <$> renameLocatedLocalBinds locatedLocalBinds
    <*> renameLocatedExpr lExpr
renameExpr (PhDo lStatements) = PhDo <$> renameLocatedStatements lStatements
renameExpr (ExplicitTuple lTupleArguments) = ExplicitTuple <$> traverse renameLocatedExpr lTupleArguments
renameExpr (ExplicitList lExprs) = ExplicitList <$> traverse renameLocatedExpr lExprs
renameExpr (ArithSeq arithSeqInfo) = ArithSeq <$> renameArithSeqInfo arithSeqInfo
renameExpr (Typed lPhType lExpr) = Typed <$> renameLocatedPhType lPhType <*> renameLocatedExpr lExpr

renameArithSeqInfo :: ArithSeqInfo ParsedName -> Renamer (ArithSeqInfo Name)
renameArithSeqInfo (From lExpr) = From <$> renameLocatedExpr lExpr
renameArithSeqInfo (FromThen lExpr1 lExpr2) =
  FromThen
    <$> renameLocatedExpr lExpr1
    <*> renameLocatedExpr lExpr2
renameArithSeqInfo (FromTo lExpr1 lExpr2) =
  FromTo
    <$> renameLocatedExpr lExpr1
    <*> renameLocatedExpr lExpr2
renameArithSeqInfo (FromThenTo lExpr1 lExpr2 lExpr3) =
  FromThenTo
    <$> renameLocatedExpr lExpr1
    <*> renameLocatedExpr lExpr2
    <*> renameLocatedExpr lExpr3

renameLocatedStatements :: [LStmt ParsedName] -> Renamer [LStmt Name]
renameLocatedStatements = traverse renameLocatedStatement

renameLocatedStatement :: LStmt ParsedName -> Renamer (LStmt Name)
renameLocatedStatement = mapLocM renameStatement

renameStatement :: Stmt ParsedName -> Renamer (Stmt Name)
renameStatement (SExpr lExpr) = SExpr <$> renameLocatedExpr lExpr
renameStatement (SGenerator lPat lExpr) = SGenerator <$> renameLocatedPattern lPat <*> renameLocatedExpr lExpr
renameStatement (SLet locatedLocalBinds) = SLet <$> renameLocatedLocalBinds locatedLocalBinds

renameLocatedPatterns :: [LPat ParsedName] -> Renamer [LPat Name]
renameLocatedPatterns patterns = traverse renameLocatedPattern patterns

renameLocatedPattern :: LPat ParsedName -> Renamer (LPat Name)
renameLocatedPattern = mapLocM renamePattern

renamePatterns :: [Pat ParsedName] -> Renamer [Pat Name]
renamePatterns patterns = traverse renamePattern patterns

renamePattern :: Pat ParsedName -> Renamer (Pat Name)
renamePattern (PVar parsedName) = PVar <$> renameParsedName parsedName
renamePattern (PCon parsedName patterns) = PCon <$> renameParsedName parsedName <*> renamePatterns patterns
renamePattern (PAs parsedName pat) = PAs <$> renameParsedName parsedName <*> renamePattern pat
renamePattern (PLit lit) = pure $! PLit lit
renamePattern PWild = pure PWild
renamePattern (PTuple patterns) = PTuple <$> renamePatterns patterns
renamePattern (PList patterns) = PList <$> renamePatterns patterns
renamePattern (ParPat pat) = ParPat <$> renamePattern pat
