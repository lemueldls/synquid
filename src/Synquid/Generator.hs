{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Synquid.Generator (generateFromGoal) where

import Synquid.Logic
import Synquid.Type hiding (set)
import Synquid.Program
import Synquid.Error
import Synquid.SolverMonad
import Synquid.Util
import Synquid.Pretty
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Foldable as F
import Control.Monad.Logic
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative hiding (empty)
import Control.Lens ( (^.) )
import qualified Text.PrettyPrint.ANSI.Leijen as L
import Debug.Trace

-- defineGen :: Id -> Int -> RType -> RSchema
-- defineGen name arity body = do
--   Monotype $ FunctionT name (fold

gen :: RProgram
gen = Program (PSymbol "<gen>") (ScalarT IntT (BoolLit True))

randPure :: Formula -> RProgram
-- randPure fml = Program (PSymbol (show (pretty fml))) (ScalarT IntT (BoolLit True))
randPure fml = Program (PApp (Program (PSymbol "<rand@3#pure>") (ScalarT IntT (BoolLit True))) (Program (PSymbol (show (pretty fml))) (ScalarT IntT (BoolLit True)))) (ScalarT IntT (BoolLit True))

randInt :: RProgram
randInt = Program (PSymbol "<rand@0#int>") (ScalarT IntT (BoolLit True))

randBool :: RProgram
randBool = Program (PSymbol "<rand@0#bool>") (ScalarT BoolT (BoolLit True))

-- | Synthesize a generator and its return type from a goal
generateFromGoal :: Goal -> (RProgram, Goal)
generateFromGoal goal =
  let name = gName goal
      unresolved = env ^. unresolvedConstants
      (generator, returnType) = case gSpec goal of
        Monotype t -> generateFromTopLevelType name [] t
        ForallT a t -> todo
        ForallP a t -> todo
      env = gEnvironment goal
      spec = baseTypeToSchemaSkeleton returnType
  in (generator, goal {
    gName = "gen " ++ name,
    -- gEnvironment = env { _unresolvedConstants = Map.insert name spec unresolved },
    gSpec = spec
  })

baseTypeToSchemaSkeleton :: BaseType Formula -> RSchema
baseTypeToSchemaSkeleton t = case t of
  BoolT -> Monotype $ ScalarT BoolT (BoolLit True)
  IntT -> Monotype $ ScalarT IntT (BoolLit True)
  DatatypeT name tArgs pArgs -> todo
  TypeVarT {} -> todo

generateFromTopLevelType :: Id -> [Id] -> RType -> (RProgram, BaseType Formula)
generateFromTopLevelType name args t = do
  case t of
    ScalarT t fml -> (foldPApp (Program (PSymbol name) (ScalarT BoolT (BoolLit True))) args, t)
    FunctionT x tArg tRes -> do
      let (res, returnType) = generateFromTopLevelType name (x:args) tRes
      let generator = Program (PLet x (generateFromType tArg) res) (ScalarT BoolT (BoolLit True))
      (generator, returnType)

foldPApp :: RProgram -> [Id] -> RProgram
foldPApp = foldr
      (\ x p
         -> Program
              (PApp p (Program (PSymbol x) (ScalarT BoolT (BoolLit True))))
              (ScalarT BoolT (BoolLit True)))

generateFromType :: RType -> RProgram
generateFromType t = do
  case t of
    ScalarT t fml -> Program (PApp gen (Program PHole (baseTypeToTypeSkeleton t))) (ScalarT BoolT (BoolLit True))
    FunctionT x tArg tRes -> Program (PLet x (generateFromType tArg) (generateFromType tRes)) (ScalarT BoolT (BoolLit True))

baseTypeToTypeSkeleton :: BaseType Formula -> RType
baseTypeToTypeSkeleton t = case t of
  BoolT -> ScalarT BoolT (BoolLit True)
  IntT -> ScalarT IntT (BoolLit True)
  DatatypeT name tArgs pArgs -> todo
  TypeVarT {} -> todo

-- generateFromBaseType :: BaseType Formula -> RProgram
-- generateFromBaseType t = case t of
--   BoolT -> Program (PSymbol "randBool") (ScalarT BoolT (BoolLit True))
--   IntT -> Program (PSymbol "randInt") (ScalarT IntT (BoolLit True))
--   DatatypeT name tArgs pArgs -> todo
--   TypeVarT {} -> todo


-- generateFromFormula :: Formula -> RProgram
-- generateFromFormula fml = do
--     case fml of
--       BoolLit b -> randPure fml
--       IntLit i -> randPure fml
--       SetLit s elems -> randPure fml
--       Var s name -> todo
--       Unknown s name -> todo
--       Unary op e -> todo
--       Binary op e1 e2 -> generateFromBinOp op e1 e2
--       Ite e0 e1 e2 -> todo
--       Pred b name args -> todo
--       Cons b name args -> todo
--       All x e -> todo

-- generateFromBinOp :: BinOp -> Formula -> Formula -> RProgram
-- generateFromBinOp op e1 e2 = do
--   case op of
--     Times -> todo
--     Plus -> Program (PApp (Program (PSymbol "+") (ScalarT IntT (BoolLit True))) (generateFromFormula e1)) (ScalarT IntT (BoolLit True))
--     Minus -> todo
--     Eq -> do
--       case e1 of
--         Var _ valueVarName -> generateFromFormula e2
--         _ -> todo
--     Neq -> todo
--     Lt -> todo
--     Le -> todo
--     Gt -> todo
--     Ge -> todo
--     And -> todo
--     Or -> todo
--     Implies -> todo
--     Iff -> todo
--     Union -> todo
--     Intersect -> todo
--     Diff -> todo
--     Member -> todo
--     Subset -> todo


todo = error "todo"
