module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad (forM, forM_, filterM)

import SAT hiding (solve)
import qualified SAT
import SAT.Bool

type Const = String
type Var = String
type PredName = String
type ActionName = String
type PlanItem = (ActionName, [Const])
type Plan = [PlanItem]

data Problem 
  = Problem 
  { initialState :: Set (Prop Const)
  , goal :: [Prop Const]
  , actions :: [Action]
  } deriving (Show, Ord, Eq)

data Action
  = Action
  { actionName :: String
  , preCond    :: [Prop Term]
  , postCond   :: [Prop Term]
  }  deriving (Show, Ord, Eq)

data Prop a
  = Prop
  { polarity  :: Bool
  , predicate :: PredName
  , arguments :: [a]
  } deriving (Show, Ord, Eq)

data Term
  = ConstTerm Const
  | VarTerm Var
  deriving (Show, Ord, Eq)

data Settings
  = Settings
  { settingsMaxSteps :: Maybe Int
  } deriving (Show)

type GroundInstance = (Set (Prop Const), Set PredName)
type GrounderState = (GroundInstance, GroundInstance)

ground :: Problem -> Set (Prop Const)
ground = undefined

parseStdin :: IO Problem
parseStdin = undefined

type State      = Map (Prop Const) Lit
type Transition = Map PlanItem Lit

reprBool :: Bool -> Lit
reprBool True = true
reprBool False = false

newState :: Solver -> Problem -> State -> Set (Prop Const) -> IO (State, Transition)
newState s problem state props = do
  x <- newIORef :: Map (Prop Const) (Set PlanItem)
  transition <- forM (actions problem) $ \a -> do
    let params = allPaarms
    forM params $ \p -> do

solve :: Problem -> Settings -> IO (Maybe Plan)
solve problem settings = withNewSolver $ \s -> do
  let maxSteps = fromMaybe 100 (settingsMaxSteps settings)
  let propositions = ground problem
  let trySolve state transitions n = do
        x <- SAT.solve s [ f l | p <- goal problem 
                               , let f = if polarity p then id else neg
                               , let l = state Map.! p]
        if x then do
          putStrLn "SUCCESS."
          plan <- forM transitions $ \m -> do
            xs <- filterM (\(x,l) -> modelValue s l) (Map.toList m)
            return (fst (head xs))
          return (Just plan)
        else if n < maxSteps then do
          putStrLn $ "STEP" ++ (show (n+1)) ++ "."
          (nextState, nextTransition) <- newState s problem state propositions
          trySolve nextState (transitions ++ [nextTransition]) (n+1)
        else do
          putStrLn $ "FAILED after " ++ (show n) ++ " steps."
          return Nothing

  let stateI = Map.fromList [(x,v) | x <- Set.toList propositions
       , let v = reprBool (Set.member x (initialState problem))]

  trySolve stateI [] 0

main = do
  putStrLn "hello"
