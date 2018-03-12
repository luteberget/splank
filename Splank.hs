module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List ((\\), nub)
import Control.Monad (forM, forM_, filterM, join)

import SAT hiding (solve, bool)
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
  { initialState :: Set ((Bool, Prop Const))
  , goal :: [(Bool, Prop Const)]
  , actions :: [Action]
  } deriving (Show, Ord, Eq)

data Action
  = Action
  { actionName :: String
  , preConds   :: [(Bool, Prop Term)]
  , postConds  :: [(Bool, Prop Term)]
  }  deriving (Show, Ord, Eq)

data Prop a
  = Prop
  { predicate :: PredName
  , arguments :: [a]
  } deriving (Show, Ord, Eq)

data Term
  = ConstTerm Const
  | VarTerm Var
  deriving (Show, Ord, Eq)

vars :: Prop Term -> [Var]
vars (Prop _ args) = nub (join (fmap argVars args))
  where
    argVars (VarTerm v) = [v]
    argVars (ConstTerm _) = []

data Settings
  = Settings
  { settingsMaxSteps :: Maybe Int
  } deriving (Show)

data AbstractBool = ATrue | AFalse | ABoth Lit

type PropSet = Set (Prop Const)
type AbstractProps = Map (Prop Const) AbstractBool

abool :: Bool -> AbstractBool
abool True = ATrue
abool False = AFalse

addA :: Solver -> AbstractBool -> AbstractBool -> IO AbstractBool
addA s a b
  | a /= b = ABoth =<< newLit s
  | otherwise = return a

--type Table = (Prop Term,[Prop Const])

data FactStatus = Known | Possible Lit

hasSign :: AbstractBool -> Bool -> (Bool, FactStatus)
hasSign ATrue True = (True, Known)
hasSign ATrue False = (False, Known)
hasSign AFalse True = (False, Known)
hasSign AFalse False = (True, Known)
hasSign (ABoth v) True = (True, Possible v)
hasSign (ABoth v) False = (True, Possible (neg v))

propagate :: AbstractProps -> [Action] -> (AbstractProps, Set PlanItem)
propagate ps as = Set.unions [ undefined ] -- step a | a <- as ]
  where
    freeTerms :: [Prop Term]
    freeTerms = nub (join [ unbound a | a <- as ])

    -- look at preconditions and find possible ground actions
    groundActions :: Action -> [PlanItem]
    groundActions a = [ (n, v) 
      where
        varValues = foldl1 Set.intersection (fmap (varValues vs) (preConds a))

    varValues :: [Var] -> (Bool, Prop Term) -> Set [Const]
    varValues vs (sign,prop@(Proposition name args)) 

    match :: [Term] -> [Const] -> Bool
    match ts cs
     | len ts /= len cs = False
     | otherwise = all m (zip ts cs)
    where
      m :: Term -> Const -> Bool
      m (VarTerm _) _ = True
      m (ConstTerm c1) c2 = c1 == c2

    -- groundPreCond :: Set (Prop Term) -> 

    --step :: Action -> PropSet
    --step a = satisfiable (preConds a)

-- Find postcond variables which are unbound by preconds
unbound :: Action -> [Prop Term]
unbound a = filter (containsUnboundVar) (fmap snd (postConds a))
  where
    containsUnboundVar :: Prop Term -> Bool
    containsUnboundVar term = not (null ((vars term) \\ (boundVars)))
    boundVars :: [Var]
    boundVars = nub (join [vars t | t <- fmap snd (preConds a) ])

parseStdin :: IO Problem
parseStdin = undefined

type State      = Map (Prop Const) Lit
type Transition = Map PlanItem Lit

-- Determine the following from looking at actions:
--  a) possible assignments to free variables
--  b) possible grounded actions
newState :: Solver -> Problem -> State -> AbstractProps -> IO (State, Transition, AbstractProps)
newState s problem state propSet = do
  -- let newPropSet = propagate propSet (actions problem)
  --x <- newIORef :: Map (Prop Const) (Set PlanItem)
  -- transition <- forM (actions problem) $ \action -> do
  --   let params = undefined -- allPaarms
  --   forM params $ \p -> do
  --     undefined
  undefined

stVal :: Prop Const -> Map (Prop Const) Lit -> Lit
stVal = Map.findWithDefault false

signed :: Bool -> Lit -> Lit
signed True = id
signed False = neg

solve :: Problem -> Settings -> IO (Maybe Plan)
solve problem settings = withNewSolver $ \s -> do
  let maxSteps = fromMaybe 100 (settingsMaxSteps settings)
  let trySolve state transitions propSet n = do
        x <- SAT.solve s [ signed s (state Map.! p) | (s,p) <- goal problem ]
        if x then do
          putStrLn "SUCCESS."
          plan <- forM transitions $ \m -> do
            xs <- filterM (\(x,l) -> modelValue s l) (Map.toList m)
            return (fst (head xs))
          return (Just plan)
        else if n < maxSteps then do
          putStrLn $ "STEP" ++ (show (n+1)) ++ "."
          (nextState, nextTransition, nextPropSet) <- newState s problem state propSet
          trySolve nextState (transitions ++ [nextTransition]) nextPropSet (n+1)
        else do
          putStrLn $ "FAILED after " ++ (show n) ++ " steps."
          return Nothing

  let mkMap bool = Map.fromList [ (prop, bool sign) 
                                | (sign,prop) <- Set.toList (initialState problem) ]
  trySolve (mkMap SAT.bool) [] (mkMap abool) 0

main = do
  putStrLn "hello"
