module Main where

import Problem
import Parser
import SAT
import SAT.Bool
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (join, foldM, forM, filterM)
import Data.Maybe (catMaybes)
import Debug.Trace

exactlyOne :: Solver -> [Lit] -> IO ()
exactlyOne s xs = do
  atMostOne s xs
  addClause s xs

type GroundPred = (String,[String])
type State = Map GroundPred Lit
data GroundAction
  = GroundAction
  { gaPredicate :: GroundPred
  , gaPreCond :: [Lit]
  , gaPostCond :: [(GroundPred, Bool)]
  } deriving (Eq, Ord, Show)

-- TODO domains as :: Map
--

groundInitialState :: Problem -> State
groundInitialState problem = Map.union initialTrue allFalse
  where
    initialValues :: [GroundPred]
    initialValues = [ (propPredicate prop, propArgs prop) 
                    | prop <- initialState problem]
    initialTrue = Map.fromList [(p,true ) | p <- initialValues ]

    groundProblem :: [GroundPred]
    groundProblem = [ (predicateName predicate, args) 
                    | predicate <- predicates problem
                    , let argProd = cart [getDomain problem d 
                                         | d <- predicateArgsDomains predicate]
                    , args <- argProd]

    allFalse = Map.fromList    [(p,false) | p <- groundProblem ]

getDomain :: Problem -> String -> [String]
getDomain problem d = [ v | dom <- domains problem
                          , domainName dom == d
                          , v <- domainValues dom]

-- getPredicateDomains :: Problem -> String -> [[String]]
-- getPredicateDomains problem n = [ [ getDomain d | d <- predicateArgsDomains pred ] 
--                                 | pred <- predicates problem
--                                 , n == predicateName pred ]

cart :: [[a]] -> [[a]]
cart [] = [[]]
cart (x:xs) = [ e:rest | e <- x, rest <- cart xs ]

type Variable = String
type PredName = String

single :: (Eq a) => [a] -> a
single [] = error "Single called with empty list."
single (x:xs) = allEq x xs
  where
    allEq e [] = e
    allEq e (x:xs) = if e == x then allEq e xs else (error "Single failed")

groundActions :: Problem -> State -> [GroundAction]
groundActions problem state = join $ fmap tryAction (actions problem)
  where
    tryAction :: Action -> [GroundAction]
    tryAction a = catMaybes (fmap ground assignments)
      where
        assignments = [ Map.fromList a
                      | a <- cart [ [ (var,val) 
                                    | val <- getDomain problem (varDomain var) ]
                                  | var <- actionVars a ] ]

        varDomain :: Variable -> String
        varDomain var = single x -- (trace ((show a) ++ "--" ++ (show var) ++ "--" ++ (show x)) x)
          where x = [ (predicateArgsDomains pred) !! idx
                         | cond <- (actionFrom a) ++ (actionTo a)
                         , (idx,val) <- zip [0..] (propArgs cond)
                         , (VarTerm var) == val
                         , pred <- predicates problem
                         , predicateName pred == propPredicate cond ]

        applyL :: Map String String -> [String] -> [String]
        applyL m vs = fmap (\x -> m Map.! x) vs

        applyVT :: Map String String -> [Term] -> [String]
        applyVT m vs = fmap (\x -> case x of
                                VarTerm v -> m Map.! v
                                ConstTerm c -> c) vs

        applyT :: Map String String -> Prop Term -> (GroundPred, Bool)
        applyT m (Prop b pred args) = ((pred, applyVT m args), b)

        ground :: Map String String -> Maybe GroundAction
        ground m = do
          from <- mapM inState (fmap (applyT m) (actionFrom a))
          to <- foldM consistent Map.empty (fmap (applyT m) (actionTo a))
          return $ GroundAction (actionName a, applyL m (actionVars a)) 
                     from (Map.toList to)

        consistent :: Map GroundPred Bool -> (GroundPred, Bool) 
                   -> Maybe (Map GroundPred Bool)
        consistent m (p,b1) = case Map.lookup p m of
                               Just b2 -> if b1 == b2 then Just m else Nothing
                               Nothing -> Just (Map.insert p b1 m)

        inState :: (GroundPred, Bool) -> Maybe Lit
        inState (p,b)
          | l == bool b = Just true
          | neg l == bool b = Nothing
          | otherwise = if b then Just l else Just (neg l)
          where l = state Map.! p

nextState :: Solver -> Problem -> State -> IO (Maybe (Map GroundPred Lit, State))
nextState s problem state = do
  case groundActions problem state of
    [] -> return Nothing
    [action] -> do
                 sequence_ [ addClause s [cond] | cond <- gaPreCond action ]
                 let newState = foldl i state (gaPostCond action)
                       where
                         i state (p,b) = Map.insert p (bool b) state
                 return $ Just (Map.fromList [(gaPredicate action, true)], newState)
    actions -> do
      putStrLn " *> Creating step with the following actions:"
      sequence_ [ putStrLn $ "   *> " ++ (show a) | a <- actions ]
      -- Create literals for actions
      actionList <- sequence [ newLit s | _ <- actions ]
      let actionMap = Map.fromList (zip (fmap gaPredicate actions) actionList)
      -- Precondition constraints
      sequence_ [ 
        sequence_ [ addClause s [neg actLit, cond] | cond <- gaPreCond action ]
        | (actLit, action) <- zip actionList actions ]
      -- Action constraint
      exactlyOne s actionList
      -- New state and postconditon constraints
      --
      --
      let newStates :: Map GroundPred ([(GroundPred, Bool)], Maybe Bool)
          newStates = Map.fromListWith insert
                        [ (p,([(p,b)],Just b)) | a <- actions, (p,b) <- gaPostCond a ]
            where 
              insert :: ([(GroundPred,Bool)], Maybe Bool) 
                     -> ([(GroundPred,Bool)], Maybe Bool)
                     -> ([(GroundPred,Bool)], Maybe Bool)
              insert (xs,(Just x)) (ys,(Just y)) = if x == y then ((xs++ys),Just x) else ((xs++ys),Nothing)
              insert (xs,_) (ys,_) = ((xs++ys),Nothing)

      let insertx :: Map GroundPred Lit 
                      -> (GroundPred, ([(GroundPred, Bool)], Maybe Bool)) 
                      -> IO (Map GroundPred Lit)
          insertx state (p, (_,Just x)) = return (Map.insert p (bool x) state)
          insertx state (p, (conds, Nothing)) = do
              lit <- newLit s
              forM conds $ \(pred,cond) -> do 
                addClause s [neg (actionMap Map.! pred), 
                             if cond then lit else (neg lit)]
              return (Map.insert p lit state)
      newState <- foldM insertx state (Map.toList newStates)
      return $ Just (actionMap, newState)
--       
-- 
-- condition :: [Pred] -> State -> [Lit]
-- condition = map (lookup state) statePred
-- 

condition :: State -> Prop String -> Lit
condition state prop = if polarity prop then x else neg x
  where x = state Map.! (propPredicate prop, propArgs prop)

type Plan = [GroundPred]

evalPlan :: Solver -> Map GroundPred Lit -> IO GroundPred
evalPlan s m = do
  positives <- filterM (\(_,l) -> modelValue s l) (Map.toList m) 
  let (pred,_) = head positives
  return pred

plan :: Int -> Problem -> IO (Maybe Plan)
plan maxN problem = withNewSolver $ \s -> do
  let state = groundInitialState problem
  solve s 0 state []

  where
    solve :: Solver -> Int -> State -> [Map GroundPred Lit] -> IO (Maybe Plan)
    solve s n state plan = do
      b <- SAT.solve s (fmap (condition state) (goalState problem))
      if b then do
        putStrLn "*** Solution"
        result <- mapM (evalPlan s) plan
        return (Just result)
      else do
        if n < maxN then do 
          -- add new state
          putStrLn "*** Adding state"
          next <- nextState s problem state
          case next of
            Nothing -> do -- fail
                       putStrLn "*** No further actions possible"
                       return Nothing
            Just (actions, state) -> solve s (n+1) state (plan ++ [actions])
        else do 
          putStrLn "*** Maximum number of actions reached"
          -- fail
          return Nothing
-- 
-- -- allSame :: [Bool] -> Maybe Bool
-- -- allSame [] = error "empty list passed to allSame"
-- -- allSame [x] = Just x
-- -- allSame (x:xs) = allEqual x xs
-- --   where
-- --     allEqual :: Bool -> [Bool] -> Maybe Bool
-- --     allEqual a [] = Just a
-- --     allEqual a (x:xs) = if a == x then allEqual a xs else Nothing
-- -- 
-- -- isConstBool x = x == true || x == false
