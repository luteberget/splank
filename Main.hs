module Main where

import Problem
import Parser
import SAT
import SAT.Bool
import SAT.Equal
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (join, foldM, forM, filterM, forM_)
import Data.Maybe (catMaybes, isJust, fromJust)

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
  , gaPostCond :: Map GroundPred Bool
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

maybeSingle :: (Eq a) => [a] -> Maybe a
maybeSingle [] = Nothing
maybeSingle (x:xs) = allEq x xs
  where
    allEq e [] = Just e
    allEq e (x:xs) = if e == x then allEq e xs else Nothing

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
                     from to

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
                 let newState = foldl i state (Map.toList (gaPostCond action))
                       where
                         i state (p,b) = Map.insert p (bool b) state
                 return $ Just (Map.fromList [(gaPredicate action, true)], newState)
    actions -> do
      --putStrLn " *> Creating step with the following actions:"
      -- sequence_ [ putStrLn $ "   *> " ++ (show a) | a <- actions ]
      --
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
      let affectedPreds = Map.fromListWith (++) [ (p,[(gaPredicate a, b)]) | a <- actions, (p, b) <- Map.toList (gaPostCond a) ] 

      stateUpdates <- forM (Map.toList affectedPreds) $ \(p,actionValues) -> do
        let singleValue = maybeSingle (fmap snd actionValues)
        if length actionValues == length actions && isJust singleValue then do
          return (p, bool $ fromJust singleValue)
        else do
          lit <- newLit s
          forM_ actionValues $ \(pred, val) -> do
            addClause s [neg (actionMap Map.! pred), if val then lit else neg lit]
          if length actionValues < length actions then do
            equalOr s (fmap (\x -> actionMap Map.! (fst x)) actionValues)
              (state Map.! p) lit
          else return ()
          return (p, lit)
      let newState = Map.union (Map.fromList stateUpdates) state
      return $ Just (actionMap, newState)

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
          putStrLn "*** Adding state"
          next <- nextState s problem state
          case next of
            Nothing -> do putStrLn "*** No further actions possible"
                          return Nothing
            Just (actions, state) -> do
               solve s (n+1) state (plan ++ [actions])
        else do 
          putStrLn "*** Maximum number of actions reached"
          return Nothing
