module Main where

import Problem
import Parser
import SAT
import Data.Map (Map)
import qualified Data.Map as Map

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
                    , let argProd = cart [getDomain d 
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

groundActions :: Problem -> State -> [GroundAction]
groundActions problem state = join $ fmap tryAction (actions problem)
  where
    tryAction :: Action -> [GroundAction]
    tryAction a = catMaybes (fmap ground assignments)
      where
        assignments :: [Map String String]
        assignments = 

        applyL :: Map String String -> [String] -> [String]
        applyL m vs = fmap (\x -> m Map.! x) vs

        applyT :: Map String String -> Prop Term -> (GroundPred, Bool)
        applyT m (Prop b pred args) = ((pred, applyL m args), b)

        ground :: Map String String -> Maybe GroundAction
        ground m = do
          from <- mapM inState (fmap (applyT m) (actionFrom a))
          let to = fmap (applyT m) (actionTo a)
          return GroundAction (actionName a, applyL m (actionVars a)) from to

        inState :: (GroundPred, Bool) -> Maybe Lit
        inState (p,b)
          | l == bool b = Just true
          | neg l == bool b = Nothing
          | otherwise = if b then Just l else Just (neg l)

-- nextState :: Solver -> Problem -> State -> IO (Maybe ([(GroundPred,Lit)], State))
-- nextState s problem state = do
--   let actions = groundActions problem state
--   case actions of
--     [] -> return Nothing
--     [action] -> do
--              -- Precondition must hold
--              sequence_ [ addClause s [cond] | cond <- gaPreCond action ]
-- 
--              -- State must be set to
--              --foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
--              --
--              let newState = foldl insert state (gaPostCond a)
--                where
--                  insert :: Map GroundPred Lit -> (GroundPred, Bool) -> Map GroundPred Lit
--                  insert state (p,b) = Map.insert (p,bool b) state
-- 
--              return Just ([(action, true)], newState)
--     actions -> do
--       -- Create literals for actions
--       actionList <- sequence [ newLit s | _ <- actions ]
--       -- Precondition constraints
--       forM (zip actionList actions) $ \(doAction,action) -> do
--         sequence_ [ addClause s [neg doAction, cond] | cond <- gaPreCond action]
--       -- Action constraint
--       exactlyOne s actionList
--       -- New state and postconditon constraints
--       newStates :: Map GroundPred ([(Lit, Bool)], Maybe Bool)
--       let newStates = Map.fromListWith insert
--                         [ (p,([p,b],Just b)) | a <- action, (p,b) <- gaPostCond a ]
--         where 
--           insert :: ([(Lit,Bool)], Maybe Bool) 
--                  -> ([(Lit,Bool)], Maybe Bool)
--                  -> ([(Lit,Bool)], Maybe Bool)
--           insert (xs,(Just x)) (ys,(Just y)) = if x == y then ((xs++ys),Just x) else ((xs++ys),Nothing)
--           insert (xs,_) (ys,_) = ((xs++ys),Nothing)
-- 
--       newState <- foldM insert state newStates
--         where
--           insert :: Map GroundPred Lit -> (GroundPred, ([(Lit, Bool)], Maybe Bool)) -> IO (Map GroundPred Lit)
--           insert state (p, (_,Just x)) = return (Map.insert (p,x) state)
--           insert state (p, (conds, Nothing)) = do
--             lit <- newLit s
--             forM conds $ \(a,cond) -> do 
--               addClause s [neg a, if cond then lit else (neg lit)]
--             return (Map.insert (p,lit) state)
-- 
--       -- Return action list and state
--       let actionList = zip (fmap gaPredicate actions) actionList
--       return (actionList, newState)
--       
-- 
-- condition :: [Pred] -> State -> [Lit]
-- condition = map (lookup state) statePred
-- 
-- type Plan = [ActPred]
-- 
-- plan :: Int -> Problem -> IO (Maybe Plan)
-- plan maxN problem = withNewSolver $ \s -> do
--   let state = groundInitialState problem
--   solve s 0 state []
-- 
--   where
--     solve :: Solver -> Int -> State -> [ActPred] -> IO (Maybe Plan)
--     solve s n state plan = do
--       b <- SAT.solve s (condition (goal problem) state)
--       if b then do
--         -- solution
--         --
--         return $ Just (modelValue plan)
--       else do
--         if n < maxN then do 
--           -- add new state
--           next <- nextState s problem state
--           case next of
--             Nothing -> do -- fail
--                        return Nothing
--             Just (state, actions) -> solve s (n+1) newState (plan ++ [actions])
--         else do 
--           -- fail
--           return Nothing
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
