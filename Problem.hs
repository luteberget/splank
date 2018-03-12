module Problem where

data Problem
  = Problem
  { domains :: [Domain]
  , predicates :: [Predicate]
  , initialState :: [Prop String]
  , goalState :: [Prop String]
  , actions :: [Action]
  } deriving (Eq, Ord, Show)

data Domain
  = Domain
  { domainName :: String
  , domainValues :: [String]
  } deriving (Eq, Ord, Show)

data Predicate
  = Predicate
  { predicateName :: String
  , predicateArgsDomains :: [String]
  } deriving (Eq, Ord, Show)

data Prop a
  = Prop
  { polarity :: Bool
  , propPredicate :: String
  , propArgs :: [a]
  } deriving (Eq, Ord, Show)

data Term
  = ConstTerm String
  | VarTerm String
  deriving (Eq, Ord, Show)

data Action
  = Action
  { actionName :: String
  , actionVars :: [String]
  , actionFrom :: [Prop Term]
  , actionTo :: [Prop Term]
  } deriving (Eq, Ord, Show)

