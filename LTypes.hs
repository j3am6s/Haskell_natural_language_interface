module LTypes where

type Atm = String
data LTp = Atm Atm | DivL LTp LTp | DivR LTp LTp | Neg LTp
  deriving (Show,Eq)

data Lexicon a = Lexicon { items :: [(String,LTp,a)] , stp :: LTp }
