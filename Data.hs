module Data where

import Data.Set ( Set )
import Data.Map

data Rule a = Eps
            | Final Char
            | Follow Char a
            deriving ( Eq, Ord, Show, Read )

data FA a = FA { faStates :: Set a
               , faDelta :: Map (a, Char) (Set a)
               , faAccepting :: Set a
               , faInitial :: a
               } deriving ( Eq, Show, Read )

data RG a = RG { rgNonterminals :: Set a
               , rgRules :: Map a (Rule a)
               , rgInitial :: a
               } deriving ( Eq, Show, Read )
