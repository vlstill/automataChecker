{-# LANGUAGE NamedFieldPuns, RecordWildCards, ScopedTypeVariables, ExplicitForAll #-}

module Compare where

import Data
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Applicative
import Control.Arrow

data GrState a = Nonterm a
               | Fin
               deriving ( Eq, Ord, Show, Read )

rg2nfa :: forall a. Ord a => RG a -> FA (GrState a)
rg2nfa (RG {..}) = FA { faStates = Fin `Set.insert` (Set.map Nonterm rgNonterminals)
                      , faInitial = Nonterm rgInitial
                      , faAccepting
                      , faDelta
                      }
  where
    faDelta = Map.fromListWith Set.union . concatMap convertRule $ Map.toList rgRules

    convertRule :: (a, Set (RuleRHS a)) -> [((GrState a, Char), Set (GrState a))]
    convertRule (nt, target) = concatMap (convertOne nt) $ Set.toList target

    convertOne :: a -> RuleRHS a -> [((GrState a, Char), Set (GrState a))]
    convertOne nt Eps          = [] -- ignored here, see faAccepting
    convertOne nt (Final c)    = [((Nonterm nt, c), Set.singleton Fin)]
    convertOne nt (Follow c n) = [((Nonterm nt, c), Set.singleton (Nonterm n))]

    faAccepting 
      | Just rhs <- Map.lookup rgInitial rgRules, Eps `Set.member` rhs = Set.fromList [ Fin, Nonterm rgInitial ]
      | otherwise = Set.singleton Fin
