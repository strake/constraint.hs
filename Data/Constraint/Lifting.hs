{-# LANGUAGE QuantifiedConstraints #-}
module Data.Constraint.Lifting where

import Prelude hiding (Functor, (.), id)

import Control.Categorical.Functor
import Control.Category
import Control.Category.Groupoid
import Data.Constraint
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Morphism.Iso
import Unconstrained

class Lifting c d f where
    lift :: c a :- d (f a)

type Endolifting c = Lifting c c

instance Lifting c Unconstrained1 f where lift = Sub Dict

instance Lifting (Functor s (->)) (Functor (NT s) (NT (->))) Compose where lift = Sub Dict

instance Lifting Category Groupoid Iso where lift = Sub Dict

instance (∀ a . Eq a => c (f a)) => Lifting Eq c f where lift = Sub Dict
instance (∀ a . Ord a => c (f a)) => Lifting Ord c f where lift = Sub Dict
instance (∀ a . Bounded a => c (f a)) => Lifting Bounded c f where lift = Sub Dict
instance (∀ a . Semigroup a => c (f a)) => Lifting Semigroup c f where lift = Sub Dict
instance (∀ a . Monoid a => c (f a)) => Lifting Monoid c f where lift = Sub Dict
instance (∀ a . Read a => c (f a)) => Lifting Read c f where lift = Sub Dict
instance (∀ a . Show a => c (f a)) => Lifting Show c f where lift = Sub Dict
