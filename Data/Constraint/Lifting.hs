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

--instance (∀ a . c a => d (f a)) => Lifting c d f where lift = Sub Dict

instance Lifting c Unconstrained1 f where lift = Sub Dict

instance Lifting Semigroup Monoid Maybe where lift = Sub Dict

instance Lifting Semigroup Semigroup ((->) a) where lift = Sub Dict
instance Lifting Monoid Monoid ((->) a) where lift = Sub Dict

instance Semigroup a => Lifting Semigroup Semigroup ((,) a) where lift = Sub Dict
instance Monoid a => Lifting Monoid Monoid ((,) a) where lift = Sub Dict

instance Lifting (Functor s (->)) (Functor (NT s) (NT (->))) Compose where lift = Sub Dict

instance Lifting Category Groupoid Iso where lift = Sub Dict
