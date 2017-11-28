{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Constraint.Compose (Compose, decompose) where

import Data.Constraint

class c (f a) => Compose (c :: β -> Constraint) (f :: α -> β) (a :: α)
instance c (f a) => Compose c f a

decompose :: Compose c f a :- c (f a)
decompose = Sub Dict
