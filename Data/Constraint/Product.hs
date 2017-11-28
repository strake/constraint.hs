{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Constraint.Product where

class    (a, b) => Product a b
instance (a, b) => Product a b
