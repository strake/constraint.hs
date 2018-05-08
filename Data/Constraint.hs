module Data.Constraint (Constraint, Dict (..), withDict, (:-) (..), (\\), unmapDict) where

import Prelude hiding (Functor, (.), id)

import Control.Categorical.Functor
import Control.Category
import Data.Kind (Constraint)
import Data.Semigroup

data Dict a where Dict :: a => Dict a

deriving instance Eq (Dict a)
deriving instance Ord (Dict a)
deriving instance a => Read (Dict a)
deriving instance Show (Dict a)
deriving instance a => Bounded (Dict a)

instance a => Enum (Dict a) where
    toEnum _ = Dict
    fromEnum Dict = 0

instance Semigroup (Dict a) where Dict <> Dict = Dict
instance a => Monoid (Dict a) where
    mempty = Dict
    mappend = (<>)

withDict :: Dict a -> (a => b) -> b
withDict Dict b = b

newtype a :- b = Sub (a => Dict b)

instance Eq (a :- b) where Sub _ == Sub _ = True
instance Ord (a :- b) where Sub _ `compare` Sub _ = EQ
instance Show (a :- b) where showsPrec p (Sub _) = showParen (p > 10) $ showString "Sub Dict"
instance Semigroup (a :- b) where (<>) = pure

instance Category (:-) where
    id = Sub Dict
    Sub c . Sub b = Sub (withDict b c)

instance Functor (:-) (->) Dict where map (Sub a) Dict = withDict a Dict

(\\) :: (b => c) -> (a :- b) -> (a => c)
f \\ Sub Dict = f

unmapDict :: (Dict a -> Dict b) -> a :- b
unmapDict f = Sub (f Dict)
