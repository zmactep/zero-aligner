module Sequence.Alignment.Type where

import           Data.Array (Array (..))
import Data.ByteString.Char8 (ByteString)

-- |Represents alignment matrix
type Matrix = Array (Int, Int) Int

-- |Substitution function for 'a' values comparison
type Substitution a = a -> a -> Int

-- |Gap type
type Gap = Int

-- |Condition, that gets matrix, 'i' and 'j'
type Condition = Matrix -> ByteString -> ByteString -> Int -> Int -> Bool

-- |A set of traceback conditions
data Conditions = Conditions { isStop  :: Condition -- ^Should we stop?
                             , isDiag  :: Condition -- ^Should we go daigonally?
                             , isVert  :: Condition -- ^Should we go vertically?
                             , isHoriz :: Condition -- ^Should we go horizontally?
                             }

-- |Traceback starting point selector function, bool indicate if we have to add gaps to sequence
type StartSelector = Matrix -> (Bool, (Int, Int))

-- |Introduces all useful methods to create an alignment
class Alignment a where
  gap :: a -> Gap
  substitution :: a -> Substitution Char
  conditions :: a -> Conditions
  inits :: a -> Int -> Int
  selector :: a -> StartSelector
  additional :: a -> Int
