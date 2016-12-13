module Sequence.Alignment.Matrix.Scoring where

import           Control.Applicative       (liftA2)
import           Data.Map.Strict           (Map (..), fromList, (!))
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Sequence.Alignment.Type

class ScoringMatrix a where
  scoring :: a -> Substitution Char

mkSubstitution :: String -> Substitution Char
mkSubstitution t c d = m ! (c, d)
  where m = loadMatrix t

loadMatrix :: String -> Map (Char, Char) Int
loadMatrix txt = fromList $ concat table
  where f = liftA2 (||) T.null ((/= '#') . T.head)
        lines = filter f (T.strip <$> T.lines (T.pack txt))
        letters = (map T.head . T.words . head) lines
        table = (map (lineMap . T.words) . tail) lines
        lineMap (x:xs) = let f (n, c) = ((T.head x, c), n)
                         in f <$> (read . T.unpack <$> xs) `zip` letters
