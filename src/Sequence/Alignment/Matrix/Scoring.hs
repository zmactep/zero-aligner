{-# LANGUAGE BangPatterns #-}

module Sequence.Alignment.Matrix.Scoring where

import           Control.Applicative       (liftA2)
import           Data.Map.Strict           (fromList, (!))
import Data.List (words)

import           Sequence.Alignment.Type

class ScoringMatrix a where
  scoring :: a -> Substitution Char

mkSubstitution :: String -> Substitution Char
mkSubstitution t c d = m ! (c, d)
  where !m = fromList $ loadMatrix t

loadMatrix :: String -> [((Char, Char), Int)]
loadMatrix txt = concat table
  where f = liftA2 (||) null ((/= '#') . head)
        strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
        !txtlns = filter f (strip <$> lines txt)
        !letters = (map head . words . head) txtlns
        !table = (map (lineMap . words) . tail) txtlns
        lineMap (x:xs) = let !hx = head x
                             f (n, c) = ((hx, c), n)
                         in f <$> (read <$> xs) `zip` letters
