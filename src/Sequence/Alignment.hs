{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Sequence.Alignment
    ( Substitution, Gap, Alignment (..)
    , alignment
    , mkGlobal, mkLocal, mkSemiglobal, mkEditDistance
    ) where

import           Data.Array                   (array, range, (!))
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as B

import           Sequence.Alignment.Instances
import           Sequence.Alignment.Type

alignment :: Alignment a => a -> ByteString -> ByteString -> (Int, (ByteString, ByteString))
alignment sa s t = (score, trace sm sn si ti)
  where (m, n) = (B.length s, B.length t)
        bounds = ((0, 0), (m, n))

        (needGaps, (sm, sn)) = selector sa matrix
        (si, ti) = if needGaps then tails else ([], [])

        score = matrix ! (sm, sn)
        gapSymbol = '-'

        tails :: (String, String)
        tails | m == sm = (gaps (n - sn), B.unpack (B.drop sn t))
              | n == sn = (B.unpack (B.drop sm s), gaps (m - sm))

        gaps :: Int -> String
        gaps size = replicate size gapSymbol

        distance :: Int -> Int -> Int
        distance i 0 = inits sa i
        distance 0 j = inits sa j
        distance i j = maximum [ matrix ! (i - 1, j - 1) + sub i j
                               , matrix ! (i - 1, j) + g
                               , matrix ! (i, j - 1) + g
                               , additional sa
                               ]
          where !g = gap sa

        sub :: Substitution Int
        sub = subIJ sa s t

        matrix :: Matrix
        !matrix = array bounds [(ij, uncurry distance ij) | ij <- range bounds]

        trace :: Int -> Int -> String -> String -> (ByteString, ByteString)
        trace i j s' t' | isStop  matrix s t i j = (B.pack s', B.pack t')
                        | isVert  matrix s t i j = trace (i - 1) j (addToS i) (gapSymbol:t')
                        | isHoriz matrix s t i j = trace i (j - 1) (gapSymbol:s') (addToT j)
                        | isDiag  matrix s t i j = trace (i - 1) (j - 1) (addToS i) (addToT j)
          where addToS i = (s `B.index` (i - 1)):s'
                addToT j = (t `B.index` (j - 1)):t'
                Conditions {..} = conditions sa
