{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void, forM_)

import Worker
import Sequence.Alignment
import Sequence.Alignment.Matrix

import GovnoCode

trastuzumab = "EVQLVESGGGLVQPGGSLRLSCAASGFNIKDTYIHWVRQAPGKGLEWVARIYPTNGYTRYADSVKGRFTISADTSKNTAYLQMNSLRAEDTAVYYCSRWGGDGFYAMDYWGQGTLVTVSS"

pertuzumab = "EVQLVESGGGLVQPGGSLRLSCAASGFTFTDYTMDWVRQAPGKGLEWVADVNPNSGGSIYNQRFKGRFTLSVDRSKNTLYLQMNSLRAEDTAVYYCARNLGPSFYFDYWGQGTLVTVSS"

simpleScoring :: Substitution Char
simpleScoring c d | c == d = 4
                  | c /= d = -4

main :: IO ()
--main = runAlignerWorker def
main = do let sg = mkSemiglobal gcode (-5)
          forM_ [1..1000000] $ \_ -> do
            let (score, (f, s)) = alignment sg trastuzumab pertuzumab
            print score
            print f
            print s
