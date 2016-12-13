{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import Sequence.Alignment
import Sequence.Alignment.Matrix

tra = "EVQLVESGGGLVQPGGSLRLSCAASGFNIKDTYIHWVRQAPGKGLEWVARIYPTNGYTRYADSVKGRFTISADTSKNTAYLQMNSLRAEDTAVYYCSRWGGDGFYAMDYWGQGTLVTVSS"
per = "EVQLVESGGGLVQPGGSLRLSCAASGFTFTDYTMDWVRQAPGKGLEWVADVNPNSGGSIYNQRFKGRFTLSVDRSKNTLYLQMNSLRAEDTAVYYCARNLGPSFYFDYWGQGTLVTVSS"

main :: IO ()
main = hspec $ do
         describe "Edit distance" $ do
           it "align empty strings" $ do
             let (_, (s', t')) = alignment mkEditDistance "" ""
             s' `shouldBe` ""
             t' `shouldBe` ""
           it "align equal strings" $ do
             let (_, (s', t')) = alignment mkEditDistance "AAA" "AAA"
             s' `shouldBe` "AAA"
             t' `shouldBe` "AAA"
         describe "Global alignment" $
           it "align antibodies" $ do
             let (score, (s', t')) = alignment (mkGlobal blosum62 (-5)) tra per
             s' `shouldBe` "EVQLVESGGGLVQPGGSLRLSCAASGFNIKDTY-IHWVRQAPGKGLEWVARIYPTNGYTRYADSVKGRFTISADTSKNTAYLQMNSLRAEDTAVYYCSRWGGDGFYAMDYWGQGTLVTVSS"
             t' `shouldBe` "EVQLVESGGGLVQPGGSLRLSCAASGFTFTD-YTMDWVRQAPGKGLEWVADVNPNSGGSIYNQRFKGRFTLSVDRSKNTLYLQMNSLRAEDTAVYYCARNLGPSFY-FDYWGQGTLVTVSS"
             score `shouldBe` 459
