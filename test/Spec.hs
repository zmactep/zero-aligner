{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import Sequence.Alignment

main :: IO ()
main = hspec $
         describe "Edit distance" $ do
           it "align empty strings" $ do
             let (s', t') = alignment mkEditDistance "" ""
             s' `shouldBe` ""
             t' `shouldBe` ""
           it "align equal strings" $ do
             let (s', t') = alignment mkEditDistance "AAA" "AAA"
             s' `shouldBe` "AAA"
             t' `shouldBe` "AAA"
