{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.QuickCheck

import Data.Monoid ((<>))
import Data.ByteString.Char8 (ByteString)

import Sequence.Alignment
import Sequence.Alignment.Matrix

chains :: [ByteString]
chains = [ "EVQLVESGGGLVQPGGSLRLSCAASGFNIKDTYIHWVRQAPGKGLEWVA" <>
           "RIYPTNGYTRYADSVKGRFTISADTSKNTAYLQMNSLRAEDTAVYYCSRWGGDGFYAMDYWGQGTLVTVSS"
         , "EVQLVESGGGLVQPGGSLRLSCAASGFTFTDYTMDWVRQAPGKGLEWVA" <>
           "DVNPNSGGSIYNQRFKGRFTLSVDRSKNTLYLQMNSLRAEDTAVYYCARNLGPSFYFDYWGQGTLVTVSS"
         , "QVQLQQPGAELVKPGASVKMSCKASGYTFTSYNMHWVKQTPGRGLEWIG" <>
           "AIYPGNGDTSYNQKFKGKATLTADKSSSTAYMQLSSLTSEDSAVYYCARSTYYGGDWYFNVWGAGTTVTVSA"
         , "EVQLVESGGGLVQPGRSLRLSCAASGFTFDDYAMHWVRQAPGKGLEWVSAI" <>
           "TWNSGHIDYADSVEGRFTISRDNAKNSLYLQMNSLRAEDTAVYCAKVSYLSTASSLDYWGQGTLVTVSS"
         , "EVQLVESGGGLVQPGGSLRLSCAASGYTFTNYGMNWVRQAPGKGLEWVG" <>
           "WINTYTGEPTYAADFKRRFTFSLDTSKSTAYLQMNSLRAEDTAVYCAKYPHYYGSSHWYFDVWGQGTLVTVSS"
         , "QVQLVQSGVEVKKPGASVKVSCKASGYTFTNYYMYWVRQAPGQGLEWMG" <>
           "GINPSNGGTNFNEKFKNRVTLTTDSSTTTAYMELKSLQFDDTAVYYCARRDYRFDMGFDYWGQGTTVTVSS"
         , "EVQLVESGGGLVQPGGSLRLSCAASGFTFSNYWMNWVRQAPGKGLEWVA" <>
           "AINQDGSEKYYVGSVKGRFTISRDNAKNSLYLQMNSLRVEDTAVYYCVRDYYDILTDYYIHYWYFDLWGRGTLVTVSS"
         ]

combinations :: [Int]
combinations = [fst $ alignment glob x y | x <- chains, y <- chains]
  where glob = mkGlobal blosum62 (-5)

results :: [Int]
results = [ 645, 459, 345, 431, 401, 337, 405, 459, 634, 375, 437, 444
          , 378, 425, 345, 375, 651, 306, 368, 418, 316, 431, 437, 306
          , 628, 413, 312, 423, 401, 444, 368, 413, 669, 337, 436, 337
          , 378, 418, 312, 337, 647, 304, 405, 425, 316, 423, 436, 304, 688]

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
         describe "Global alignment" $ do
           it "align antibodies" $ do
             let (tra : per : _) = chains
             let (score, (s', t')) = alignment (mkGlobal blosum62 (-5)) tra per
             s' `shouldBe` "EVQLVESGGGLVQPGGSLRLSCAASGFNIKDTY-IHWVRQAPGKGLEWVARIYPTNGYTRYADSVKGRFTISADTSKNTAYLQMNSLRAEDTAVYYCSRWGGDGFYAMDYWGQGTLVTVSS"
             t' `shouldBe` "EVQLVESGGGLVQPGGSLRLSCAASGFTFTD-YTMDWVRQAPGKGLEWVADVNPNSGGSIYNQRFKGRFTLSVDRSKNTLYLQMNSLRAEDTAVYYCARNLGPSFY-FDYWGQGTLVTVSS"
             score `shouldBe` 459
           it "align fast" $
             combinations `shouldBe` results
