{-# LANGUAGE TemplateHaskell #-}

module Sequence.Alignment.Matrix.Template where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import Sequence.Alignment.Matrix.Scoring

matrix :: QuasiQuoter
matrix = QuasiQuoter { quotePat = undefined
                     , quoteType = undefined
                     , quoteExp = undefined
                     , quoteDec = matrixDec
                     }

matrixDec :: String -> Q [Dec]
matrixDec s = do let slines = lines s
                 let txt = (unlines . tail) slines
                 matType <- newName (head slines)
                 let matData = mkName (nameBase matType)
                 scoreImpl <- runQ [| mkSubstitution txt |]
                 let dataDecl = DataD [] matType [] Nothing [NormalC matData []] [ConT ''Show, ConT ''Eq]
                 let instDecl = InstanceD Nothing [] (AppT (ConT ''ScoringMatrix) (ConT matType))
                 let instBody = [FunD 'scoring [Clause [WildP] (NormalB scoreImpl) []]]
                 return [dataDecl, instDecl instBody]
