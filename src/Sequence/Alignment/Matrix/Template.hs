{-# LANGUAGE TemplateHaskell #-}

module Sequence.Alignment.Matrix.Template where

import           Data.Char                         (toLower)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Sequence.Alignment
import           Sequence.Alignment.Matrix.Scoring

matrix :: QuasiQuoter
matrix = QuasiQuoter { quotePat = undefined
                     , quoteType = undefined
                     , quoteExp = undefined
                     , quoteDec = matrixDec
                     }


matrixDec :: String -> Q [Dec]
matrixDec s = do let slines = lines s
                 let txt = (unlines . tail) slines
                 let name = head slines
                 (typeName, dataDecl) <- typeDec name
                 (funcName, funDecls) <- functionDec name txt
                 smDecl <- instDec typeName funcName
                 return $ dataDecl : smDecl : funDecls

instDec :: Name -> Name -> Q Dec
instDec typeN funN = return $ decl [body]
  where decl = InstanceD Nothing [] (AppT (ConT ''ScoringMatrix) (ConT typeN))
        body = FunD 'scoring [Clause [WildP] (NormalB (VarE funN)) []]

typeDec :: String -> Q (Name, Dec)
typeDec name = do typeN <- newName name
                  let dataN = mkName (nameBase typeN)
                  let dervs = [ConT ''Show, ConT ''Eq]
                  return (typeN, DataD [] typeN [] Nothing [NormalC dataN []] dervs)

functionDec :: String -> String -> Q (Name, [Dec])
functionDec name txt = do let subM = loadMatrix txt
                          funName <- newName (toLower <$> name)
                          let funSign = SigD funName (AppT (ConT ''Substitution) (ConT ''Char))
                          let clauses = mkClause <$> subM
                          let funDecl = FunD funName clauses
                          return (funName, [funSign, funDecl])

mkClause :: ((Char, Char), Int) -> Clause
mkClause ((c, d), i) = Clause [litC c, litC d] (NormalB (litI i)) []
  where litC = LitP . CharL
        litI = LitE . IntegerL . fromIntegral
