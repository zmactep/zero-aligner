module Sequence.Alignment.Instances where

import           Data.Array              ((!), bounds)
import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Char8   as B

import           Sequence.Alignment.Type

data EditDistance = EditDistance

data AlignmentType = GlobalAlignment
                   | LocalAlignment
                   | SemiglobalAlignment
  deriving (Show, Eq)

data SimpleAlignment = SimpleAlignment { aType :: AlignmentType
                                       , subst :: Substitution Char
                                       , aGap  :: Gap
                                       }

instance Alignment EditDistance where
  gap _ = -1
  substitution _ c d | c == d    = 0
                     | otherwise = -1
  conditions ed = Conditions { isStop = defaultStop ed
                             , isDiag = defaultDiag ed
                             , isVert = defaultVert ed
                             , isHoriz = defaultHoriz ed
                             }
  selector ed mat = let (_, sp) = bounds mat in (True, sp)
  inits ed = (* gap ed)
  additional _ = minBound

instance Alignment SimpleAlignment where
  gap = aGap
  substitution = subst
  conditions sa | aType sa == LocalAlignment = localConds sa
                | otherwise                  = defaultConds sa
  selector sa mat | aType sa == GlobalAlignment     = let (_, sp) = bounds mat
                                                      in  (False, sp)
                  | aType sa == LocalAlignment      = (False, localStart mat)
                  | aType sa == SemiglobalAlignment = (True, semiglobalStart mat)
  inits sa | aType sa == GlobalAlignment = (* gap sa)
           | otherwise                   = const 0
  additional sa | aType sa == LocalAlignment = 0
                | otherwise                  = minBound

mkGlobal :: Substitution Char -> Gap -> SimpleAlignment
mkGlobal = SimpleAlignment GlobalAlignment

mkLocal :: Substitution Char -> Gap -> SimpleAlignment
mkLocal = SimpleAlignment LocalAlignment

mkSemiglobal :: Substitution Char -> Gap -> SimpleAlignment
mkSemiglobal = SimpleAlignment SemiglobalAlignment

mkEditDistance :: EditDistance
mkEditDistance = EditDistance


-- Default helpers

localStart :: Matrix -> (Int, Int)
localStart = undefined

semiglobalStart :: Matrix -> (Int, Int)
semiglobalStart = undefined

subIJ :: Alignment a => a -> ByteString -> ByteString -> Substitution Int
subIJ sa s t i j = substitution sa (s `B.index` (i - 1)) (t `B.index` (j - 1))

localConds :: Alignment a => a -> Conditions
localConds a = (defaultConds a) { isStop = localStop a }

defaultConds :: Alignment a => a -> Conditions
defaultConds a = Conditions { isStop = defaultStop a
                            , isDiag = defaultDiag a
                            , isVert = defaultVert a
                            , isHoriz = defaultHoriz a
                            }

localStop :: Alignment a => a -> Condition
localStop _ m _ _ i j = i == 0 || j == 0 || m ! (i, j) == 0

defaultStop :: Alignment a => a -> Condition
defaultStop _ _ _ _ i j = i == 0 && j == 0

defaultDiag :: Alignment a => a -> Condition
defaultDiag sa m s t i j = i /= 0 && j /= 0 && m ! (i, j) == m ! (i - 1, j - 1) + sub i j
  where sub = subIJ sa s t

defaultVert :: Alignment a => a -> Condition
defaultVert sa m s t i j = i /= 0 && m ! (i, j) == m ! (i - 1, j) + gap sa

defaultHoriz :: Alignment a => a -> Condition
defaultHoriz sa m s t i j = j /= 0 && m ! (i, j) == m ! (i, j - 1) + gap sa
