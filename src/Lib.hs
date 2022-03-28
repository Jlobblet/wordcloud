{-# LANGUAGE TupleSections #-}

module Lib
  ( makeWordMap,
  )
where

import Control.Applicative (liftA2)
import Data.Char (isAlphaNum, isSpace)
import Data.Function ((&))
import Data.List (dropWhileEnd)
import qualified Data.Map as Map
import Data.Map.Strict (insertWith)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

makeWordMap :: IO (Map.Map T.Text Integer)
makeWordMap = do
  contents <- T.IO.getContents
  contents
    & T.words
    & fmap filterNonAlphaNum
    & filter (not . T.null)
    & countWords
    & pure

filterNonAlphaNum :: T.Text -> T.Text
filterNonAlphaNum = liftA2 (.) T.dropWhileEnd T.dropWhile (not . isAlphaNum)

countWords :: [T.Text] -> Map.Map T.Text Integer
countWords = Map.fromListWith (+) . fmap (,1)
