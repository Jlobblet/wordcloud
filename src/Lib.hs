{-# LANGUAGE TupleSections #-}

module Lib
  ( makeWordMap,
  )
where

import Control.Applicative (liftA2)
import Data.Char (isAlphaNum, isSpace)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

makeWordMapFromStdin :: IO (Map.Map T.Text Integer)
makeWordMapFromStdin = makeWordMap <$> T.IO.getContents

makeWordMap :: T.Text -> Map.Map T.Text Integer
makeWordMap = countWords . filter (not . T.null) . fmap filterNonAlphaNum . T.words

filterNonAlphaNum :: T.Text -> T.Text
filterNonAlphaNum = liftA2 (.) T.dropWhileEnd T.dropWhile (not . isAlphaNum)

countWords :: [T.Text] -> Map.Map T.Text Integer
countWords = Map.fromListWith (+) . fmap (,1)
