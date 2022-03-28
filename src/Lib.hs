{-# LANGUAGE TupleSections #-}

module Lib
  ( makeWordMapFromStdin,
    makeWordMap,
    trimNonAlphaNum,
    countWords,
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
makeWordMap = countWords . filter (not . T.null) . fmap trimNonAlphaNum . T.words

trimNonAlphaNum :: T.Text -> T.Text
trimNonAlphaNum = T.dropAround (not . isAlphaNum)

countWords :: [T.Text] -> Map.Map T.Text Integer
countWords = Map.fromListWith (+) . fmap (,1)
