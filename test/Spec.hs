{-# LANGUAGE TemplateHaskell #-}

import Data.List (genericLength)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lib (countWords, makeWordMap, trimNonAlphaNum)

main :: IO Bool
main = tests

tests :: IO Bool
tests =
  checkParallel $$(discover)

genText = forAll $ T.pack <$> Gen.list (Range.linear 0 100) Gen.unicode

prop_idempotentTrim :: Property
prop_idempotentTrim =
  property $ do
    text <- genText
    let f1 = trimNonAlphaNum text
    let f2 = trimNonAlphaNum f1
    f1 === f2

prop_shorterTrim :: Property
prop_shorterTrim =
  property $ do
    text <- genText
    let trimmed = trimNonAlphaNum text
    assert $ T.length trimmed <= T.length text

prop_commutable :: Property
prop_commutable =
  property $ do
    text <- genText
    let trimUpper = T.toUpper . trimNonAlphaNum $ text
    let upperTrim = trimNonAlphaNum . T.toUpper $ text
    trimUpper === upperTrim

prop_substring :: Property
prop_substring =
  property $ do
    text <- genText
    let trimmed = trimNonAlphaNum text
    assert $ trimmed `T.isInfixOf` text

prop_totalWordsSame :: Property
prop_totalWordsSame =
  property $ do
    text <- genText
    let words = filter (not . T.null) . fmap trimNonAlphaNum . T.words $ text
    let m = countWords words
    genericLength words === sum m
