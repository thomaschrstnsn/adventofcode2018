#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
  --package parsec
-}
{-# LANGUAGE DeriveFoldable #-}

import           Test.Hspec                     ( describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )
import           Text.ParserCombinators.Parsec

input :: IO String
input = readFile "day08input.txt"

type AParser a = GenParser Char () a

parser :: AParser [Int]
parser = do
  results <- sepBy pInt space
  _       <- many newline
  eof
  return results

pInt :: AParser Int
pInt = do
  digits <- many1 digit
  return $ read digits

readTreeData :: String -> Either String [Int]
readTreeData x = do
  let res = parse parser "in" x
  case res of
    (Left  err) -> Left $ show err
    (Right r  ) -> Right r

type Metadata = [Int]
data Tree a = Tree {tChildren :: [Tree a], tNode :: a} deriving (Show, Eq, Foldable)

type IntParser a = GenParser Int () a

pTree :: IntParser (Tree Metadata)
pTree = do
  res <- pNode
  _   <- eof
  return res

pNode :: IntParser (Tree Metadata)
pNode = do
  numChildren <- anyToken
  numMetadata <- anyToken
  children    <- count numChildren pNode
  meta        <- count numMetadata anyToken
  return $ Tree { tChildren = children, tNode = meta }

readTree :: [Int] -> Either String (Tree Metadata)
readTree x = do
  let res = parse pTree "in" x
  case res of
    (Left  err) -> Left $ show err
    (Right r  ) -> Right r

getChecksums :: Metadata -> [Int]
getChecksums = id

solve :: String -> Either String Int
solve s = do
  d <- readTreeData s
  t <- readTree d
  let checksums = concatMap getChecksums t
  return $ sum checksums

example :: String
example = unwords $ map show expected

expected = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]

tests = do
  describe "parsing"
    $          it "works with example"
    $          readTreeData example
    `shouldBe` Right expected
  describe "solve" $ it "works with example" $ solve example `shouldBe` Right
    138

main :: IO ()
main = do
  inp <- input
  hspec tests
  print $ solve inp
