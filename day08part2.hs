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
{-# LANGUAGE RecordWildCards #-}

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

value :: Tree Metadata -> Int
value Tree {..} | null tChildren = sum tNode
                | otherwise      = sum $ map childValue tNode
 where
  childValue :: Int -> Int
  childValue n | n < 1 || n > length tChildren = 0
               | otherwise = value $ head $ drop (n - 1) tChildren

solve :: String -> Either String Int
solve s = do
  d <- readTreeData s
  t <- readTree d
  return $ value t

example :: String
example = unwords $ map show expected

expected = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]

tests = do
  describe "parsing"
    $          it "works with example"
    $          readTreeData example
    `shouldBe` Right expected
  describe "solve" $ it "works with example" $ solve example `shouldBe` Right 66

main :: IO ()
main = do
  inp <- input
  hspec tests
  print $ solve inp
