#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
  --package parsec
-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Char                     (toLower)
import           Data.Function                 (on)
import           Data.List                     (groupBy, intercalate, sortOn)
import           Data.Maybe                    (mapMaybe)
import           Specs                         (specFromExamples, specItem)
import           Test.Hspec                    (SpecWith, describe, hspec, it,
                                                shouldBe)
import           Text.ParserCombinators.Parsec

grouping :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
grouping =
  map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortOn fst

input :: IO String
input = readFile "day06input.txt"

type AParser a = GenParser Char () a

parser :: AParser [Point]
parser = do
  results <- sepBy pPoint newline
  _ <- many newline
  eof
  return results

pPoint :: AParser Point
pPoint = do
  x <- pInt
  _ <- string ", "
  y <- pInt
  return (x, y)

pInt :: AParser Int
pInt = do
  digits <- many1 digit
  return $ read digits

readPoints :: String -> Either String [Point]
readPoints x = do
  let res = parse parser "in" x
  case res of
    (Left err) -> Left $ show err
    (Right r)  -> Right r

type Point = (Int, Int)

data Box = Box
  { bTop    :: Int
  , bBottom :: Int
  , bLeft   :: Int
  , bRight  :: Int
  } deriving (Show, Eq)

bound :: [Point] -> Box
bound ps = Box {bTop = maxY, bBottom = minY, bLeft = minX, bRight = maxX}
  where
    xs = map fst ps
    ys = map snd ps
    minX = minimum xs
    maxX = maximum xs
    maxY = maximum ys
    minY = minimum ys

pointsInBox :: Box -> [Point]
pointsInBox Box {..} = [(x, y) | x <- [bLeft .. bRight], y <- [bBottom .. bTop]]

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

newtype Position =
  Position Point
  deriving (Eq, Show, Ord)

pointsInRegion :: Int -> [Position] -> [Point]
pointsInRegion maxDist positions =
  filter (\p -> sumDist p < maxDist) (pointsInBox box)
  where
    points = map (\(Position p) -> p) positions
    box = bound points
    sumDist :: Point -> Int
    sumDist p = sum (map (distance p) points)

solve :: Int -> String -> Either String Int
solve maxDistance s =
  length . pointsInRegion maxDistance . map Position <$> readPoints s

example = intercalate "\n" ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]

expected = [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]

tests = do
  describe "parsing" $
    it "works with example" $ readPoints example `shouldBe` Right expected
  describe "bound" $
    it "it works for the example" $
    bound expected `shouldBe` Box {bTop = 9, bBottom = 1, bLeft = 1, bRight = 8}
  describe "bound vs pointsInBound" $
    it "converge as inverse" $
    bound (pointsInBox (bound expected)) `shouldBe` bound expected
  describe "distance" $
    specFromExamples
      [((0, 0), 0), ((1, 0), 1), ((20, 20), 40)]
      (\(input, expected) ->
         specItem
           ("distance from origo to " ++
            show input ++ " should be " ++ show expected) $
         distance (0, 0) input `shouldBe` expected)
  describe "solve" $
    it "works with example" $ solve 32 example `shouldBe` Right 16

main :: IO ()
main = do
  let box = bound expected
  inp <- input
  hspec tests
  print $ solve 10000 inp
