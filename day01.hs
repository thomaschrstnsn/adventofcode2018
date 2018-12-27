#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
-}
import           Data.Char  (digitToInt)
import           Specs      (specFromExamples, specItem)
import           Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

input :: IO [String]
input = lines <$> readFile "day01input.txt"

readInputs :: [String] -> [Int]
readInputs = map readInt
  where
    readInt :: String -> Int
    readInt x@(s:ss) =
      case s of
        '+' -> read ss
        '-' -> negate $ read ss
        _   -> read x

sumInputs :: [Int] -> Int
sumInputs = sum

solve :: [String] -> Int
solve = sumInputs . readInputs

tests = do
  describe "readInputs" $
    specFromExamples
      [ (["+1", "-2", "+3", "+1"], [1, -2, 3, 1])
      , (["+1", "+1", "+1"], [1, 1, 1])
      , (["+1", "+1", "-2"], [1, 1, -2])
      , (["-1", "-2", "-3"], [-1, -2, -3])
      ]
      (\(input, expected) ->
         specItem (show input ++ " should be: " ++ show expected) $
         readInputs input `shouldBe` expected)
  describe "solve" $
    specFromExamples
      [ (["+1", "-2", "+3", "+1"], 3)
      , (["+1", "+1", "+1"], 3)
      , (["+1", "+1", "-2"], 0)
      , (["-1", "-2", "-3"], -6)
      ]
      (\(input, expected) ->
         specItem (show input ++ " should be: " ++ show expected) $
         solve input `shouldBe` expected)

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
