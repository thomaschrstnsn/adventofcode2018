#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
-}
import           Data.Char  (digitToInt)
import           Data.Set   (Set)
import qualified Data.Set   as Set
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

findFirstRepeatFreq :: [Int] -> Int
findFirstRepeatFreq xs = helper (cycle xs) 0 (Set.fromList [0])
  where
    helper :: [Int] -> Int -> Set Int -> Int
    helper (x:xs) cur seen =
      let next = x + cur
       in if Set.member next seen
            then next
            else helper xs next (Set.union (Set.fromList [next]) seen)

solve :: [String] -> Int
solve = findFirstRepeatFreq . readInputs

tests =
  describe "solve" $
  specFromExamples
    [ (["+1", "-1"], 0)
    , (["+3", "+3", "+4", "-2", "-4"], 10)
    , (["-6", "+3", "+8", "+5", "-6"], 5)
    , (["+7", "+7", "-2", "-7", "-4"], 14)
    ]
    (\(input, expected) ->
       specItem (show input ++ " first reaches: " ++ show expected ++ " twice") $
       solve input `shouldBe` expected)

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
