#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
-}
import Data.Char (digitToInt)
import Specs (specFromExamples, specItem)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import Data.Set (Set)
import Data.List (nub, sortBy, tails)
import qualified Data.Set as Set

input :: IO [String]
input = lines <$> readFile "day02input.txt"

arePrototypes :: String -> String -> Bool
arePrototypes x y = helper x y True
  where
    helper :: String -> String -> Bool -> Bool
    helper [] (y:ys) _ = False
    helper (x:xs) [] _ = False
    helper [] [] _ = True
    helper (x:xs) (y:ys) True =
      if x == y
        then
          helper xs ys True
        else
          helper xs ys False
    helper (x:xs) (y:ys) False =
      (x == y) && helper xs ys False

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
            
prototypes :: [String] -> [(String, String)]
prototypes xs = filter (uncurry arePrototypes) $ pairs xs

sameParts :: String -> String -> String
sameParts x y = helper x y []
  where
    helper :: String -> String -> String -> String
    helper [] (y:ys) r = r
    helper (x:xs) [] r = r
    helper [] [] r = r
    helper (x:xs) (y:ys) r =
      if x == y
        then
          helper xs ys (r ++ [x])
        else
          helper xs ys r

solve :: [String] -> [String]
solve xs = map (\(x,y) -> "(" ++ show x ++ ", " ++ show y ++ "): " ++ show (sameParts x y)) $ prototypes xs

examples = 
  [ "abcde"
  , "fghij"
  , "klmno"
  , "pqrst"
  , "fguij"
  , "axcye"
  , "wvxyz"
  ]

tests = do 
  describe "prototypes" $ 
    it "works as prescribed" $
      prototypes examples `shouldBe` [("fghij", "fguij")]
  describe "solve" $
    it "works" $
      solve examples `shouldBe` ["(\"fghij\", \"fguij\"): \"fgij\""]

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
