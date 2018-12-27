#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
-}
import           Data.Char  (digitToInt)
import           Data.List  (nub, sortBy)
import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Specs      (specFromExamples, specItem)
import           Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

input :: IO [String]
input = lines <$> readFile "day02input.txt"

data PackageType
  = Twopeet
  | Threepeet
  deriving (Show, Eq, Ord)

type Checksum = Set PackageType

freq :: (Eq a) => [a] -> [(Int, a)]
freq xs = map count uniques
  where
    uniques = nub xs
    count x = (length $ filter (== x) xs, x)

checksum :: String -> Checksum
checksum xs = Set.fromList repeats
  where
    frequencies = freq xs
    hasSome :: [a] -> Bool
    hasSome = not . null
    filterFreq :: Int -> [(Int, Char)]
    filterFreq x = filter (\(f, _) -> f == x) frequencies
    twos = hasSome $ filterFreq 2
    threes = hasSome $ filterFreq 3
    repeats =
      case (twos, threes) of
        (True, True)   -> [Twopeet, Threepeet]
        (False, True)  -> [Threepeet]
        (True, False)  -> [Twopeet]
        (False, False) -> []

solve :: [String] -> Int
solve xs = withTwos * withThrees
  where
    checksums = map checksum xs
    countWithElem x = length $ filter (Set.member x) checksums
    withTwos = countWithElem Twopeet
    withThrees = countWithElem Threepeet

examples =
  [ ("abcdef", none)
  , ("bababc", both)
  , ("abbcde", two)
  , ("abcccd", three)
  , ("aabcdd", two)
  , ("abcdee", two)
  , ("ababab", three)
  ]
  where
    none = Set.empty
    two = Set.fromList [Twopeet]
    three = Set.fromList [Threepeet]
    both = Set.union two three

tests = do
  describe "solve" $
    it "works with example" $ solve (map fst examples) `shouldBe` 12
  describe "checksum" $
    specFromExamples
      examples
      (\(input, expected) ->
         specItem (show input ++ " should be: " ++ show expected) $
         checksum input `shouldBe` expected)

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
