#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
-}
import           Data.Char  (toLower, toUpper)
import           Data.List  (minimum, nub)
import           Specs      (specFromExamples, specItem)
import           Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

input :: IO String
input = head . lines <$> readFile "day05input.txt"

removeUnits :: String -> Char -> String
removeUnits s x = filter (\c -> (c /= low) && (c /= up)) s
  where
    low = toLower x
    up = toUpper x

allPermutes :: String -> [String]
allPermutes s = map (removeUnits s) chars
  where
    chars = nub $ map toLower s

react :: String -> String
react s = helper s ""
  where
    canReact :: Char -> Char -> Bool
    canReact x y = (x /= y) && (toLower x == toLower y)
    helper [] [] = []
    helper [] [x] = [x]
    helper [] (x:y:res)
      | canReact x y = helper [] res
      | otherwise = reverse (x : y : res)
    helper (x:xs) [] = helper xs [x]
    helper (x:xs) (y:ys)
      | canReact x y = helper xs ys
      | otherwise = helper xs (x : y : ys)

solve :: String -> Int
solve s = minimum $ map (length . react) (allPermutes s)

tests =
  describe "reactions removed" $
  specFromExamples
    [ ("aA", "")
    , ("abBA", "")
    , ("abAB", "abAB")
    , ("aabAAB", "aabAAB")
    , ("dabAcCaCBAcCcaDA", "dabCBAcaDA")
    ]
    (\(input, expected) ->
       specItem (show input ++ " should yield: " ++ show expected) $
       react input `shouldBe` expected)

main :: IO ()
main = do
  inp <- input
  hspec tests
  print $ solve inp
