#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
  --package parsec
-}

import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , intercalate
                                                , sortOn
                                                )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Text.ParserCombinators.Parsec

grouping :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
grouping =
  map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortOn fst

input :: IO String
input = readFile "day07input.txt"

type AParser a = GenParser Char () a

parser :: AParser [StepDependency]
parser = do
  results <- sepBy pDependency newline
  _       <- many newline
  eof
  return results

-- Step F must be finished before step E can begin.
pDependency :: AParser StepDependency
pDependency = do
  _    <- string "Step "
  dep  <- Step <$> letter
  _    <- string " must be finished before step "
  step <- Step <$> letter
  _    <- string " can begin."
  return $ dep ==> step

readDependencies :: String -> Either String [StepDependency]
readDependencies x = do
  let res = parse parser "in" x
  case res of
    (Left  err) -> Left $ show err
    (Right r  ) -> Right r

newtype Step =
  Step Char
  deriving (Eq, Show, Ord)

data StepDependency = SD
  { step      :: Step
  , dependsOn :: Step
  } deriving (Eq, Show, Ord)

-- | Steps and their required dependencies
type DependencyGraph = Map Step (Set Step)

buildGraph :: [StepDependency] -> DependencyGraph
buildGraph sds =
  Map.union internal $ Map.fromAscList $ map (\l -> (l, Set.empty)) $ Set.toList
    leafs
 where
  internal =
    Map.fromAscList $ map (\(k, vs) -> (k, Set.fromList vs)) $ grouping $ map
      (\sd -> (step sd, dependsOn sd))
      sds
  allSteps = Set.fromList $ map dependsOn sds
  leafs    = Set.difference allSteps (Set.fromList $ Map.keys internal)

(==>) :: Step -> Step -> StepDependency
(==>) dep step = SD { step = step, dependsOn = dep }

run :: [Step] -> DependencyGraph -> [Step]
run res dg | Map.empty == dg = reverse res
           | otherwise       = run res' next
 where
  removeCurrent s ds =
    if s == current then Nothing else Just $ Set.filter (/= current) ds
  runnable = Map.filter Set.null dg
  current  = minimum $ Map.keys runnable
  next     = Map.mapMaybeWithKey removeCurrent dg
  res'     = current : res

solve :: String -> Either String String
solve s = do
  deps <- readDependencies s
  let gs    = buildGraph deps
  let steps = run [] gs
  return $ map (\(Step c) -> c) steps

example :: String
example = intercalate
  "\n"
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

expected :: [StepDependency]
expected = [c ==> a, c ==> f, a ==> b, a ==> d, b ==> e, d ==> e, f ==> e]
  where [a, b, c, d, e, f] = map Step ['A' .. 'F']

tests = do
  describe "parsing"
    $          it "works with example"
    $          readDependencies example
    `shouldBe` Right expected
  describe "solve" $ it "works with example" $ solve example `shouldBe` Right
    "CABDFE"

main :: IO ()
main = do
  inp <- input
  hspec tests
  print $ solve inp
