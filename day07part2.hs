#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
  --package parsec
-}

import           Data.Char                      ( ord )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , intercalate
                                                , sort
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

data Worker = Worker { wTask :: Step, wRemaining :: Int} deriving (Show, Eq, Ord)

run :: Configuration -> Int -> Set Worker -> DependencyGraph -> Int
run conf time workers dg | Map.empty == dg = time
                         | otherwise       = run conf tnext workers' next
 where
  removeDone s ds = if Set.member s doneSteps
    then Nothing
    else Just $ Set.difference ds doneSteps
  ticked       = Set.map (\w -> w { wRemaining = wRemaining w - 1 }) workers
  done         = Set.filter ((== 0) . wRemaining) ticked
  doneSteps    = Set.map wTask done
  notDone      = Set.difference ticked done
  notDoneSteps = Set.map wTask notDone
  next         = Map.mapMaybeWithKey removeDone dg
  runnable     = Set.fromList $ Map.keys $ Map.filter Set.null next
  current      = sort $ Set.toList $ Set.difference runnable notDoneSteps
  available    = numberOfWorkers conf - Set.size notDone
  newWorkers =
    Set.fromList
      $ map
          (\(Step c) -> Worker
            { wTask      = Step c
            , wRemaining = ord c + offsetPerTask conf - (ord 'A' - 1)
            }
          )
      $ take available current
  workers' = Set.union notDone newWorkers
  tnext    = if Map.empty == next then time else time + 1

data Configuration = Config {numberOfWorkers, offsetPerTask :: Int}

solve :: Configuration -> String -> Either String Int
solve c s = do
  deps <- readDependencies s
  let gs = buildGraph deps
  return $ run c 0 Set.empty gs

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
  describe "solve"
    $          it "works with example"
    $          solve conf example
    `shouldBe` Right 15
  where conf = Config { numberOfWorkers = 2, offsetPerTask = 0 }

main :: IO ()
main = do
  inp <- input
  hspec tests
  let config = Config { numberOfWorkers = 5, offsetPerTask = 60 }
  print $ solve config inp
