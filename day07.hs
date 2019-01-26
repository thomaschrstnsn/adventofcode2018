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
                                                , nub
                                                , sortOn
                                                )
import           Test.Hspec                     ( describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )
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

(==>) :: Step -> Step -> StepDependency
(==>) dep step = SD { step = step, dependsOn = dep }

data Graph a =
  Node a
       [Graph a]
  deriving (Eq, Show)

leaf :: a -> Graph a
leaf x = Node x []

single :: a -> Graph a -> Graph a
single x g = Node x [g]

multiple :: a -> [Graph a] -> Graph a
multiple = Node

node :: Graph a -> a
node (Node x _) = x

type DependencyGraph = Graph Step

except :: Eq a => [a] -> [a] -> [a]
except xs ys = filter (not . (`elem` ys)) xs

union :: Eq a => [a] -> [a] -> [a]
union xs ys = filter (`elem` ys) xs

stepsThatLeadTo :: [Step] -> [StepDependency] -> Step -> [Step]
stepsThatLeadTo remaining sds s =
  map dependsOn (filter ((== s) . step) sds) `union` remaining

fuse :: [DependencyGraph] -> [DependencyGraph]
fuse dgs = map (\(s, dgss) -> multiple s (concat dgss)) $ grouping $ map
  (\(Node s st) -> (s, st))
  dgs

expand :: [StepDependency] -> [Step] -> DependencyGraph -> [DependencyGraph]
expand sds remaining dg@(Node s _) = case stepsThatLeadTo remaining sds s of
  []  -> [dg]
  [x] -> [single x dg]
  xs  -> map (`single` dg) xs

buildGraph :: [StepDependency] -> [DependencyGraph]
buildGraph sds = build' (steps `except` terminals) $ map leaf terminals
 where
  steps        = nub $ concatMap (\sd -> [step sd, dependsOn sd]) sds
  dependencies = nub $ map dependsOn sds
  terminals    = steps `except` dependencies
  build' :: [Step] -> [DependencyGraph] -> [DependencyGraph]
  build' []             [done] = [done]
  build' remainingSteps level  = if level == next
    then level -- error $ "feck: " ++ show (length level) ++ " nodes: " ++ show
      -- (map node level)
    else build' rs' next
   where
    currentAndRemaining = remainingSteps ++ map node level
    next = fuse $ concatMap (expand sds currentAndRemaining) level
    stepsHere           = map node next
    rs'                 = remainingSteps `except` stepsHere

type Requirements = Map Step [Step]

buildReqs :: [StepDependency] -> Requirements
buildReqs sds =
  Map.fromAscList $ grouping $ map (\sd -> (step sd, dependsOn sd)) sds

run :: Requirements -> [DependencyGraph] -> String
run reqs dgs = run' dgs []
 where
  run' :: [DependencyGraph] -> [Step] -> String
  run' []  res = nub $ reverse $ map (\(Step c) -> c) res
  run' dgs res = run' dgs' res'
   where
    canRun :: DependencyGraph -> Bool
    canRun (Node s _) = case Map.lookup s reqs of
      Just deps -> null $ deps `except` res
      Nothing   -> True
    possible         = filter canRun dgs
    next             = head $ sortOn node possible
    (Node step deps) = next
    dgs'             = deps ++ filter (/= next) dgs
    res'             = step : res

solve :: String -> Either String String
solve s = do
  deps <- readDependencies s
  let gs = buildGraph deps
  let r  = buildReqs deps
  return $ run r gs

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

toDot :: [StepDependency] -> [String]
toDot sds = ["digraph {"] ++ map sdAsLine sds ++ ["}"]
 where
  sdAsLine :: StepDependency -> String
  sdAsLine sd = "\t" ++ [from] ++ " -> " ++ [to]
   where
    (Step from) = dependsOn sd
    (Step to  ) = step sd

writeDotFile :: [StepDependency] -> String -> IO ()
writeDotFile sds fn = writeFile fn content
  where content = intercalate "\n" (toDot sds)

main :: IO ()
main = do
  inp <- input
  hspec tests
  print $ solve inp
