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

data Area = Area
  { closest :: Position
  , points  :: [Point]
  } deriving (Show, Eq)

largestArea :: [Point] -> Area
largestArea ps =
  last $ sortOn (length . points) $ areas (map Position ps) (bound ps)

mth :: (Maybe a, b) -> Maybe (a, b)
mth (m, y) =
  case m of
    Just x  -> Just (x, y)
    Nothing -> Nothing

areas :: [Position] -> Box -> [Area]
areas ps box =
  map (\(pos, areaPoints) -> Area {closest = pos, points = areaPoints}) $
  grouping closest
  where
    closest :: [(Position, Point)]
    closest =
      mapMaybe (mth . (\p -> (closestPosition ps p, p))) (pointsInBox box)

closestPosition :: [Position] -> Point -> Maybe Position
closestPosition ps p =
  if length closestPositions == 1
    then Just $ head closestPositions
    else Nothing
  where
    posDist :: Position -> Int
    posDist (Position x) = distance x p
    distances = map (\p -> (posDist p, p)) ps
    grouped :: [(Int, [Position])]
    grouped = grouping distances
    sorted :: [(Int, [Position])]
    sorted = sortOn fst grouped
    closestPositions :: [Position]
    closestPositions = snd $ head sorted

solve :: String -> Either String (Position, Int)
solve s = (\a -> (closest a, length (points a))) . largestArea <$> readPoints s

example = intercalate "\n" ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]

expected = [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]

closestExamples =
  [ ((0, 0), a)
  , ((2, 1), a)
  , ((5, 0), Nothing)
  , ((6, 0), c)
  , ((0, 1), a)
  , ((5, 1), Nothing)
  , ((0, 4), Nothing)
  , ((3, 6), Nothing)
  , ((5, 5), e)
  ]
  where
    p x = Just $ Position x
    a = p (1, 1)
    b = p (1, 6)
    c = p (8, 3)
    d = p (3, 4)
    e = p (5, 5)
    f = p (8, 9)

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
  describe "closest" $
    specFromExamples
      closestExamples
      (\(input, position) ->
         specItem
           ("closest postion to " ++
            show input ++ " should be " ++ show position) $
         closestPosition (map Position expected) input `shouldBe` position)
  describe "solve" $
    it "works with example" $
    solve example `shouldBe` Right (Position (5, 5), 17)

first :: (a -> Bool) -> [a] -> Maybe a
first f xs =
  case filter f xs of
    x:_ -> Just x
    []  -> Nothing

showArea :: [Area] -> Box -> String
showArea areas box = intercalate "\n" lines
  where
    xs = [bLeft box .. bRight box]
    ys = [bBottom box .. bTop box]
    closestPositionForPoint :: Point -> Maybe Position
    closestPositionForPoint p =
      case filter (elem p . points) areas of
        x:_ -> Just $ closest x
        []  -> Nothing
    legend :: [(Point, Char)]
    legend = zipWith (\(Position p) c -> (p, c)) (map closest areas) ['A' ..]
    charForPoint :: Point -> Char
    charForPoint p =
      case (maybePosition, maybeClosest) of
        (Just c, _)        -> c
        (Nothing, Nothing) -> '.'
        (Nothing, Just c)  -> toLower c
      where
        maybePosition = snd <$> first ((p ==) . fst) legend
        maybeClosest = do
          (Position closest) <- closestPositionForPoint p
          legendForClosest <- first ((closest ==) . fst) legend
          return $ snd legendForClosest
    showPoint :: Point -> String
    showPoint (x, y) = show (x, y)
    header1 = show (head xs, head ys) ++ " " ++ show box
    header2 = map (last . show) xs
    lines =
      [header1, header2] ++
      map (\y -> concatMap (\x -> [charForPoint (x, y)]) xs ++ " " ++ show y) ys

main :: IO ()
main = do
  let box = bound expected
  let as = areas (map Position expected) box
  putStrLn $ showArea as box
  inp <- input
  hspec tests
  print $ solve inp
