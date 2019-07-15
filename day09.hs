#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

import           Test.Hspec                     ( describe
                                                , hspec
                                                , it
                                                , shouldBe
                                                )
import           Specs                          ( specFromExamples
                                                , specItem
                                                )
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.Foldable                  ( maximumBy
                                                , Foldable
                                                )
import           Data.Ord                       ( comparing )
import           Debug.Trace                    ( traceShow )

data Game = Game {
  gPlayers :: Int,
  gLastMarble :: Int
} deriving (Show, Eq)

input :: Game
input = Game { gPlayers = 418, gLastMarble = 71339 }

type Marbles = [Int]

putInto :: Marbles -> Int -> Marbles
putInto !input ele = rotateAroundAndInsert rotation input ele
 where
  rotateAroundAndInsert index !m ele =
    let (second, first) = splitAt index m in concat [[ele], first, second]
  rotation = case length input of
    1 -> 1
    2 -> 0
    _ -> 2

removeNumber7CounterClockwise :: Marbles -> (Marbles, Int)
removeNumber7CounterClockwise !m =
  let rotateAroundCounter n a =
          let modLength       = n `mod` length a
              (second, first) = splitAt modLength $ reverse a
          in  reverse $ first ++ second
      rot     = rotateAroundCounter 7 m
      res     = drop 1 rot
      removed = head rot
  in  (res, removed)

maximumOn :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maximumOn = maximumBy . comparing

solve :: Game -> Int
solve game = winner
 where
  finalScores = runRounds game (initial game) [0] 1
  playerScores =
    (\(player, !scores) -> (player, sum scores)) <$> IntMap.toList finalScores
  winner = maximum $ snd <$> playerScores

type Score = IntMap [Int]

initial :: Game -> Score
initial g = IntMap.fromList $ (, []) <$> [1 .. gPlayers g]

runRounds :: Game -> Score -> Marbles -> Int -> Score
runRounds game score !marbles round
  | round > gLastMarble game = score
  | otherwise = if multipleOf23
    then
      let (nextMarbles, removedMarble) = removeNumber7CounterClockwise marbles
          nextScore = IntMap.adjust (\l -> l ++ [round, removedMarble])
                                    currentPlayer
                                    score
      in  runRounds game nextScore nextMarbles nextRound
    else
      let nextMarbles = putInto marbles round
      in  runRounds game score nextMarbles nextRound
 where
  multipleOf23  = round `mod` 23 == 0
  nextRound     = round + 1
  currentPlayer = round `mod` gPlayers game

tests = do
  describe "putInto" $ specFromExamples
    [ ([0]                  , 1, [1, 0])
    , ([1, 0]               , 2, [2, 1, 0])
    , ([2, 1, 0]            , 3, [3, 0, 2, 1])
    , ([3, 0, 2, 1]         , 4, [4, 2, 1, 3, 0])
    , ([4, 2, 1, 3, 0]      , 5, [5, 1, 3, 0, 4, 2])
    , ([5, 1, 3, 0, 4, 2]   , 6, [6, 3, 0, 4, 2, 5, 1])
    , ([6, 3, 0, 4, 2, 5, 1], 7, [7, 0, 4, 2, 5, 1, 6, 3])
    ]
    (\(input, ele, expected) ->
      specItem
          (  "putting: "
          ++ show ele
          ++ " into "
          ++ show input
          ++ " should yield: "
          ++ show expected
          )
        $          putInto input ele
        `shouldBe` expected
    )
  describe "removeNumber7"
    $ it "works as example"
    $ let input =
            [ 22
            , 11
            , 1
            , 12
            , 6
            , 13
            , 3
            , 14
            , 7
            , 15
            , 0
            , 16
            , 8
            , 17
            , 4
            , 18
            , 9
            , 19
            , 2
            , 20
            , 10
            , 21
            , 5
            ]
          expected =
            [ 19
            , 2
            , 20
            , 10
            , 21
            , 5
            , 22
            , 11
            , 1
            , 12
            , 6
            , 13
            , 3
            , 14
            , 7
            , 15
            , 0
            , 16
            , 8
            , 17
            , 4
            , 18
            ]
      in  removeNumber7CounterClockwise input `shouldBe` (expected, 9)
  describe "solving" $ specFromExamples
    [ ( (9, 25)
      , 32
      )
--    , ((10, 1618), 8317)
    , ((13, 7999), 146373)
    , ((17, 1104), 2764)
    , ((21, 6111), 54718)
    , ((30, 5807), 37305)
    ]
    (\((players, lastMarble), expected) ->
      specItem
          (  "solving: players: "
          ++ show players
          ++ " last marble: "
          ++ show lastMarble
          ++ " should yield: "
          ++ show expected
          )
        $          solve (Game { gPlayers = players, gLastMarble = lastMarble })
        `shouldBe` expected
    )

main :: IO ()
main = do
  hspec tests
  print $ solve input
