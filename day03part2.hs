#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
  --package parsec
-}
import           Data.Char                     (digitToInt)
import           Data.List                     (intercalate, nub, sortBy)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Specs                         (specFromExamples, specItem)
import           Test.Hspec                    (SpecWith, describe, hspec, it,
                                                shouldBe)
import           Text.ParserCombinators.Parsec

input :: IO String
input = readFile "day03input.txt"

newtype ClaimId =
  ClaimId Int
  deriving (Show, Eq)

data Claim = Claim
  { claimId     :: ClaimId
  , claimTop    :: Int
  , claimLeft   :: Int
  , claimWidth  :: Int
  , claimHeight :: Int
  } deriving (Show, Eq)

type AParser a = GenParser Char () a

parser :: AParser [Claim]
parser = do
  results <- sepBy pClaim newline
  _ <- many newline
  eof
  return results

pClaim :: AParser Claim
pClaim = do
  id <- pId
  _ <- string " @ "
  (left, top) <- pPos
  _ <- string ": "
  (width, height) <- pSize
  return $
    Claim
      { claimId = id
      , claimTop = top
      , claimLeft = left
      , claimWidth = width
      , claimHeight = height
      }

pId :: AParser ClaimId
pId = do
  _ <- char '#'
  ClaimId <$> pInt

pPos :: AParser (Int, Int)
pPos = do
  left <- pInt
  _ <- char ','
  top <- pInt
  return (left, top)

pSize :: AParser (Int, Int)
pSize = do
  width <- pInt
  _ <- char 'x'
  height <- pInt
  return (width, height)

pInt :: AParser Int
pInt = do
  digits <- many1 digit
  return $ read digits

readClaims :: String -> Either String [Claim]
readClaims x = do
  let res = parse parser "in" x
  case res of
    (Left err) -> Left $ show err
    (Right r)  -> Right r

type Square = (Int, Int)

squaresFromClaim :: Claim -> Set Square
squaresFromClaim c =
  Set.fromList [(x, y) | x <- [left .. right], y <- [top .. bottom]]
  where
    left = claimLeft c
    right = left + claimWidth c - 1
    top = claimTop c
    bottom = top + claimHeight c - 1

nonOverlappingSquares :: [Claim] -> [ClaimId]
nonOverlappingSquares claims = map fst $ filter noOverlaps squares
  where
    squares = map (\c -> (claimId c, squaresFromClaim c)) claims
    noOverlaps c = 0 == Set.size (Set.unions $ map (overlaps c) squares)
    overlaps (_, c1) (_, c2) =
      if c1 == c2
        then Set.empty
        else Set.intersection c1 c2

solve :: String -> Either String ClaimId
solve s = head . nonOverlappingSquares <$> readClaims s

example = intercalate "\n" ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

tests = do
  describe "parsing" $
    it "works with example" $
    readClaims example `shouldBe`
    Right
      [ (Claim
           { claimId = ClaimId 1
           , claimLeft = 1
           , claimTop = 3
           , claimWidth = 4
           , claimHeight = 4
           })
      , (Claim
           { claimId = ClaimId 2
           , claimLeft = 3
           , claimTop = 1
           , claimWidth = 4
           , claimHeight = 4
           })
      , (Claim
           { claimId = ClaimId 3
           , claimLeft = 5
           , claimTop = 5
           , claimWidth = 2
           , claimHeight = 2
           })
      ]
  describe "squares from claim" $
    specFromExamples
      [ ( Claim
            { claimId = ClaimId 123
            , claimTop = 0
            , claimLeft = 0
            , claimWidth = 1
            , claimHeight = 1
            }
        , [(0, 0)])
      , ( Claim
            { claimId = ClaimId 234
            , claimTop = 0
            , claimLeft = 0
            , claimWidth = 2
            , claimHeight = 2
            }
        , [(0, 0), (0, 1), (1, 0), (1, 1)])
      , ( Claim
            { claimId = ClaimId 345
            , claimTop = 50
            , claimLeft = 60
            , claimWidth = 2
            , claimHeight = 2
            }
        , [(60, 50), (60, 51), (61, 50), (61, 51)])
      ]
      (\(input, expected) ->
         specItem (show input ++ " should yield the squares: " ++ show expected) $
         squaresFromClaim input `shouldBe` Set.fromList expected)
  describe "solve" $
    it "works with example" $ solve example `shouldBe` Right (ClaimId 3)

main :: IO ()
main = do
  hspec tests
  inp <- input
  print $ solve inp
