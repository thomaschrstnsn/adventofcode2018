#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.18
  --package hspec
  --package hspec-core
  --package containers
  --package parsec
  --package time
-}
import           Control.Arrow                 ((&&&))
import           Data.Char                     (digitToInt)
import           Data.Function                 (on)
import           Data.List                     (group, groupBy, intercalate,
                                                nub, sort, sortOn)
import           Data.Ord                      (comparing)
import qualified Data.Time.Calendar            as TC
import qualified Data.Time.LocalTime           as LT
import           Specs                         (specFromExamples, specItem)
import           Test.Hspec                    (SpecWith, describe, hspec, it,
                                                shouldBe)
import           Text.ParserCombinators.Parsec

grouping :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
grouping =
  map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortOn fst

input :: IO String
input = readFile "day04input.txt"

newtype GuardId =
  GuardId Int
  deriving (Show, Eq, Ord)

data Event
  = BeginsShift GuardId
  | FallsAsleep
  | Wakes
  deriving (Show, Eq)

data TimeStamped a = TimeStamped
  { time   :: LT.LocalTime
  , entity :: a
  } deriving (Show, Eq)

(@@) :: a -> LT.LocalTime -> TimeStamped a
(@@) e ts = TimeStamped {time = ts, entity = e}

fromGregorian :: (Integer, Int, Int) -> (Int, Int) -> LT.LocalTime
fromGregorian (year, month, day) (hour, minute) =
  LT.LocalTime
    { LT.localDay = TC.fromGregorian year month day
    , LT.localTimeOfDay =
        LT.TimeOfDay {LT.todHour = hour, LT.todMin = minute, LT.todSec = 0}
    }

type AParser a = GenParser Char () a

parser :: AParser [TimeStamped Event]
parser = do
  results <- sepBy pTimeStampedEvent newline
  _ <- many newline
  eof
  return results

pTimeStampedEvent :: AParser (TimeStamped Event)
pTimeStampedEvent = do
  time <- pTime
  _ <- char ' '
  event <- pEvent
  return $ event @@ time

pTime :: AParser LT.LocalTime
pTime = do
  _ <- char '['
  year <- pInt
  _ <- char '-'
  month <- pInt
  _ <- char '-'
  day <- pInt
  _ <- char ' '
  hour <- pInt
  _ <- char ':'
  min <- pInt
  _ <- char ']'
  return $ fromGregorian (fromIntegral year, month, day) (hour, min)

pEvent :: AParser Event
pEvent = pWake <|> pAsleep <|> pBeginShift
  where
    pWake = do
      _ <- try $ string "wakes up"
      return Wakes
    pAsleep = do
      _ <- try $ string "falls asleep"
      return FallsAsleep
    pBeginShift = do
      _ <- try $ string "Guard #"
      id <- pInt
      _ <- string " begins shift"
      return $ BeginsShift $ GuardId id

pInt :: AParser Int
pInt = do
  digits <- many1 digit
  return $ read digits

readEvents :: String -> Either String [TimeStamped Event]
readEvents x = do
  let res = parse parser "in" x
  case res of
    (Left err) -> Left $ show err
    (Right r)  -> Right r

orderEvents :: [TimeStamped Event] -> [TimeStamped Event]
orderEvents = sortOn time

data SleepingGuard = SleepingGuard
  { sgId   :: GuardId
  , sleeps :: LT.LocalTime
  , wakes  :: LT.LocalTime
  } deriving (Show, Eq)

eventsAsSleepingGuards :: [TimeStamped Event] -> [SleepingGuard]
eventsAsSleepingGuards es = sleepingGuardHelper (tail ordered) firstGuard []
  where
    ordered = orderEvents es
    firstGuard =
      case entity $ head ordered of
        BeginsShift id -> id
        _              -> error "should start with a begin shift"
    sleepingGuardHelper ::
         [TimeStamped Event] -> GuardId -> [SleepingGuard] -> [SleepingGuard]
    sleepingGuardHelper [] _ res = res
    sleepingGuardHelper (first:next:rest) guard accum =
      case (entity first, entity next) of
        (FallsAsleep, Wakes) ->
          let sg =
                SleepingGuard
                  {sgId = guard, sleeps = time first, wakes = time next}
           in sleepingGuardHelper rest guard (accum ++ [sg])
        (BeginsShift newGuard, _) ->
          sleepingGuardHelper (next : rest) newGuard accum
        (_, _) -> error $ "mismatching events, first: " ++ show first

minuteAccess :: LT.LocalTime -> Int
minuteAccess = LT.todMin . LT.localTimeOfDay

minutesSleeping :: SleepingGuard -> Int
minutesSleeping g = w - s
  where
    w = minuteAccess (wakes g)
    s = minuteAccess (sleeps g)

mostSleepingGuard :: [SleepingGuard] -> GuardId
mostSleepingGuard gs = fst $ last $ sortOn snd guardSleeps
  where
    byGuard :: [(GuardId, [SleepingGuard])]
    byGuard = grouping $ map (\g -> (sgId g, g)) gs
    guardSleeps :: [(GuardId, Int)]
    guardSleeps = map (\(g, sgs) -> (g, sum $ map minutesSleeping sgs)) byGuard

minuteRange :: SleepingGuard -> [Int]
minuteRange sg = [start .. end]
  where
    start = minuteAccess (sleeps sg)
    end = minuteAccess (wakes sg) - 1

freq :: Ord a => [a] -> [(a, Int)]
freq = map (head &&& length) . group . sort

data SleepingMinute = Sm
  { minute    :: Int
  , frequency :: Int
  } deriving (Show, Eq)

mostSleepingMinute :: [SleepingGuard] -> GuardId -> SleepingMinute
mostSleepingMinute sgs guard = Sm {minute = min, frequency = frq}
  where
    sleeps = concatMap minuteRange $ filter (\sg -> sgId sg == guard) sgs
    (min, frq) = last $ sortOn snd $ freq sleeps

solve :: String -> Either String Int
solve s = do
  guards <- eventsAsSleepingGuards <$> readEvents s
  let minutes = map (\g -> (sgId g, mostSleepingMinute guards (sgId g))) guards
  let (GuardId guard, best) = last $ sortOn (frequency . snd) minutes
  return $ guard * minute best

example =
  intercalate
    "\n"
    [ "[1518-11-01 00:00] Guard #10 begins shift"
    , "[1518-11-01 00:05] falls asleep"
    , "[1518-11-01 00:25] wakes up"
    , "[1518-11-01 00:30] falls asleep"
    , "[1518-11-01 00:55] wakes up"
    , "[1518-11-01 23:58] Guard #99 begins shift"
    , "[1518-11-02 00:40] falls asleep"
    , "[1518-11-02 00:50] wakes up"
    , "[1518-11-03 00:05] Guard #10 begins shift"
    , "[1518-11-03 00:24] falls asleep"
    , "[1518-11-03 00:29] wakes up"
    , "[1518-11-04 00:02] Guard #99 begins shift"
    , "[1518-11-04 00:36] falls asleep"
    , "[1518-11-04 00:46] wakes up"
    , "[1518-11-05 00:03] Guard #99 begins shift"
    , "[1518-11-05 00:45] falls asleep"
    , "[1518-11-05 00:55] wakes up"
    ]

expected =
  [ (BeginsShift $ GuardId 10) @@ fromGregorian (1518, 11, 01) (00, 00)
  , FallsAsleep @@ fromGregorian (1518, 11, 01) (00, 05)
  , Wakes @@ fromGregorian (1518, 11, 01) (00, 25)
  , FallsAsleep @@ fromGregorian (1518, 11, 01) (00, 30)
  , Wakes @@ fromGregorian (1518, 11, 01) (00, 55)
  , (BeginsShift $ GuardId 99) @@ fromGregorian (1518, 11, 01) (23, 58)
  , FallsAsleep @@ fromGregorian (1518, 11, 02) (00, 40)
  , Wakes @@ fromGregorian (1518, 11, 02) (00, 50)
  , (BeginsShift $ GuardId 10) @@ fromGregorian (1518, 11, 03) (00, 05)
  , FallsAsleep @@ fromGregorian (1518, 11, 03) (00, 24)
  , Wakes @@ fromGregorian (1518, 11, 03) (00, 29)
  , (BeginsShift $ GuardId 99) @@ fromGregorian (1518, 11, 04) (00, 02)
  , FallsAsleep @@ fromGregorian (1518, 11, 04) (00, 36)
  , Wakes @@ fromGregorian (1518, 11, 04) (00, 46)
  , (BeginsShift $ GuardId 99) @@ fromGregorian (1518, 11, 05) (00, 03)
  , FallsAsleep @@ fromGregorian (1518, 11, 05) (00, 45)
  , Wakes @@ fromGregorian (1518, 11, 05) (00, 55)
  ]

expectedSleepingGuards =
  [ sg 10 ((1518, 11, 01), (00, 05)) ((1518, 11, 01), (00, 25))
  , sg 10 ((1518, 11, 01), (00, 30)) ((1518, 11, 01), (00, 55))
  , sg 99 ((1518, 11, 02), (00, 40)) ((1518, 11, 02), (00, 50))
  , sg 10 ((1518, 11, 03), (00, 24)) ((1518, 11, 03), (00, 29))
  , sg 99 ((1518, 11, 04), (00, 36)) ((1518, 11, 04), (00, 46))
  , sg 99 ((1518, 11, 05), (00, 45)) ((1518, 11, 05), (00, 55))
  ]
  where
    sg id s w =
      SleepingGuard
        { sgId = GuardId id
        , sleeps = uncurry fromGregorian s
        , wakes = uncurry fromGregorian w
        }

tests = do
  describe "parsing" $
    it "works with example" $ readEvents example `shouldBe` Right expected
  describe "ordering" $
    it "can sort as expected" $
    orderEvents (reverse expected) `shouldBe` expected
  describe "eventsAsSleepingGuards" $
    it "works with example" $
    eventsAsSleepingGuards (reverse expected) `shouldBe` expectedSleepingGuards
  describe "most sleeping guard" $
    it "works with example" $
    mostSleepingGuard (eventsAsSleepingGuards (reverse expected)) `shouldBe`
    GuardId 10
  describe "guard's most sleeping minute" $
    it "works with example" $
    mostSleepingMinute (eventsAsSleepingGuards (reverse expected)) (GuardId 10) `shouldBe`
    Sm {minute = 24, frequency = 2}
  describe "solve" $
    it "works with example" $ solve example `shouldBe` Right 4455

main :: IO ()
main = do
  inp <- input
  hspec tests
  print $ solve inp
