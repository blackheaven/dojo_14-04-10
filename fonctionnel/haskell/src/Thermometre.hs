module Thermometre (
          Temperature(..)
        , DayStmt(..), morning, evening, makeDayStmt
        , WeekStmt(..), makeWeekStmt
        , MonthStmt(..), makeMonthStmt
        , Statistics(..)
        , temperatureToStatistics
        , avg
        , FilterPosition
        , filterStats
        )  where

import Data.Monoid

data Temperature = Celcius { getVal :: Float }
                 | Fahrenheit { getVal :: Float } deriving (Show)

instance Eq Temperature where
        (Celcius a) == (Celcius b) = abs (a - b) < 0.1
        (Fahrenheit a) == (Fahrenheit b) = abs (a - b) < 0.1
        (Fahrenheit a) == (Celcius b) = abs (b - fahrenheitToCelcius a) < 0.5
        (Celcius a) == (Fahrenheit b) = abs (a - fahrenheitToCelcius b) < 0.5

instance Ord Temperature where
        compare (Celcius a) (Celcius b) = compare a b
        compare (Fahrenheit a) (Fahrenheit b) = compare a b
        compare (Fahrenheit a) (Celcius b) = compare (fahrenheitToCelcius a) b
        compare (Celcius a) (Fahrenheit b) = compare a (fahrenheitToCelcius b)

fahrenheitToCelcius :: Float -> Float
fahrenheitToCelcius f = (f - 32.0) / 1.8

data DayStmt = DayStmt [Temperature] deriving (Show, Eq)
morning :: DayStmt -> Temperature
morning (DayStmt ts) = head ts
evening :: DayStmt -> Temperature
evening (DayStmt ts) = head $ tail ts
data WeekStmt = WeekStmt [DayStmt] deriving (Show, Eq)
data MonthStmt = MonthStmt [WeekStmt] deriving (Show, Eq)

makeDayStmt :: [Temperature] -> [DayStmt]
makeDayStmt = map DayStmt . group 2

makeWeekStmt :: [DayStmt] -> [WeekStmt]
makeWeekStmt = map WeekStmt . group 7

makeMonthStmt :: [WeekStmt] -> [MonthStmt]
makeMonthStmt = map MonthStmt . group 4

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = take n l : group n (drop n l)

data Statistics = Statistics { cardinal :: Int
                             , coldest :: Temperature
                             , hotest :: Temperature
                             , celciusAcc :: Temperature
                             , fahrenheitAcc :: Temperature
                             } deriving (Show, Eq)

temperatureToStatistics :: Temperature -> Statistics
temperatureToStatistics t@(Fahrenheit _) = Statistics 1 t t (Celcius 0) t
temperatureToStatistics t@(Celcius _) = Statistics 1 t t t (Fahrenheit 0)

avg :: Statistics -> Temperature
avg s = Celcius $ (getVal (celciusAcc  s) + fahrenheitToCelcius (getVal (fahrenheitAcc  s))) / fromIntegral (cardinal s)

instance Monoid Statistics where
        mempty = Statistics 0 (Celcius 1000.0) (Celcius (-1000.0)) (Celcius 0.0) (Fahrenheit 32.0)
        mappend (Statistics a b c (Celcius d) (Fahrenheit e)) (Statistics a' b' c' (Celcius d') (Fahrenheit e')) = Statistics (a + a') (min b b') (max c c') (Celcius (d + d')) (Fahrenheit (e + e'))

type FilterPosition = Int -> Bool
filterStats :: FilterPosition -> [a] -> [a]
filterStats f = map snd . filter (f . fst) . zip [1..]

-- fmap (filter ...) -- filter month
-- fmap . fmap -- filter weeks
-- fmap . fmap . fmap -- filter day
-- make*/unmake* => comonad/typeclass
-- Flattable
-- Foldable
