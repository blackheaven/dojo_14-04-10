{-# Language TemplateHaskell #-}

module Thermometre (
          Temperature(..)
        , DayStmt(..), makeDayStmt
        , WeekStmt(..), makeWeekStmt
        , MonthStmt(..), makeMonthStmt
        , Statistics(..)
        , makeStatistics
        , avg
        , weeklyStats
        , monthlyStats
        , getStats
        , allTemperatures
        , mornings
        , evenings
        , mondays
        , evenWeeks
        , evenWeeksMondayMornings
        )  where

import Data.Monoid
import Data.Maybe
import Data.List (intersect)
import Control.Monad
import Control.Applicative

data Temperature = Celcius { value :: Float }
                 | Fahrenheit { value :: Float } deriving (Show)


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

data DayStmt = DayStmt { morning :: Maybe Temperature
                       , evening :: Maybe Temperature
                       } deriving (Show, Eq)

data WeekStmt = WeekStmt { monday    :: Maybe DayStmt
                         , thuesday  :: Maybe DayStmt
                         , wednesday :: Maybe DayStmt
                         , thursday  :: Maybe DayStmt
                         , friday    :: Maybe DayStmt
                         , saturday  :: Maybe DayStmt
                         , sunday    :: Maybe DayStmt
                         } deriving (Show, Eq)

data MonthStmt = MonthStmt { first  :: Maybe WeekStmt
                           , second :: Maybe WeekStmt
                           , third  :: Maybe WeekStmt
                           , forth  :: Maybe WeekStmt
                           } deriving (Show, Eq)

makeDayStmt :: [Temperature] -> [DayStmt]
makeDayStmt = map mk . group 2
    where mk g = DayStmt (g !! 0) (g !! 1)

makeWeekStmt :: [DayStmt] -> [WeekStmt]
makeWeekStmt = map mk . group 7
    where mk g = WeekStmt (g !! 0) (g !! 1) (g !! 2) (g !! 3) (g !! 4) (g !! 5) (g !! 6)

makeMonthStmt :: [WeekStmt] -> [MonthStmt]
makeMonthStmt = map mk . group 4
    where mk g = MonthStmt (g !! 0) (g !! 1) (g !! 2) (g !! 3)

group :: Int -> [a] -> [[Maybe a]]
group _ [] = []
group n l = ((map Just $ take n l) ++ replicate n Nothing) : group n (drop n l)

data Statistics = Statistics { cardinal :: Int
                             , coldest :: Temperature
                             , hotest :: Temperature
                             , celciusAcc :: Temperature
                             , fahrenheitAcc :: Temperature
                             } deriving (Show, Eq)

makeStatistics :: [Temperature] -> Statistics
makeStatistics d = Statistics (length d) (minimum d) (maximum d) cAcc fAcc
    where (cAcc, fAcc) = foldl dispatchAcc (Celcius 0.0, Fahrenheit 32.0) d
          dispatchAcc (Celcius c, f) (Celcius n) = (Celcius (c + n), f)
          dispatchAcc (c, Fahrenheit f) (Fahrenheit n) = (c, Fahrenheit (f + n))

avg :: Statistics -> Temperature
avg s = Celcius $ (value (celciusAcc s) + fahrenheitToCelcius (value (fahrenheitAcc s))) / fromIntegral (cardinal s)

-- Extractor
data Extractor = Extractor (MonthStmt -> [WeekStmt]) (WeekStmt -> [DayStmt]) (DayStmt -> [Temperature])

instance Monoid Extractor where
        mempty = allTemperatures
        mappend (Extractor w1 d1 t1) (Extractor w2 d2 t2) = Extractor (\ws -> w1 ws `unif` w2 ws) (\ds -> d1 ds `unif` d2 ds) (\ts -> t1 ts `unif` t2 ts)
            where unif a b
                    | length a < length b = a `intersect` b
                    | otherwise           = b `intersect` a

extract :: Extractor -> [MonthStmt] -> [Temperature]
extract (Extractor m w d) = concatMap d . concatMap w . concatMap m

allWeeksF :: MonthStmt -> [WeekStmt]
allWeeksF w = catMaybes $ [first, second, third, forth] <*> pure w

allDaysF :: WeekStmt -> [DayStmt]
allDaysF d = catMaybes $ [monday, thuesday, wednesday, thursday, friday, saturday, sunday] <*> pure d

allTemperaturesF :: DayStmt -> [Temperature]
allTemperaturesF t = catMaybes $ [morning, evening] <*> pure t

-- Groupers
getStats :: Extractor -> [MonthStmt] -> Statistics
getStats f = makeStatistics . extract f

weeklyStats :: Extractor -> [MonthStmt] -> [Statistics]
weeklyStats e d = map ((\f -> getStats f d) . filterByMonth) [first, second, third, forth]
    where filterByMonth m = Extractor (\w -> catMaybes [m w]) allDaysF allTemperaturesF `mappend` e

monthlyStats :: Extractor -> [MonthStmt] -> [Statistics]
monthlyStats e = map (\m -> getStats e [m])

-- Extractors implementations
allTemperatures :: Extractor
allTemperatures = Extractor allWeeksF allDaysF allTemperaturesF

mornings :: Extractor
mornings = Extractor allWeeksF allDaysF filterTemperatures
    where filterTemperatures t = maybeToList $ morning t

evenings :: Extractor
evenings = Extractor allWeeksF allDaysF filterTemperatures
    where filterTemperatures t = maybeToList $ evening t

mondays :: Extractor
mondays = Extractor allWeeksF filterDays allTemperaturesF
    where filterDays d = maybeToList $ monday d

evenWeeks :: Extractor
evenWeeks = Extractor filterWeeks allDaysF allTemperaturesF
    where filterWeeks d = catMaybes [second d, forth d]

evenWeeksMondayMornings :: Extractor
evenWeeksMondayMornings = evenWeeks `mappend` mondays `mappend` mornings

