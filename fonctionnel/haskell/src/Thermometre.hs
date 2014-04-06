{-# Language TemplateHaskell #-}

module Thermometre (
          Temperature(..)
        , DayStmt(..), makeDayStmt
        , WeekStmt(..), makeWeekStmt
        , MonthStmt(..), makeMonthStmt
        , Statistics(..)
        , makeStatistics
        , avg
        , morningStats
        , eveningStats
        , weeklyStats
        , monthlyStats
        , montlyEvenWeeklyMondayMorningStats
        )  where

import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Lens
import Control.Lens.Cons

data Temperature = Celcius { _value :: Float }
                 | Fahrenheit { _value :: Float } deriving (Show)

makeLenses ''Temperature

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

data DayStmt = DayStmt { _morning :: Maybe Temperature
                       , _evening :: Maybe Temperature
                       } deriving (Show, Eq)
makeLenses ''DayStmt

data WeekStmt = WeekStmt { _monday    :: Maybe DayStmt
                         , _thuesday  :: Maybe DayStmt
                         , _wednesday :: Maybe DayStmt
                         , _thursday  :: Maybe DayStmt
                         , _friday    :: Maybe DayStmt
                         , _saturday  :: Maybe DayStmt
                         , _sunday    :: Maybe DayStmt
                         } deriving (Show, Eq)
makeLenses ''WeekStmt

data MonthStmt = MonthStmt { _first  :: Maybe WeekStmt
                           , _second :: Maybe WeekStmt
                           , _third  :: Maybe WeekStmt
                           , _forth  :: Maybe WeekStmt
                           } deriving (Show, Eq)
makeLenses ''MonthStmt

makeDayStmt :: [Temperature] -> [DayStmt]
makeDayStmt = map mk . group 2
    where mk g = DayStmt (g ^? _head) (g ^? _tail . _head)

makeWeekStmt :: [DayStmt] -> [WeekStmt]
makeWeekStmt = map mk . group 7
    where mk g = WeekStmt (g ^? _head) (g ^? _tail . _head) (g ^? _tail . _tail . _head) (g ^? _tail . _tail . _tail . _head) (g ^? _tail . _tail . _tail . _tail . _head) (g ^? _tail . _tail . _tail . _tail . _tail . _head) (g ^? _tail . _tail . _tail . _tail . _tail . _tail . _head)

makeMonthStmt :: [WeekStmt] -> [MonthStmt]
makeMonthStmt = map mk . group 4
    where mk g = MonthStmt (g ^? _head) (g ^? _tail . _head) (g ^? _tail . _tail . _head) (g ^? _tail . _tail . _tail . _head)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = take n l : group n (drop n l)

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
avg s = Celcius $ (view value (celciusAcc  s) + fahrenheitToCelcius (view value (fahrenheitAcc  s))) / fromIntegral (cardinal s)

data Extractor = Extractor (MonthStmt -> [WeekStmt]) (WeekStmt -> [DayStmt]) (DayStmt -> [Temperature])

extract :: Extractor -> [MonthStmt] -> [Temperature]
extract (Extractor m w d) = concatMap d . concatMap w . concatMap m

getStats :: Extractor -> [MonthStmt] -> Statistics
getStats f = makeStatistics . extract f

allWeeks :: MonthStmt -> [WeekStmt]
allWeeks w = catMaybes [view first w, view second w, view third w, view forth w]
allDays :: WeekStmt -> [DayStmt]
allDays d = catMaybes [view monday d, view thuesday d, view wednesday d, view thursday d, view friday d, view saturday d, view sunday d]
allTemperatures :: DayStmt -> [Temperature]
allTemperatures t = catMaybes [view morning t, view evening t]

morningStats :: [MonthStmt] -> Statistics
morningStats = getStats (Extractor allWeeks allDays filterDays)
    where filterDays t = catMaybes [view morning t]

eveningStats :: [MonthStmt] -> Statistics
eveningStats = getStats (Extractor allWeeks allDays filterDays)
    where filterDays t = catMaybes [view evening t]

weeklyStats :: [MonthStmt] -> [Statistics]
weeklyStats d = map (\f -> getStats f d) $ map (\m -> Extractor (\w -> catMaybes [view m w]) allDays allTemperatures) [first, second, third, forth]

monthlyStats :: [MonthStmt] -> [Statistics]
monthlyStats = map (\m -> getStats (Extractor allWeeks allDays allTemperatures) [m])

montlyEvenWeeklyMondayMorningStats :: [MonthStmt] -> Statistics
montlyEvenWeeklyMondayMorningStats = getStats (Extractor filterMonths filterWeeks filterDays)
    where filterMonths w = catMaybes [view second w, view forth w]
          filterWeeks d = catMaybes [view monday d]
          filterDays t = catMaybes [view morning t]

