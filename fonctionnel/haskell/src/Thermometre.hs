{-# Language TemplateHaskell #-}

module Thermometre (
          Temperature(..)
        , DayStmt(..), makeDayStmt
        , WeekStmt(..), makeWeekStmt
        , MonthStmt(..), makeMonthStmt
        , Statistics(..)
        , temperatureToStatistics
        , avg
        , montlyEvenWeeklyMondayMorningStats
        )  where

import Data.Monoid
import Data.Foldable
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

temperatureToStatistics :: Temperature -> Statistics
temperatureToStatistics t@(Fahrenheit _) = Statistics 1 t t (Celcius 0) t
temperatureToStatistics t@(Celcius _) = Statistics 1 t t t (Fahrenheit 0)

avg :: Statistics -> Temperature
avg s = Celcius $ (view value (celciusAcc  s) + fahrenheitToCelcius (view value (fahrenheitAcc  s))) / fromIntegral (cardinal s)

instance Monoid Statistics where
        mempty = Statistics 0 (Celcius 1000.0) (Celcius (-1000.0)) (Celcius 0.0) (Fahrenheit 32.0)
        mappend (Statistics a b c (Celcius d) (Fahrenheit e)) (Statistics a' b' c' (Celcius d') (Fahrenheit e')) = Statistics (a + a') (min b b') (max c c') (Celcius (d + d')) (Fahrenheit (e + e'))

montlyEvenWeeklyMondayMorningStats :: [MonthStmt] -> Statistics
montlyEvenWeeklyMondayMorningStats = fold . map temperatureToStatistics . mapMaybe mondayMornings . evenWeeks
    where evenWeeks w = catMaybes $ [view second, view forth] <*> w
          mondayMornings d = view monday d >>= view morning

