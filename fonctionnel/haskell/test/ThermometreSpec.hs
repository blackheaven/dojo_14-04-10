module ThermometreSpec (main, spec) where

import Test.Hspec
import Thermometre
import Data.Foldable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Temperature datatype" $ do
        describe "Eq instance" $ do
            describe "Both Celcius" $ do
                it "same" $ do
                    Celcius 2.0 `shouldBe` Celcius 2.0
                it "different" $ do
                    (Celcius 4.0 == Celcius 2.0) `shouldBe` False
            describe "Both Fahrenheit" $ do
                it "same" $ do
                    Fahrenheit 2.0 `shouldBe` Fahrenheit 2.0
                it "different" $ do
                    (Fahrenheit 4.0 == Fahrenheit 2.0) `shouldBe` False
            describe "Fahrenheit then Celius" $ do
                it "same" $ do
                    Fahrenheit 100.0 `shouldBe` Celcius 38.0
                it "different" $ do
                    (Fahrenheit 100.0 == Celcius 2.0) `shouldBe` False
            describe "Celius then Fahrenheit" $ do
                it "same" $ do
                    Celcius 38.0 `shouldBe` Fahrenheit 100.0
                it "different" $ do
                    (Celcius 2.0 == Fahrenheit 100.0) `shouldBe` False

    describe "DayStmt datatype" $ do
        it "morning" $ do
            morning (DayStmt [(Celcius 2.0), (Celcius 4.0)]) `shouldBe` Celcius 2.0
        it "evening" $ do
            evening (DayStmt [(Celcius 2.0), (Celcius 4.0)]) `shouldBe` Celcius 4.0

    describe "makeDayStmt" $ do
        it "one day" $ do
            makeDayStmt [(Celcius 2.0), (Celcius 4.0)] `shouldBe` [DayStmt [(Celcius 2.0), (Celcius 4.0)]]
        it "two days" $ do
            makeDayStmt [(Celcius 2.0), (Celcius 4.0)
                        , (Celcius 3.0), (Celcius 6.0)] `shouldBe` [
                          DayStmt [(Celcius 2.0), (Celcius 4.0)]
                        , DayStmt [(Celcius 3.0), (Celcius 6.0)]]

    describe "makeWeekStmt" $ do
        it "one week" $ do
            makeWeekStmt (replicate 2 (DayStmt [])) `shouldBe` [WeekStmt [DayStmt [], DayStmt []]]
        it "two weeks" $ do
            makeWeekStmt (replicate 10 (DayStmt [])) `shouldBe` [
                        WeekStmt (replicate 7 (DayStmt [])), WeekStmt (replicate 3 (DayStmt []))]

    describe "makeMonthStmt" $ do
        it "one month" $ do
            makeMonthStmt (replicate 2 (WeekStmt [])) `shouldBe` [MonthStmt [WeekStmt [], WeekStmt []]]
        it "two months" $ do
            makeMonthStmt (replicate 7 (WeekStmt [])) `shouldBe` [
                        MonthStmt (replicate 4 (WeekStmt [])), MonthStmt (replicate 3 (WeekStmt []))]

    describe "Statistics" $ do
        it "coldest" $ do
            coldest (fold (map temperatureToStatistics [Celcius 2.0, Celcius 4.0])) `shouldBe` Celcius 2.0
        it "hotest" $ do
            hotest (fold (map temperatureToStatistics [Celcius 2.0, Celcius 4.0])) `shouldBe` Celcius 4.0
        it "avg" $ do
            avg (fold (map temperatureToStatistics [Celcius 2.0, Celcius 4.0])) `shouldBe` Celcius 3.0
        it "cardinal" $ do
            cardinal (fold (map temperatureToStatistics [Celcius 2.0, Celcius 4.0])) `shouldBe` 2
        it "celciusAcc" $ do
            celciusAcc (fold (map temperatureToStatistics [Celcius 2.0, Celcius 4.0])) `shouldBe` Celcius 6.0
        it "fahrenheitAcc" $ do
            fahrenheitAcc (fold (map temperatureToStatistics [Celcius 2.0, Celcius 4.0])) `shouldBe` Fahrenheit 32.0
