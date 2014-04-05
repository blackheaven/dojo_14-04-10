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
            describe "Fahrenheit then Celcius" $ do
                it "same" $ do
                    Fahrenheit 100.0 `shouldBe` Celcius 38.0
                it "different" $ do
                    (Fahrenheit 100.0 == Celcius 2.0) `shouldBe` False
            describe "Celcius then Fahrenheit" $ do
                it "same" $ do
                    Celcius 38.0 `shouldBe` Fahrenheit 100.0
                it "different" $ do
                    (Celcius 2.0 == Fahrenheit 100.0) `shouldBe` False

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

    describe "montlyEvenWeeklyMondayMorningStats" $ do
        it "Count 2 months" $ do
            celciusAcc (montlyEvenWeeklyMondayMorningStats . makeMonthStmt
                       . makeWeekStmt $ replicate 56 (DayStmt (Just $ Celcius 1.0)
                       (Just $ Celcius 42.0))) `shouldBe` Celcius 8.0
        it "card 2 months" $ do
            cardinal (montlyEvenWeeklyMondayMorningStats . makeMonthStmt
                       . makeWeekStmt $ replicate 56 (DayStmt (Just $ Celcius 1.0)
                       (Just $ Celcius 42.0))) `shouldBe` 8
