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
            describe "Given both Celcius values" $ do
                it "when values are the same then should be equal" $ do
                    Celcius 2.0 `shouldBe` Celcius 2.0
                it "when values are different then should be different" $ do
                    (Celcius 4.0 == Celcius 2.0) `shouldBe` False
            describe "Given both Fahrenheit values" $ do
                it "when values are the same then should be equal" $ do
                    Fahrenheit 2.0 `shouldBe` Fahrenheit 2.0
                it "when values are different then should be different" $ do
                    (Fahrenheit 4.0 == Fahrenheit 2.0) `shouldBe` False
            describe "Given Fahrenheit then Celcius values" $ do
                it "when values are equivalent then should be equal" $ do
                    Fahrenheit 100.0 `shouldBe` Celcius 38.0
                it "when values are not equivalent then should be different" $ do
                    (Fahrenheit 100.0 == Celcius 2.0) `shouldBe` False
            describe "Given Celcius then Fahrenheit values" $ do
                it "when values are equivalent then should be equal" $ do
                    Celcius 38.0 `shouldBe` Fahrenheit 100.0
                it "when values are not equivalent then should be different" $ do
                    (Celcius 2.0 == Fahrenheit 100.0) `shouldBe` False

    describe "montlyEvenWeeklyMondayMorningStats" $ do
        describe "celcius" $ do
            it "Given 2 months minus one week when all morning have 1°C then should be equal to 3°C" $ do
                celciusAcc (montlyEvenWeeklyMondayMorningStats . makeMonthStmt
                           . makeWeekStmt $ replicate 49 (DayStmt (Just $ Celcius 1.0)
                           (Just $ Celcius 42.0))) `shouldBe` Celcius 3.0
