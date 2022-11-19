module WordleSpec where

import Test.Hspec

spec :: Spec
spec = describe "When I have tests" $ it "I have sanity" $ True `shouldBe` True

main :: IO ()
main = putStrLn "Running the tests..."
