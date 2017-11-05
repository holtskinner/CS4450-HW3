{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Homework3 where
import Test.Hspec
import RPNAST


prob1 :: String -> PExp
prob1 _ = []

prob2    :: a
prob2    = undefined

prob3    :: a
prob3    = undefined

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
test_prob1 :: IO ()

test_prob1 = hspec $ do
  describe "Prob1 from HW3" $ do
    context "For empty string" $ do
      it "should return the empty list" $ do
        prob1 "" `shouldBe` []

    context "For \"200 + - * /\"" $ do
      it "should return \'[Val 200, Plus, Minus, Mul, IntDiv]\'" $ do
        prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]