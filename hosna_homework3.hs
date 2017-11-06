{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Homework3 where
import Test.Hspec
import RPNAST


prob1 :: String -> PExp
prob1 _ = []

prob2 :: PExp -> Int
prob2 _ = 2

prob3 :: PExp -> RPNResult
prob3 _ = Failure DivByZero

prob4 :: PExp -> Result String String
prob4 _ = Failure "Bad Input."

-- Write your Hspec Tests below
test_prob1 :: IO ()
test_prob1 = hspec $ do
  describe "Prob1 from HW3" $ do
    context "For empty string" $ do
      it "should return the empty list" $ do
        prob1 "" `shouldBe` []

    context "For \"200 + - * /\"" $ do
      it "should return [Val 200, Plus, Minus, Mul, IntDiv]" $ do
        prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]

    context "For \"+ - * / 200\"" $ do
      it "should return [Plus, Minus, Mul, IntDiv, Val 200]" $ do
        prob1 "+ - * / 200" `shouldBe` [Plus, Minus, Mul, IntDiv, Val 200]


test_prob2 :: IO ()
test_prob2 = hspec $ do
  describe "Prob2 from HW3" $ do
    context "For [Val 4, Val 2, IntDiv]" $ do
      it "should return 2" $ do
        prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2


test_prob3 :: IO ()
test_prob3 = hspec $ do
  describe "Prob3 from HW3" $ do
    context "For [Val 5, Val 0, IntDiv]" $ do
      it "should return Failure DivByZero" $ do
        prob3 [Val 5, Val 0, IntDiv] `shouldBe` Failure DivByZero

    context "For [IntDiv, Plus, Val 0]" $ do
      it "should return Failure BadSyntax" $ do
        prob3 [IntDiv, Plus, Val 0] `shouldBe` Failure BadSyntax
    
    context "For [Val 5, Val 1, Val 1, Plus, Mul]" $ do
      it "should return Success 10" $ do
        prob3 [Val 5, Val 1, Val 1, Plus, Mul] `shouldBe` Success 10

test_prob4 :: IO ()
test_prob4 = hspec $ do
  describe "Prob4 from HW4" $ do
    context "For [Val 1, Val 1, Plus]" $ do
      it "should return Success \"(1 + 1)\"" $ do
        prob4 [Val 1, Val 1, Plus] `shouldBe` Success "(1 + 1)"
    
    context "For [Val 2, Val 4, Plus, Val 3, IntDiv]" $ do
      it "should return Success \"((2 + 4) / 3)\"" $ do
        prob4 [Val 2, Val 4, Plus, Val 3, IntDiv] `shouldBe` Success "((2 + 4) / 3)"

    context "For [Val 2]" $ do
      it "should return Success \"2\"" $ do
        prob4 [Val 2] `shouldBe` Success "2"
    
    context "For [Plus]" $ do
      it "should return Failure \"Bad Input.\"" $ do
        prob4 [Plus] `shouldBe` Failure "Bad Input."

test_probs :: IO ()
test_probs = do
  putStrLn "-------- All Problem Results --------"
  test_prob1
  test_prob2
  test_prob3
  test_prob4
  putStrLn "-------------------------------------"