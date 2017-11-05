module HSpecExamples where
import HomeworkSols
import Test.Hspec
--import Test.QuickCheck
--import Control.Exception (evaluate)

-- cabal update && cabal install hspec

-- prob1 :: Char -> Char
          
test_prob1 = hspec $ do
  describe "Prob1 from HWK1" $ do
    context "For \'a\' .. \'y\'" $ do
      it "should return the next letter" $ do
        map (\x -> prob1 x) ['a'..'y'] `shouldBe` ['b'..'z']

    context "For \'0\' .. \'9\'" $ do
      it "should return the input unchanged" $ do
        map (\x -> prob1 x) "0123456789" `shouldBe` "0123456789"

    context "For \'z\' and \'Z\'" $ do
      it "should return \'a\' and \'A\'" $ do
        map (prob1) ['z','Z'] `shouldBe` "aA"

    context "Anything else" $ do
      it "should return input unchanged" $ do
        map (prob1) "\\n\\r\\s\\t" `shouldSatisfy` (not . null)


test_prob2 = hspec $ do
  describe "Prob2 from HWK1" $ do
    context "For \'0\' .. \'9\'" $ do
      it "should return [0 .. 9]" $ do
        map (prob2) ['0'..'9'] `shouldBe` [0..9]

    context "FOR BAD STUFF" $ do
      it "should return -1" $ do
        map (prob2) ['a'..'c'] `shouldBe` [(-1),(-1),(-1)]
        

































-- Basic HSpec Usage
-- Silly function that returns a constant value of 1
basic_use_case :: Num n => i -> n
basic_use_case _  = 1
-- using `shouldBe`
basic_test1 :: IO ()
basic_test1 = hspec $ do
  describe "basic_use_case1" $ do
    context "when ..." $ do
      it "does ..." $ do
       \x -> (basic_use_case x) `shouldBe` (1 :: Int)

-- using `shouldSatisfy`
basic_test2 :: IO ()
basic_test2 = hspec $ do
  describe "basic_use_case2" $ do
    context "when ..." $ do
      it "does ..." $ do
        map (basic_use_case) [1..100] `shouldSatisfy` (not . null)

-- Combine Simple Test Easily
combined_basic_tests :: IO ()
combined_basic_tests = hspec $ do
  describe "basic_use_cases" $ do
    context "Using ShouldBe" $ do
      it "does ..." $ do
       \x -> (basic_use_case x) `shouldBe` (1 :: Int)

    context "Using ShouldSatisfy" $ do
      it "does ..." $ do
        map (basic_use_case) [1..100] `shouldSatisfy` (not . null)
      


-- Basic HSpec Tests for Error handling
error_use_case _ = errorWithoutStackTrace "Undefined"

error_test1 :: IO ()
error_test1 = hspec $ do
  describe "error_use_case" $ do
    context "Errors Expected" $ do
      it "throws an error if used" $ do
      \x -> (error_use_case x) `shouldThrow` anyErrorCall


-- Intermediate HSpec Usage for QuickCheck
intermediate_use_case :: [a] -> Maybe a
intermediate_use_case []    = Nothing
intermediate_use_case (x:_) = Just x

intermediate_test1 :: IO ()
intermediate_test1 = hspec $ do
  describe "intermediate_use_case" $ do
    context "when ..." $ do
      it "does ..." $
        property $
        \x -> (intermediate_use_case x) == (Just (head x :: Int)) ||
              (intermediate_use_case x) == Nothing


