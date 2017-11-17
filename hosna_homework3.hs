{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- CITE SOURCES: he won't murder us, just cite it damn it; cite > plagiarism
--			don't make him look ("more") like an asshole in front
--			of chadhy and billy

module Homework3 where
import Test.Hspec
import Control.Exception (evaluate) -- `shouldThrow` anyError
import RPNAST

-- notes: lexing (tokenizing), words (tokenizes by white space), read and pattern-matching,
--		monadically: sequence of applications that eat up lines of code to do read
-- description:
prob1 :: String -> PExp
prob1 x = prob1' (words x)

prob1' :: [String] -> PExp
prob1' [] = []
prob1' (" ":xs)   = prob1' xs
prob1' ("+":xs)   = Plus:(prob1' xs)
prob1' ("-":xs)   = Minus:(prob1' xs)
prob1' ("*":xs)   = Mul:(prob1' xs)
prob1' ("/":xs)   = IntDiv:(prob1' xs)
prob1' (x:xs)     = (Val (read x :: Int)):(prob1' xs)

-- notes: PExp is list of Ops, use stacks -> popping off, dynamic and static errors,
--		pattern match for good cases (exhaustive) [total#(op) = total#(val) - 1],
--		accumulator passing style -> list that handles processed ops, and one
--		that handles unprocessed ops; apparently should be worked on as group
-- description:
prob2 :: PExp -> Int
prob2 ((Val v):[])               = v --[2]
prob2 (x:[])                     = error "Bad syntax!" --[*]
prob2 (x:y:xs)
       | (xs == [])              = error "Needs an operator!" --prevents [2,2]
       | (isVal x) && (isVal y)  = prob2_aps (y:xs) [x]  --only if [2,2,xs]
       | otherwise               = error "Must begin w/ two Vals!" --prevents [2,+] and [+, xs]

--helper function
prob2_aps :: PExp -> PExp -> Int
prob2_aps ((Val x):[]) (ys)  = error "Needs to end w/ an operator!" --prevents [2] [ys] (needs to end in op)
prob2_aps (x:xs) (y:[])
        | (isVal x) = prob2_aps xs (x:y:[])  --if [2, xs] [2], makes [xs] [2, 2]
        | otherwise = error "Bad Syntax!" --prevents [+, xs] [2] (needs two nums to operate)
prob2_aps (x:[]) (y:z:ys)  -- last input, must be evaluated
        | (x == Plus)    = case (y, z) of  --[+] [2, 2]
                              (Val y', Val z') -> (z' + y')
        | (x == Minus)   = case (y, z) of
                              (Val y', Val z') -> (z' - y')
        | (x == Mul)     = case (y, z) of
                              (Val y', Val z') -> (z' * y')
        | (x == IntDiv)  = case (y, z) of
                              (Val 0, Val z')   -> error "Cannot divide by 0!"
                              (Val y', Val z') -> (z' `div` y')
        | otherwise      = error "Needs to end w/ an operator!"      --prevents [2] [ys]
-- if number move on else do operation
prob2_aps (x:xs) (y:z:ys)
        | (x == Plus)   = case (y, z) of
                            (Val y', Val z') -> prob2_aps xs ((Val (z' + y')):ys)
        | (x == Minus)  = case (y, z) of
                            (Val y', Val z') -> prob2_aps xs ((Val (z' - y')):ys)
        | (x == Mul)    = case (y, z) of
                            (Val y', Val z') -> prob2_aps xs ((Val (z' * y')):ys)
        | (x == IntDiv)    = case (y, z) of
                            (Val 0, Val z')  -> error "Cannot divide by 0!"
                            (Val y', Val z') -> prob2_aps xs ((Val (z' `div` y')):ys)
        | otherwise     = prob2_aps xs (x:y:z:ys)
isVal :: Op -> Bool
isVal x = if (x `elem` [Plus, Minus, Mul, IntDiv])
          then False
          else True
{-
  -- regular factorial, sucks mad dong
  fact :: Int -> Int
  fact 0 = 1
  fact n = n * fact (n - 1)

  -- fancy new factorial, uses accumulator processing style
  -- keeps track of running value by passing it as parameter(uses eager evaluation)
  fact_aps :: Int -> Int -> Int
  fact_aps 0 a = a
  fact_aps n a = fact_aps (n - 1) (n * a)
  -- primer function
  factorial :: Int -> Int
  factorial n = fact_aps n 1
-}
-- notes: if prob2 is exhaustive -> can be changed slightly to be prob3
-- description:
prob3 :: PExp -> RPNResult
prob3 ((Val v):[])               = Success v --[2]
prob3 (x:[])                     = Failure BadSyntax --[*]
prob3 (x:y:xs)
       | (xs == [])              = Failure BadSyntax --prevents [2,2]
       | (isVal x) && (isVal y)  = prob3_aps (y:xs) [x]  --only if [2,2,xs]
       | otherwise               = Failure BadSyntax --prevents [2,+] and [+, xs]

--helper function
prob3_aps :: PExp -> PExp -> RPNResult
prob3_aps ((Val x):[]) (ys)  = Failure BadSyntax --prevents [2] [ys] (needs to end in op)
prob3_aps (x:xs) (y:[])
        | (isVal x) = prob3_aps xs (x:y:[])  --if [2, xs] [2], makes [xs] [2, 2]
        | otherwise = Failure BadSyntax --prevents [+, xs] [2] (needs two nums to operate)
prob3_aps (x:[]) (y:z:ys)  -- last input, must be evaluated
        | (x == Plus)    = case (y, z) of  --[+] [2, 2]
                              (Val y',Val z') -> Success (z' + y')
        | (x == Minus)   = case (y, z) of
                              (Val y',Val z') -> Success (z' - y')
        | (x == Mul)     = case (y, z) of
                              (Val y',Val z') -> Success (z' * y')
        | (x == IntDiv)  = case (y, z) of
                              (Val 0, Val z')  -> Failure DivByZero
                              (Val y', Val z') -> Success (z' `div` y')
        | otherwise      = Failure BadSyntax      --prevents [2] [ys]
-- if number move on else do operation
prob3_aps (x:xs) (y:z:ys)
        | (x == Plus)   = case (y, z) of
                            (Val y',Val z') -> prob3_aps xs ((Val (z' + y')):ys)
        | (x == Minus)  = case (y, z) of
                            (Val y',Val z') -> prob3_aps xs ((Val (z' - y')):ys)
        | (x == Mul)    = case (y, z) of
                            (Val y',Val z') -> prob3_aps xs ((Val (z' * y')):ys)
        | (x == IntDiv) = case (y, z) of
                            (Val 0,Val z')  -> Failure DivByZero
                            (Val y',Val z') -> prob3_aps xs ((Val (z' `div` y')):ys)
        | otherwise     = prob3_aps xs (x:y:z:ys)

-- notes: use stacks, apparently will cause the most difficulty
{- built off of class notes: Thank you, Mr. Reynolds

evalRPN :: PExp -> Int
evalRPN ops = go' ops []
  where go' (Plus:rest)   (r:l:vals) = go' rest ((l + r):vals) --l and r = strings
        go' ((Val i):[]) vals        = error "THIS DON't WORK"
        go' ((Val i):rest) vals      = go' rest (i:vals)
        go' (Mul:rest) (r:l:vals)    = undefined
        go' (Minus:rest) (r:l:vals)  = undefined
        go' (IntDiv:rest) (0:l:vals) = undefined
        go' (IntDiv:rest) (r:l:vals) = undefined
        go' [] [i]                   = i
        go' _ _                      = undefined

-}
-- description: returns a string of an equivalent expression in infix notation w/ parenthesis
prob4 :: PExp -> Result String String
prob4 os = aps os []
  where aps [] (r:[])                         = Success r
        aps [] (r:l:[])                       = Failure "Bad Input"
        aps ((Val i):(Val i'):[]) vals        = Failure "Bad Input"
        aps ((Val i):rest) (vals)             = aps rest ((show i):vals)
        aps (Plus:rest) (r:l:vals)            = aps rest (("(" ++ l ++ " + " ++ r ++ ")"):vals)
        aps (Mul:rest) (r:l:vals)             = aps rest (("(" ++ l ++ " * " ++ r ++ ")"):vals)
        aps (Minus:rest) (r:l:vals)           = aps rest (("(" ++ l ++ " - " ++ r ++ ")"):vals)
        aps (IntDiv:rest) ("0":l:vals)        = Failure "DivByZero"
        aps (IntDiv:rest) (r:l:vals)          = aps rest (("(" ++ l ++ " / " ++ r ++ ")"):vals)
        aps _ _                               = Failure "Bad Input"

-- Write your Hspec Tests below
-- note: need at least 1 more than the examples given in the pdf
--		please also try to be thorough

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

    -- note: there should be a bad case for bad input other than ""

test_prob2 :: IO ()
test_prob2 = hspec $ do
  describe "Prob2 from HW3" $ do

    context "For [Val 4, Val 2, IntDiv]" $ do
      it "should return 2" $ do
        prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2

    context "For [Val 10, Val 2, Mul]" $ do
      it "should return 20" $ do
        prob2 [Val 10, Val 2, Mul] `shouldBe` 20

    context "For [Val 15, Val 7, Plus]" $ do
      it "should return 22" $ do
        prob2 [Val 15, Val 7, Plus] `shouldBe` 22

    context "For [Val 100, Val 25, Minus]" $ do
      it "should return 75" $ do
        prob2 [Val 100, Val 25, Minus] `shouldBe` 75
    -- note: input with incorrect number of vals and ops

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

    context "For [Val 4, Val 2, IntDiv]" $ do
      it "should return 2" $ do
        prob3 [Val 4, Val 2, IntDiv] `shouldBe` Success 2

    context "For [Val 10, Val 2, Mul]" $ do
      it "should return 20" $ do
        prob3 [Val 10, Val 2, Mul] `shouldBe` Success 20

    context "For [Val 15, Val 7, Plus]" $ do
      it "should return 22" $ do
        prob3 [Val 15, Val 7, Plus] `shouldBe` Success 22

    context "For [Val 100, Val 25, Minus]" $ do
      it "should return 75" $ do
        prob3 [Val 100, Val 25, Minus] `shouldBe` Success 75

    -- note: see note for test_prob2

test_prob4 :: IO ()
test_prob4 = hspec $ do
  describe "Prob4 from HW4" $ do
    context "For [Val 1, Val 1, Plus]" $ do
      it "should return Success \"(1 + 1)\"" $ do
        prob4 [Val 1, Val 1, Plus] `shouldBe` Success "(1 + 1)"

    context "For [Val 2, Val 4, Plus, Val 3, IntDiv]" $ do
      it "should return Success \"((2 + 4) / 3)\"" $ do
        prob4 [Val 2, Val 4, Plus, Val 3, IntDiv] `shouldBe` Success "((2 + 4) / 3)"

    context "For [Val 3, Val 8, Minus, Val 6, Mul]" $ do
      it "should return Success \"((3 - 8) * 6)\"" $ do
        prob4 [Val 3, Val 8, Minus, Val 6, Mul] `shouldBe` Success "((3 - 8) * 6)"

    context "For [Val 2]" $ do
      it "should return Success \"2\"" $ do
        prob4 [Val 2] `shouldBe` Success "2"

    context "For [Plus]" $ do
      it "should return Failure \"Bad Input.\"" $ do
        prob4 [Plus] `shouldBe` Failure "Bad Input"

test_probs :: IO ()
test_probs = do
  putStrLn "-------- All Problem Results --------"
  test_prob1
  test_prob2
  test_prob3
  test_prob4
  putStrLn "-------------------------------------"
