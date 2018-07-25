{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec

main :: IO ()
main = putStrLn "Hello, Haskell!"

runTests :: IO ()
runTests = hspec $ do
    describe "am" $ do
        it "am to am2" $
            fixFromPos 0 am `shouldBe` am2
        it "am2 to am3" $
            fixFromPos 1 am2 `shouldBe` am3
    describe "m" $ do
        it "m to m2" $
            fixFromPos 0 m `shouldBe` m2
        it "m2 to m3" $
            fixFromPos 1 m2 `shouldBe` m3

am = AM
    [ Equation [1,-1,-1,1]   0
    , Equation [2,0,2,0]     8
    , Equation [0,-1,-2,0] (-8)
    , Equation [3,-3,-2,4]   7 ]

am2 = AM
    [ Equation [1,-1,-1,1]   0
    , Equation [0,2,4,-2]    8
    , Equation [0,-1,-2,0] (-8)
    , Equation [0,0,1,1 ]    7 ]

am3 = AM
    [ Equation [1,-1,-1,1]   0
    , Equation [0,2,4,-2]    8
    , Equation [0,0,0,-1]  (-4)
    , Equation [0,0,1,1 ]    7 ]

m = AM
    [ Equation [1,3,1]     9
    , Equation [1,1,-1]    1
    , Equation [3,11,5]   35 ]

m2 = AM
    [ Equation [1,3,1]     9
    , Equation [0,-2,-2] (-8)
    , Equation [0,2,2]     8 ]

m3 = AM
    [ Equation [1,3,1]     9
    , Equation [0,-2,-2] (-8)
    , Equation [0,0,0]     0 ]

type Numbertype = Rational

type Pos = Int

data Equation = Equation
  { lhs :: [Numbertype]
  , rhs :: Numbertype } deriving (Eq, Show)

data AM = AM
  { equations :: [Equation] } deriving (Eq, Show)

data Vertical = Vertical
  { val :: Numbertype
  , vertVals :: [Numbertype] }

type DoubleValue = Numbertype

data DoubleEquation = DoubleEquation Equation Equation DoubleValue deriving Show

solution :: AM -> [Numbertype]
solution am = map rhs $ equations $ fixAM am

fixAM :: AM -> AM
fixAM am = foldr fixFromPos am $ getStartPositions am

getStartPositions :: AM -> [Pos]
getStartPositions am = [0..length $ equations am]

fixFromPos :: Pos -> AM -> AM
fixFromPos pos am =
    let doubles :: [DoubleEquation] = getDoubles pos am
        newEquations :: [Equation] = map calcEquation doubles
        in appendEquations newEquations pos am

getDoubles :: Pos -> AM -> [DoubleEquation]
getDoubles pos am =
    let currentEquation = getCurrentEquation pos am
        -- Ignore already calculated equations
        relevant = drop (pos + 1) $ equations am
        in map (\n -> DoubleEquation currentEquation n $ newValue currentEquation n pos) relevant

getCurrentEquation :: Pos -> AM -> Equation
getCurrentEquation pos am = equations am !! pos

newValue :: Equation -> Equation -> Pos -> DoubleValue
newValue e1 e2 pos =
    let v1 = lhs e1 !! pos
        v2 = lhs e2 !! pos
        in v2 / (-v1)

calcEquation :: DoubleEquation -> Equation
calcEquation (DoubleEquation e1 e2 doubleValue) =
    fromList $ map (calcCell doubleValue) $ zip (toList e1) (toList e2)

calcCell :: DoubleValue -> (Numbertype, Numbertype) -> Numbertype
calcCell doubleValue (v1,v2) = v2 + (doubleValue * v1)

fromList :: [Numbertype] -> Equation
fromList xs = Equation (init xs) (last xs)

toList :: Equation -> [Numbertype]
toList (Equation lhs rhs) = lhs ++ [rhs]

appendEquations :: [Equation] -> Pos -> AM -> AM
-- Concat with previously ignored equations
appendEquations newEquations pos am = AM (take (pos+1) (equations am) ++ newEquations)
