-- | Test for @NegativeLiterals@ extension (see GHC #13211)

{-# LANGUAGE NegativeLiterals #-}

floatZero0 = 0 :: Float
floatZero1 = 0.0 :: Float

floatNegZero0 = -0 :: Float
floatNegZero1 = -0.0 :: Float

doubleZero0 = 0 :: Double
doubleZero1 = 0.0 :: Double

doubleNegZero0 = -0 :: Double
doubleNegZero1 = -0.0 :: Double

main = do
    print (isNegativeZero floatZero0)
    print (isNegativeZero floatZero1)
    print (isNegativeZero floatNegZero0)
    print (isNegativeZero floatNegZero1)
    print (isNegativeZero doubleZero0)
    print (isNegativeZero doubleZero1)
    print (isNegativeZero doubleNegZero0)
    print (isNegativeZero doubleNegZero1)