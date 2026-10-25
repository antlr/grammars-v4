{-# LANGUAGE RelaxedLayout #-}

module ShouldFail where

f x = case x of
         False -> do
    { return x; }
