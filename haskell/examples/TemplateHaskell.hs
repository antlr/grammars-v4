{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import System.IO
import Proposal229f_instances

-- Testing that we can parse $[...] and $"..."
main = do
  hPutStrLn stderr $['1','2','3']
  hPutStrLn stderr $$['1','2','3']
  hPutStrLn stderr $"123"
  hPutStrLn stderr $$"123"
