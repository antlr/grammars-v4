describeBlackOrWhite c =
    "This colour is"
    ++ case c of
         Black           -> " black"
         White           -> " white"
         RGB 0 0 0       -> " black"
         RGB 255 255 255 -> " white"
         _               -> "... uh... something else"
    ++ ", yeah?"