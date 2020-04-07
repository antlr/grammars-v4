{-# LANGUAGE MultiWayIf #-}

bmiTell :: Float -> String
bmiTell bmi = if 
  | bmi <= 18.5 -> if | bmi >= 10 -> "lala."
                      | otherwise -> "lolo."
  | bmi <= 25.0 -> "Average weight."
  | bmi <= 30.0 -> "Overweight."
  | otherwise   -> "Clinically overweight."
