double :: Num a => a -> a
double x = x * 2

bothTrue :: Bool -> Bool -> Bool
bothTrue True True = True
bothTrue a b = a && b

oneTrue :: Bool -> Bool -> Bool
oneTrue True True = True
oneTrue True False = True
oneTrue False True = True
oneTrue False False = False

sign :: (Num a, Ord a) => a -> Int
sign a
   | a < 0 = -1
   | a == 0 = 0
   | otherwise = 1

sign' :: (Num a, Ord a) => a -> Int
sign' 0 = 0
sign' a
   | a < 0 = -1
   | otherwise = 1

triple :: Num a => a -> a
triple a = a * 3

max3 :: Ord a => a -> a -> a -> a
max3 a b c = max (max a b) c

f2c :: Double -> Double
f2c f = (f - 32) / 1.8

gcd' :: (Integral x, Ord x) => x -> x -> x
gcd' a 0 = a
gcd' 0 b = b
gcd' a b = gcd' b (mod a b)

eval_f :: (Num a, Ord a) => a -> a
eval_f x
   | x <= 0 = -x
   | x >= 2 = 4
   | otherwise = x * x

dayOfWeek :: Int -> String
dayOfWeek 1 = "Monday"
dayOfWeek 2 = "Tuesday"
dayOfWeek 3 = "Wednesday"
dayOfWeek 4 = "Thursday"
dayOfWeek 5 = "Friday"
dayOfWeek 6 = "Saturday"
dayOfWeek 7 = "Sunday"
dayOfWeek x = error "Wrong number"

describeTemperature :: Double -> String
describeTemperature a
   | a < 10 = "cold"
   | (a >= 10) && (a < 15) = "cool"
   | (a >= 15) && (a < 25) = "warm"
   | otherwise = "hot"

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

circleArea :: Double -> Double
circleArea radius = pi * radius^2

isLeapYear :: Int -> Bool
isLeapYear year
  | (mod year 100) == 0 && (mod year 400) /= 0 = False
  | otherwise = mod year 4 == 0

nDays :: Int -> Int
nDays year
  | isLeapYear year = 366
  | otherwise = 365


nDays' year = if isLeap then 366 else 365
  where
    isLeap
      | (mod year 100) == 0 && (mod year 400) /= 0 = False
      | otherwise = mod year 4 == 0

test = if bothTrue False True || oneTrue False False || True `xor` True || f2c 80 > 0  then
         (gcd' 128 76 + sign (-6) + sign' 5 + max3 12 27 32 + nDays 2015 + nDays' 2015,
          triple (double (eval_f 1.5)),
          dayOfWeek 3 ++ " " ++ describeTemperature (f2c 100))
         else (0, 0, "")
