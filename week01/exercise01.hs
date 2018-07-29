toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse(toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (firstElem : secondElem : listRest) = firstElem : ((*2) secondElem) : (doubleEveryOther listRest)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10 = x + sumDigits(xs)
  | otherwise = ((fst $ divMod x 10) + (snd $ divMod x 10)) + sumDigits(xs)

validate :: Integer -> Bool
validate 0 = True
validate n
  | mod (sumDigits(reverse(doubleEveryOther(toDigitsRev(n))))) 10 == 0 = True
  | otherwise = False 
