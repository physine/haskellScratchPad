
hypt :: Double -> Double -> Double
hypt x y = sqrt x^2 + y^2

-- ":t hypt" get the type declaration of hypt

----------------------------------
-- composition function "." 

addFive :: Integer -> Integer
addFive x = x + 5 

addTen :: Integer -> Integer
addTen x = x + 10

addFif = addFive . addTen
----------------------------------
-- infix (aposed to prefix) ``

-- 17 `mod` 3
----------------------------------
-- facatorial

fact :: Integer -> Integer
-- fact n = if n == 0 then 1 else n * fact (n-1)

-- This is sugar for the above.
-- If haskell sees an argument as a constant if assumes it is matches that
-- argument with the result it is equal to on the right side of the equal sign.

-- fact 0 = 1
-- fact n = n * fact (n-1)

-- Can also use guard for arbitrary conditions ...

fact n | n == 0 = 1
fact n = n * fact (n-1)

----------------------------------
-- Lists

{-

ghci> 1:(2:(3:[]))
[1,2,3]
ghci> 1:(2:(3:[]))
[1,2,3]

x:xs in haskell is like (cons x xs) in scheme.

[1, 2, 3, 4] is syntactic sugar for a:(b:(c:(d:[])))

-}

--------------------------------------
-- function which reversed list

reverseList :: [a] -> [a]
reverseList [] = []
reverseList xs = last xs :  reverseList (init xs)

--------------------------------------
-- function which partitions odd and even elements

--partitionList :: [a] -> [a]
--partitionList [] = []
--partitionList xs = 

--------------------------------------
-- myFilter list p

myFilter :: (Integer -> Bool) -> [Integer] -> [Integer]
myFilter p xs = [x | x <- xs, p x ]

--------------------------------------
-- passFail list pred

passFail1 :: (Integer -> Bool) -> [Integer] -> [[Integer]]
passFail1 p xs = [ filter p xs , filter (\x -> if p x then False else True) xs ]

passFail2 :: (Integer -> Bool) -> [Integer] -> [[Integer]]
passFail2 p xs = [ myFilter p xs , myFilter (\x -> if p x then False else True) xs ]

--------------------------------------
-- myMap f xs

myMap :: (a -> a) -> [a] -> [a]
myMap f xs = [f x | x <- xs]

--------------------------------------
-- crossMap f xs ys

sub :: (t1 -> t2 -> a) -> t1 -> [t2] -> [a]
sub f x ys = [(f) x y | y <- ys]

crossMap :: (a1 -> t -> a2) -> [a1] -> [t] -> [a2]
crossMap f xs ys =
    if null xs then
        []
    else
        [ n | n <- sub f (head xs) ys ] ++ crossMap f (tail xs) ys

--------------------------------------
-- scatterGather c xs ys 

scatterGather :: t -> [Int] -> [t] -> [t]
scatterGather c xs ys
    | null xs = [] 
    | head xs > length ys = c : scatterGather c (tail xs) ys
    | otherwise = ys !! head xs : scatterGather c (tail xs) ys

--------------------------------------
-- tear p xs - WORKING 

tear :: (a -> Bool) -> [a] -> [[a]]
tear p xs = [
    [ x | x <- xs, p x],
    [ x | x <- xs, not (p x)] ]

--------------------------------------------
-- WORKING

nChars xs ys n 
    | null xs || null ys = xs ++ ys
    | odd n = take n xs ++ nChars (drop n xs) ys (n+1)
    | even n = take n ys ++ nChars xs (drop n ys) (n+1)
    | otherwise = []

foo xs ys = nChars xs ys 1

--------------------------------------------
-- WORKING

trSub xs n = 
    if null xs then
        []
    else
        (head xs) !! n : trSub (tail xs) n

tr :: [[a]] -> [[a]]
tr xs = [ trSub xs n | n <- [0..length (xs !! 0)-1]]

--------------------------------------------
-- WORKING 

-- weaveHunks n xs ys = take n xs ++ take n ys ++ weaveHunks n (drop n xs) (drop n ys)

weaveHunks n xs ys
    | length xs < n = xs ++ ys
    | length ys < n = ys ++ xs
    | otherwise = take n xs ++ take n ys ++ weaveHunks n (drop n xs) (drop n ys)

--------------------------------------------
-- WORKING

sub1 :: a -> Int -> [a]
sub1 x y =
    if y == 0 then 
        []
    else
        x : sub1 x (y-1)

revCount :: [a] -> [Int] -> [a]
revCount xs ys =
    if null xs then
        []
    else
        revCount (tail xs) (tail ys) ++ sub1 (head xs) (head ys)

--------------------------------------------
-- WORKING

afterFilter :: (a -> Bool) -> [a] -> [a]
afterFilter p xs
    | null xs = []
    | length xs == 1 = []
    | p (head xs) = xs !! 1 : afterFilter p (tail xs)
    | otherwise = afterFilter p (tail xs)

--------------------------------------------
-- WORKING

sortPartition :: (a -> Bool) -> [a] -> [a]
sortPartition p xs = [ x | x <- xs, p x] ++ [ x | x <- xs, not (p x)]

--------------------------------------------
-- WORKING

tear2 :: (a -> Bool) ->  [a] -> [[a]]
tear2 p xs = [
    [ x | x <- xs, p x ],
    [ x | x <- xs, not (p x) ]]

--------------------------------------------
-- WORKING

fooHelper2 xs ys n
    | odd n = take n xs : fooHelper2 (drop n xs) ys (n+1)
    | even n = take n ys : fooHelper2 xs (drop n ys) (n+1)
    | otherwise = []

foo2 xs ys = fooHelper2 xs ys 1





















