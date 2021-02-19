min' :: Ord p => p -> p -> p
min' a b
    | a <= b = a
    | otherwise = b

max' :: Ord p => p -> p -> p
max' a b 
    | a >= b = a
    | otherwise = b 

succ' :: Num a => a -> a
succ' a = a + 1 

doubleMe :: Num a => a -> a
doubleMe x = x + x
quadrupleMe :: Integer -> Integer
quadrupleMe  = doubleMe . doubleMe

doubleMeIfLessThan100 :: (Ord p, Num p) => p -> p
doubleMeIfLessThan100 x 
    | x < 100 = doubleMe x 
    | otherwise = x

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x 
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x:xs) = x: init' xs

length' :: Num p => [a] -> p
length' [] = 0
length' (_:xs) = 1 + length' xs

null' :: [a] -> Bool
null' [] = True
null' _ = False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: (Ord t, Num t) => t -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n <= 0 = []
    | otherwise = x: take' (n-1) xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

drop' :: (Ord t, Num t) => t -> [a] -> [a]
drop' _ [] = []
drop' n xs 
    | n <= 0 = xs
    | otherwise = drop' (n-1) xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f lst@(x:xs)
    | f x = dropWhile' f xs
    | otherwise = lst

maximum' :: (Ord a, Num a) => [a] -> a
maximum' [] = error "EmptyList"
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

minimum' :: (Ord a, Num a) => [a] -> a
minimum' [] = error "Empty List"
minimum' [x] = x
minimum' (x:xs) = min' x  (minimum' xs)

sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x:xs) = x + sum' xs    

prod' :: Num p => [p] -> p
prod' [] = 1
prod' (x:xs) = x * prod' xs

elem' :: Eq t => t -> [t] -> Bool
elem' _ [] = False
elem' x (y:ys) 
    | x == y = True
    | otherwise = elem' x ys

cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = xs ++ cycle xs   

repeat' :: t -> [t]
repeat' x = x : repeat' x

replicate' :: (Ord t, Num t) => t -> a -> [a]
replicate' n x = take' n $ repeat' x

doubleList :: Num a => [a] -> [a]
doubleList xs = [x * 2 | x <- xs]

fst :: (a, b) -> a
fst (a, _) = a 

snd :: (a, b) -> b
snd (_, b) = b 

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = [] 
zip' _ [] = [] 
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

triangle_triplets :: [(Integer, Integer, Integer)]
triangle_triplets = [(z, y, x) | x <- [1..25], y <- [1..x], z <- [1..y], x * x == y * y + z * z]

remove_uppercase :: [Char] -> [Char]
remove_uppercase str = [c | c <- str, not $ elem' c ['A'..'Z']]

fact_basic :: (Eq p, Num p) => p -> p
fact_basic 0 = 1
fact_basic n = n * fact_basic (n-1)

factorial :: (Eq p, Num p) => p -> p
factorial n = tail_factorial n 1 

tail_factorial :: (Eq t, Num t) => t -> t -> t
tail_factorial 0 r = r 
tail_factorial n r = tail_factorial (n - 1) (r * n)

fib_basic :: (Eq a, Num a, Num p) => a -> p
fib_basic 0 = 1
fib_basic 1 = 1
fib_basic n = fib_basic (n-1) + fib_basic (n-2)

fib :: (Eq t1, Num t1, Num t2) => t1 -> t2
fib n = tail_fib n 0 1 

tail_fib :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
tail_fib 0 _ prev1 = prev1
tail_fib n prev2 prev1 = tail_fib (n-1) prev1 (prev1 + prev2) 

balance :: [Char] -> Bool
balance xs = balance' xs ""

balance' :: [Char] -> [Char] -> Bool
balance' [] [] = True 
balance' [] _  = False
balance' (x:xs) ys
    | x == '(' = balance' xs (x:ys)
    | x == ')' && ys == [] = False
    | x == ')' = balance' xs (tail ys)
    | otherwise = balance' xs ys

fizbuzz :: (Integral a, Show a) => a -> [Char]
fizbuzz n 
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3  == 0 = "Fizz"
    | n `mod` 5  == 0 = "Buzz"
    | otherwise       = show n 

printFizBuzz :: IO ()
printFizBuzz = putStrLn $ unwords $ map fizbuzz [1..100]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort bigger
    where
        smaller = [a | a <- xs, a <= x]
        bigger  = [a | a <- xs, a >  x]

map' :: (t -> a) -> [t] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

collatz_chain :: Integral a => a -> [a]
collatz_chain 1 = [1]
collatz_chain n 
    | even n = n : collatz_chain (n `div` 2)
    | odd n  = n : collatz_chain (n * 3 + 1)

foldl' :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs 

sum'' :: [Integer] -> Integer
sum'' = foldl' (+) 0

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' sep (x:xs) = x : sep : intersperse' sep xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' [[]] = []
concat' (x:xs) = x ++ concat' xs  

intercalate' :: [a] -> [[a]] -> [a]
intercalate' sep xs = concat' $ intersperse' sep xs

unwords' :: [[Char]] -> [Char]
unwords' = intercalate' " "

intersect'               :: (Eq a) => [a] -> [a] -> [a]
intersect'               =  intersectBy' (==)

intersectBy'             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy' eq xs ys    =  [x | x <- xs, any (eq x) ys]

findIndices' :: (Num a, Enum a) => (t -> Bool) -> [t] -> [a]
findIndices' p xs = [i | (x, i) <- zip' xs [0..], p x]

findMissing :: Eq a => [a] -> [a] -> [a]
findMissing xs ys = [x | x <- xs, not $ elem' x ys]
