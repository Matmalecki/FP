
-- zad 7

fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibi n = fib_iter 0 1 n

fib_iter a b n = 
    if n <= 1 then b
    else fib_iter b (a+b) (n-1)

-- zad 8

square x = x * x

expr x n 
    | (n==0) = 1
    | (n==1) = x
    | even n = square (expr x (div n 2))
    | odd n  = x * (expr x (n-1))

expi x n = exp_iter x n 1

exp_iter x n acc 
    | n == 0 = acc
    | n == 1 = acc * x
    | even n = exp_iter (x*x) (div n 2) (acc)
    | odd n = exp_iter (x*x) (div (n-1) 2) (acc * x)

-- zad 9

append [] m = m
append (x:xs) m = x:(append xs m)


member x [] = False
member x (y:ys) = if x == y then True else member x ys

reverse1 [] = []
reverse1 (x:xs) = append (reverse1 xs) [x]

last1 [x] = x
last1 (x:xs) = last1 xs

delete x [] = []
delete x (y:ys)
    | x == y = delete x ys
    | otherwise = (y:(delete x ys))

split x l = pom_split x l [] []


pom_split x [] l1 l2 = [l1,l2]

pom_split x (y:ys) l1 l2 
    | x < y = pom_split x ys (y:l1) l2
    | otherwise = pom_split x ys l1 (y:l2)

map1 f (x:xs) = (f x):(map f xs)

map2 _ [] _          = []
map2 _ _ []          = []
map2 f (x:xs) (y:ys) = (f x y): (map2 f xs ys)

pairing [] _ = []
pairing _ [] = []
pairing (x:xs) (y:ys) = (x,y):(pairing xs ys) 

filter1 p [] = []
filter1 p (x:xs)
    | (p x) = x:(filter1 p xs)
    | otherwise = filter1 p xs