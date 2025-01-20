sumUp :: [Int] -> Int
sumUp []     = 0
sumUp (x:xs) = x + sumUp xs

evens :: [Int] -> [Int]
evens []     = []
evens (x:xs) 
    | even x = x : evens xs
    | otherwise = evens xs

incAll :: [Int] -> [Int]
incAll[] = []
incAll (x:xs)=(x+1):incAll xs

incBy :: Int -> [Int] -> [Int]
incBy _[] = []                     
incBy n (x:xs) = (x + n) : incBy n xs 

append :: [Int] -> [Int] -> [Int]
append [] second =second
append first [] = first
append (x:xs) second = x: append xs second


--problem 4
sumUp' :: [Int] -> Int
sumUp' l=foldr(+) 0 l

evens' :: [Int] -> [Int]
evens' l     =  filter1 even l

incAll' :: [Int] -> [Int]
incAll' l = map1(+1) l

incBy' :: Int -> [Int] -> [Int]
incBy' n  l = map1(+n) l

-- problem5 checking by replace map in problem 4 to map' and filter'
map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []                       
map1 f (x:xs) = f x : map1 f xs   

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []                    
filter1 p (x:xs)
  | p x       = x : filter1 p xs      
  | otherwise = filter1 p xs  

-- problem 6 
sqrt' :: Float -> Maybe Float
sqrt' x 
  | x < 0     = Nothing         
  | otherwise = Just (sqrt x)    

div' :: Float -> Float -> Either String Float
div' _ 0 = Left "Error: Division by zero"   
div' x y = Right (x / y)     

--problem 7
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pairUp :: [a] -> [b] -> [(a, b)]
pairUp [] _ = []                      
pairUp _ [] = []                      
pairUp (x:xs) (y:ys) = (x, y) : pairUp xs ys 

splitUp :: [(a, b)] -> ([a], [b])
splitUp [] = ([], [])                      
splitUp ((x, y):xs) = (x:xs1, y:ys1)       
    where   
        (xs1, ys1) = splitUp xs                

sumAndLength :: [Int] -> (Int, Int)
sumAndLength l = foldr (\x (s, len) -> (s + x, len + 1)) (0, 0) l

-- problem 8 read

main :: IO()
main = do
    --print (sumUp [1,2,3,4,5])
    --print(sumUp [])
    --print (evens [1,2,3,4,5])
    --print (evens [])
    --print (incAll [1, 2, 3, 4]) 
    --print (incAll [])           
    --print (incBy 2 [1, 2, 3, 4])
    --print (incBy 0 [1, 2, 3, 4])
    --print (incBy 5 [])      
    --print (append [1,2][3,4])   
    --print (append [][])   
    print (sumUp' [1,2,3,4,5]) 
    print (evens' [1,2,3,4,5])
    print (incAll' [1, 2, 3, 4]) 
    print (incBy' 2 [1, 2, 3, 4])
    print(sqrt' 5)
    print(sqrt' (-5))
    print(swap (3.14, 42))    
    print(pairUp[1,2,3] [4,5,6])  
    print(splitUp [(1,4),(2,5),(3,6)])  
    print(sumAndLength[1,2,3,4])