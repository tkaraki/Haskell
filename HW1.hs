-- CptS 355 - Spring 2020 Assignment 1
-- Name: Tala Karaki


module HW1
     where

-- 1a. exists
exists :: Eq t => t -> [t] -> Bool
exists x xs = any (== x) xs
     

-- 1b. type for exists

{- type for exists is exists :: Eq t => t -> [t] -> Bool 
and not exists :: t -> [t] -> Bool 
because we are using the equality operator == which 
is a member function of Typeclass Eq -}


-- 1.c countInList
countInList :: (Num p, Eq t) => t -> [t] -> p
countInList x [] = 0
countInList x (y:ys)
     | x==y = 1+(countInList x ys)
     | otherwise = countInList x ys


-- 2. listDiff
listDiff :: Eq a => [a] -> [a] -> [a]
listDiff [] _  = []
listDiff xs [] = xs
listDiff (x:xs) (y:ys)
     | x == y    = listDiff xs ys
     | otherwise = listDiff (x : (listDiff xs [y])) ys


-- 3. firstN
firstN :: (Ord t, Num t) => [a] -> t -> [a]
firstN [] _ = []
firstN (x:xs) n
     | n > 0 = x : firstN xs (n-1)
     | otherwise = []


-- 4. busFinder
buses = [("Lentil",["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main",
 "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop",
 "Derby", "Dilke"]),
 ("Wheat",["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay",
 "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"]),
 ("Silver",["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Shopco",
 "RockeyWay"]),
 ("Blue",["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell",
 "Chinook", "Library"]),
 ("Gray",["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall",
 "Stadium", "Colorado"])
 ] 

busFinder :: Eq t => t -> [(a, [t])] -> [a]
busFinder _ [] = []
busFinder s ((x,y):ys) | (exists s y) = x : (busFinder s ys)
                       | otherwise = (busFinder s ys)


-- 5. cumulativeSums
sumHelper [] n = []
sumHelper (x:xs) n = (n+x) : (sumHelper xs (n+x))

cumulativeSums :: Num a => [a] -> [a]
cumulativeSums xs = sumHelper xs 0



-- 6. groupNleft
splitHelper :: Int -> [a] -> ([a],[a]) 
splitHelper _ [] = ([],[])
splitHelper 0 xs = ([],xs)
splitHelper n (x:xs)
  | n > 0 = (x:xs1,xs2)
    where
    (xs1,xs2) = splitHelper (n-1) xs


groupNleft :: Int -> [a] -> [[a]]
groupNleft _ [] = []
groupNleft n xs = as : groupNleft n bs 
  where (as,bs) = splitHelper n xs

  

main = do

print (exists 1  [])
print (countInList "5" ["3","5","5","-","4","5","1"])
print (listDiff [1,2,3] [1,1,2] )
print (firstN [1,2,3,4,5,6,7] 0)
print (busFinder "Shopco" buses)
print (cumulativeSums [1,2,3,4,-4,-3,-2])
print (groupNleft 3 [1, 2, 3, 4, 5, 6, 7, 8])

