-- CptS 355 - Spring 2020 Assignment 2
-- Tala Karaki

module HW2
     where


{- intersect & intersectTail & intersectAll - 22%-}
--intersect
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (ys) 
     | (x `elem` ys) && not (x `elem` xs)= x : (intersect xs ys)
     | otherwise = (intersect xs ys)


--intersectTail
intersectTail :: Eq a => [a] -> [a] -> [a]
intersectTail [] _ = []
intersectTail _ [] = []
intersectTail xL y = reverse ( intail xL y [] )
          where intail [] [] acc = acc
                intail [] _ acc = acc
                intail (x:xs) ys acc 
                    | (x `elem` ys) && not (x `elem` xs) = (intail xs ys (x:acc))
                    | otherwise = (intail xs ys acc)


--intersectAll
intersectAll:: Ord a => [[a]] -> [a]
intersectAll (x:xs) = foldr intersect x xs


{-2 - partition - 10%-}
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)


{- 3 - sumL, sumMaybe, and sumEither - 27% -}
--sumL
sumL :: (Num b, Foldable t) => [t b] -> b
sumL (x:xs) = foldl (+) 0 (map sum (x:xs))


-- sumMaybe 
sumHelper :: (Num a) => Maybe a -> Maybe a -> Maybe a
sumHelper Nothing Nothing = Nothing
sumHelper Nothing (Just x) = (Just x)
sumHelper (Just x) Nothing = (Just x)
sumHelper (Just x) (Just y) = (Just (x + y))

sumMaybe :: (Num a) => [[(Maybe a)]] -> Maybe a
sumMaybe iL = foldl sumHelper (Just 0) (map(foldl sumHelper(Just 0)) iL)


-- sumEither
data IEither = IString String | IInt Int
                deriving (Show, Read, Eq)
                
getInt x = read x::Int

sumHelper2 :: IEither -> IEither -> IEither
sumHelper2 (IInt x) (IInt y) = IInt ((x) + ( y ))
sumHelper2 (IInt x) (IString y) = IInt ((x) + (getInt y))
sumHelper2 (IString x) (IInt y) = IInt ((getInt x) + (y))
sumHelper2 (IString x) (IString y) = IInt ((getInt x) + (getInt y))

sumEither:: Foldable t => [t IEither] -> IEither
sumEither iL = foldl sumHelper2 (IInt 0) (map(foldl sumHelper2(IInt 0)) iL)


{-4 - depthScan, depthSearch, addTrees - 37%-}

data Tree a = LEAF a | NODE a (Tree a) (Tree a)
              deriving (Show, Read, Eq)

--depthScan
depthScan :: Tree a -> [a]
depthScan (LEAF x) = [x]
depthScan (NODE x l r) = (depthScan l) ++ (depthScan r) ++ x : []

 
--depthSearch
searchHelper :: (Eq a) => Tree a -> a -> Int -> Int
searchHelper (LEAF x) y n  = if  (x == y) then  n else (-1)
searchHelper (NODE x l r) y n = if (searchHelper l y (n+1)) > 0 then (searchHelper l y (n+1)) 
                               else if (searchHelper r y (n+1))>0   then (searchHelper r y (n+1))
                               else (-1) 

depthSearch :: (Eq a) => Tree a -> a -> Int
depthSearch (LEAF x) _  = 1
depthSearch tree x = searchHelper tree x 1


--addTrees
addTrees :: Num a => Tree a -> Tree a -> Tree a
addTrees (LEAF x) (LEAF y) = LEAF (x+y)
addTrees (NODE x l r) (NODE xx ll rr) = NODE (x+xx) (addTrees l ll) (addTrees r rr)
addTrees (LEAF xx) (NODE x l r) = NODE (x + xx) l r
addTrees (NODE x l r) (LEAF xx) = NODE (x + xx) l r
 

{- 5- Create two trees of type Tree. The height of both trees should be at least 4. Test your functions depthScan, depthSearch, addTrees with those trees. 
The trees you define should be different than those that are given.   -}
myTree1 = NODE 1 (NODE 2 (LEAF 4) (LEAF 5)) (NODE 3 (NODE 6 (LEAF 8) (LEAF 9)) (LEAF 7))
 {-             (1) 
               /   \
             (2)    (3)
             / \    / \
           (4) (5)(6) (7)
                  / \
                (8) (9)              -}



myTree2 = NODE 1 (NODE 2 (LEAF 4) (LEAF 5)) (NODE 3 (NODE 6 (LEAF 8) (NODE 9 (LEAF 10) (LEAF 11))) (LEAF 7))
{-              (1) 
               /   \
             (2)    (3)
             / \    / \
           (4) (5)(6) (7)
                  / \
                (8) (9)
                    / \
                  (10) (11)          -}

{- 
p5_test1 = (depthScan myTree1)
p5_test2 = (depthSearch myTree1 7)
p5_test3 = (depthScan myTree2)
p5_test4 = (depthSearch myTree1 10)
p5_test5 = (depthSearch myTree2 10)
p5_test6 = (addTrees myTree1 myTree2)


main = do

     --Testing Problem 5 Trees
     print ("Expected:  [4,5,2,8,9,6,7,3,1] " , p5_test1 ) 
     print ("Expected: 3 " , p5_test2)
     print ("Expected:  [4,5,2,8,10,11,9,6,7,3,1] " , p5_test3) 
     print ("Expected: -1 " , p5_test4)
     print ("Expected: 5 " , p5_test5)
     print ("Expected:  NODE 2 (NODE 4 (LEAF 8) (LEAF 10)) (NODE 6 (NODE 12 (LEAF 16) (NODE 18 (LEAF 10) (LEAF 11))) (LEAF 14)) " , p5_test6)
     
 -}