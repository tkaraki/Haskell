-- CptS 355 - Spring 2020 Assignment 1 HUnit
-- Name: Tala Karaki

module HW1Tests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW1

{- Two useful functions in the HUnit package are assertEqual and assertBool.
The arguments to 'assertEqual' are:
      a descriptive string
      the expected value
      the value being tested
The arguments to 'assertBool' are:
      a descriptive string
      the boolean value being tested
-}

buses = [("Lentil",["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main", "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop", "Derby", "Dilke"]), 
         ("Wheat",["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"]), 
         ("Silver",["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Shopco", "RockeyWay"]),
         ("Blue",["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell", "Chinook", "Library"]),
         ("Gray",["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall", "Stadium", "Colorado"])
        ] 

      --Tests for number 1a   
p1a_test1 = TestCase (assertEqual "exists [1] [[3] [5]]" False  (exists [1] [[3],[5]]) ) 
p1a_test2 = TestCase (assertBool "exists 1 [1,2,3]"  (exists 1 [1,2,3]) ) 
p1a_test3 = TestCase (assertBool "exists '3' \"CptS355\"" (exists '3' "CptS355"))
p1a_test4 = TestCase (assertEqual "exists  '+' \"='-+;\" " True  (exists  '+' "='-+;"))
p1a_test5 = TestCase (assertBool "exists 5 [1..10]"  (exists 5 [1..10]) ) 

      --Tests for number 1b  
p1b_test1 = TestCase (assertEqual "countInList \"5\" [\"3\",\"5\",\"5\",\"-\",\"4\",\"5\",\"1\"]" 3  (countInList "5" ["3","5","5","-","4","5","1"]) ) 
p1b_test2 = TestCase (assertEqual "countInList [] [[],[1,2],[3,2],[5,6,7],[8],[]]" 2 (countInList [] [[],[1,2],[3,2],[5,6,7],[8],[]]) ) 
p1b_test3 = TestCase (assertEqual "countInList True [True, False, False, False, True, True, True]" 4 (countInList True [True, False, False, False, True, True, True]))
p1b_test4 = TestCase (assertEqual "countInList \"pie\" [\"pie\", \"apple pie\", \"blueberry pie\", \"carrot pie\"]" 1  (countInList "pie" ["pie", "apple pie", "blueberry pie", "carrot pie"]) ) 
p1b_test5 = TestCase (assertEqual "countInList 0 [0..100]" 1 (countInList 0 [0..100]) ) 

      -- Test for number 2
p2_test1 = TestCase (assertEqual "listDiff [1,2,3] [1,1,2]"  (sort [3])  (sort (listDiff [1,2,3] [1,1,2])) ) 
p2_test2 = TestCase (assertEqual "listDiff [1,2,2,3,3,3] [1,1,2,3]"  (sort [2,3,3])  (sort (listDiff [1,2,2,3,3,3] [1,1,2,3])) )
p2_test3 = TestCase (assertEqual "listDiff [[2,3],[1,2],[2,3]] [[1],[2,3]]"  (sort [[2,3],[1,2]])  (sort (listDiff [[2,3],[1,2],[2,3]] [[1],[2,3]])) )
p2_test4 = TestCase (assertEqual "listDiff ['a','b','c'] ['b']"  (sort ['a', 'c'])  (sort (listDiff ['a','b','c'] ['b'])) )
p2_test5 = TestCase (assertEqual "listDiff [1] []"  (sort [1])  (sort (listDiff [1] [])) )

      ----Tests for number 3  
p3_test1 = TestCase (assertEqual "firstN [[1,2,3],[4,5],[6],[],[7,8],[]] 4" [[1,2,3],[4,5],[6],[]] (firstN [[1,2,3],[4,5],[6],[],[7,8],[]] 4) ) 
p3_test2 = TestCase (assertEqual "firstN [1,2,3,4,5,6,7] 4"  [1,2,3,4] (firstN [1,2,3,4,5,6,7] 4))
p3_test3 = TestCase (assertEqual "firstN [1,2,3,4,5,6,7] 10" [1,2,3,4,5,6,7] (firstN [1,2,3,4,5,6,7] 10))
p3_test4 = TestCase (assertEqual "firstN ['l','m','n','o','p'] 1"  ['l'] (firstN ['l','m','n','o','p'] 1))
p3_test5 = TestCase (assertEqual "firstN [1,2,3,4,5,6,7] 0" [] (firstN [1,2,3,4,5,6,7] 0))
p3_test6 = TestCase (assertEqual "firstN [1,2,3,4,5,6,7] -1" [] (firstN [1,2,3,4,5,6,7] (-1)))

      ----Tests for number 4
p4_test1 = TestCase (assertEqual "busFinder \"Walmart\" buses" (sort ["Lentil","Wheat","Silver"] )  (sort (busFinder "Walmart" HW1.buses)) ) 
p4_test2 = TestCase (assertEqual "busFinder \"Shopco\" buses" (sort ["Silver"] )  (sort (busFinder "Shopco" HW1.buses)) ) 
p4_test3 = TestCase (assertEqual "busFinder \"Main\" buses" (sort ["Lentil","Gray"])  (sort (busFinder "Main" HW1.buses)) ) 
p4_test4 = TestCase (assertEqual "busFinder \"Beasley\" buses" (sort [])  (sort (busFinder "Beasley" HW1.buses)) ) 
p4_test5 = TestCase (assertEqual "busFinder \"co\" buses" (sort [])  (sort (busFinder "co" HW1.buses)) ) 
p4_test6 = TestCase (assertEqual "busFinder \",\" buses" (sort [])  (sort (busFinder "," HW1.buses)) ) 
      
      ----Tests for number 5
p5_test1 = TestCase (assertEqual "cumulativeSums [1,2,3,4,5,6,7,8,9,10]" ([1,3,6,10,15,21,28,36,45,55]) (cumulativeSums [1,2,3,4,5,6,7,8,9,10])) 
p5_test2 = TestCase (assertEqual "cumulativeSums [5,5,5,5,5,5,5]" ([5,10,15,20,25,30,35]) (cumulativeSums [5,5,5,5,5,5,5])) 
p5_test3 = TestCase (assertEqual "cumulativeSums [1,2,3,4,-4,-3,-2]" ([1,3,6,10,6,3,1]) (cumulativeSums [1,2,3,4,-4,-3,-2]))
p5_test4 = TestCase (assertEqual "cumulativeSums []" ([]) (cumulativeSums [])) 
p5_test5 = TestCase (assertEqual "cumulativeSums [-5,-1,-11]" ([-5,-6,-17]) (cumulativeSums [-5,-1,-11]))

      ----Tests for number 6
p6_test1 = TestCase (assertEqual "groupNleft 3 [1, 2, 3, 4, 5, 6, 7, 8]" ([[1,2,3],[4,5,6],[7,8]]) (groupNleft 3 [1, 2, 3, 4, 5, 6, 7, 8]) ) 
p6_test2 = TestCase (assertEqual "groupNleft 2 [1, 2, 3, 4, 5, 6, 7, 8]" ([[1,2],[3,4],[5,6],[7,8]]) (groupNleft 2 [1, 2, 3, 4, 5, 6, 7, 8]) ) 
p6_test3 = TestCase (assertEqual "groupNleft 2 [(1,\"a\"),(2,\"b\"),(3,\"c\"),(4,\"d\"),(5,\"e\"),(6,\"f\")]" ([[(1,"a"),(2,"b")],[(3,"c"),(4,"d")],[(5,"e"),(6,"f")]]) (groupNleft 2 [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e"),(6,"f")]) ) 
--p6_test4 = TestCase (assertEqual "groupNleft 3 []" ([]) (groupNleft 3 []) )
p6_test5 = TestCase (assertEqual "groupNleft -1 [1, 2, 3]" ([[]]) (groupNleft (-1) [1, 2, 3]) ) 
-- assertEqual can't resolve the type of [] ; so the following test gives a type error. 
--p6_test4 = TestCase (assertEqual "groupNleft 3 []" ([])   (groupNleft 3 []) ) 


tests = TestList [ TestLabel "Problem 1a- test1 " p1a_test1,
                   TestLabel "Problem 1a- test2 " p1a_test2,  
                   TestLabel "Problem 1a- test3 " p1a_test3,
                   TestLabel "Problem 1a- testing symbols " p1a_test4,  
                   TestLabel "Problem 1a- testing on auto generated list " p1a_test5,

                   TestLabel "Problem 1b- test1 " p1b_test1,
                   TestLabel "Problem 1b- test2 " p1b_test2,  
                   TestLabel "Problem 1b- test3 " p1b_test3,
                   TestLabel "Problem 1b- testing string " p1b_test4,  
                   TestLabel "Problem 1b- test 0 onlist between 0 and 100" p1b_test5,

                   TestLabel "Problem 2- test1 " p2_test1, 
                   TestLabel "Problem 2- test2 " p2_test2, 
                   TestLabel "Problem 2- test3 " p2_test3,
                   TestLabel "Problem 2- testing chars " p2_test4, 
                   TestLabel "Problem 2- testing empty list " p2_test5,

                   TestLabel "Problem 3- test1 " p3_test1, 
                   TestLabel "Problem 3- test2 " p3_test2, 
                   TestLabel "Problem 3- test3 " p3_test3,
                   TestLabel "Problem 3- testing chars " p3_test4,
                   TestLabel "Problem 3- testing 0 " p3_test5,
                   TestLabel "Problem 3- testing negative input " p3_test6,

                   TestLabel "Problem 4- test1 " p4_test1, 
                   TestLabel "Problem 4- test2 " p4_test2, 
                   TestLabel "Problem 4- test3 " p4_test3,
                   TestLabel "Problem 4- test4 " p4_test4,
                   TestLabel "Problem 4- testing parts of list input " p4_test3,
                   TestLabel "Problem 4- testing chars" p4_test4,

                   TestLabel "Problem 5- test1 " p5_test1, 
                   TestLabel "Problem 5- test2 " p5_test2, 
                   TestLabel "Problem 5- test3 " p5_test3,
                   TestLabel "Problem 5- testing empty list " p5_test4, 
                   TestLabel "Problem 5- testing negative list " p5_test5,

                   TestLabel "Problem 6- test1 " p6_test1, 
                   TestLabel "Problem 6- test2 " p6_test2, 
                   TestLabel "Problem 6- test3 " p6_test3,
                   --TestLabel "Problem 6- testing empty list  " p6_test4, 
                   TestLabel "Problem 6- testing negative input" p6_test5
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests