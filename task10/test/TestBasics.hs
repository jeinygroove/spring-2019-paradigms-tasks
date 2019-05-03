import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on a non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on an infinite list" $
        head' [1..] @?= 1

    , testCase "tail' works on an empty list" $
        tail' ([] :: [Integer]) @?= []

    , testCase "tail' works on a non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' takes 3 elements from an infinite list" $
        take' 3 [1..] @?= [1, 2, 3]

    , testCase "take' takes 1 element from a 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "drop' drops 1 element from a 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' returns empty list if we want to drop more elements than the list has" $
        drop' 5 [1, 2, 3] @?= []

    , testCase "drop' can drop all elements in a list" $
        drop' 3 [1, 2, 3] @?= []

    , testCase "drop' returns the same list if we want to drop negative number of elements" $
        drop' (-1) [1, 2] @?= [1, 2]
 
    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' selects only positive numbers in the list" $
        filter' (\x -> x > 0) [-1, -2, 1, -4, 3] @?= [1, 3]
    
    , testCase "filter' works on an empty list too" $
        filter' (\x -> x) [] @?= []

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6
    
    , testCase "foldl'' works on an empty list" $
        foldl'' (*) 3 [] @?= 3

    , testCase "foldl'' can be used for finding production of elements" $
        foldl'' (*) 1 [1, 2, 3, 4, 5] @?= 120

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on infinite lists" $
        take' 5 (concat' [1, 2, 3] [4..]) @?= [1, 2, 3, 4, 5]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]
