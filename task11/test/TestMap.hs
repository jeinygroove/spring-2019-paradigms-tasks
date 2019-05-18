{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Unite tests for fromList ans toAscList" [
            testCase "fromList can construct an empty map" $
                let map = fromList [] :: m Int String in
                Map.null map @?= True,

            testCase "fromList can construct a not empty map" $
                let map = fromList [(1, "Hello"), (2, "world!")] :: m Int String in
                (Map.size map     == 2            &&
                 Map.lookup 1 map == Just "Hello" &&
                 Map.lookup 2 map == Just "world!") @?= True,

            testCase "toAscList . fromList sorts list" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                Map.toAscList map @?= [(1, "x"), (2, "a"), (3, "c")]
        ],

        testGroup "Unite test for insert" [
            testCase "insert inserts into an empty map" $
                let map = empty :: m Int String in 
                let new_map = Map.insert 1 "PF" map in
                Map.lookup 1 new_map @?= Just "PF",

            testCase "insert changes value" $
                let map = singleton 1 "PF" :: m Int String in
                let new_map = Map.insert 1 "S" map in
                Map.lookup 1 new_map @?= Just "S", 
 
            testCase "insert inserts into a not empty map" $
                let map = singleton 1 "PF" :: m Int String in
                let new_map = Map.insert 2 "S" map in
                Map.lookup 2 new_map @?= Just "S" 
        ], 

        testGroup "Unite test for insertWith" [
            testCase "insertWith inserts into an empty map" $
                let map = empty :: m Int String in
                let new_map = Map.insertWith (const $ const "F") 1 "PF" map in
                Map.lookup 1 new_map @?= Just "PF", 

            testCase "insertWith alters value" $
                let map = singleton 1 "PF" :: m Int String in
                let new_map = Map.insertWith (++) 1 "new " map in
                Map.lookup 1 new_map @?= Just "new PF", 
 
            testCase "insertWith inserts into a not empty map" $
                let map = singleton 1 "PF" :: m Int String in
                let new_map = Map.insertWith (++) 2 "S" map in
                Map.lookup 2 new_map @?= Just "S" 
        ], 

        testGroup "Unite test for insertWithKey" [
            testCase "insertWithKey inserts into an empty map" $
                let map  = empty :: m Int String in
                let new_map = Map.insertWithKey (\k new old -> show k ++ new ++ old) 1 "PF" map in
                Map.lookup 1 new_map @?= Just "PF",

            testCase "insertWithKey alters if value exists" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.insertWithKey (\k new old -> show k ++ new ++ old) 1 " new " map in
                Map.lookup 1 new_map @?= Just "1 new PF",
 
            testCase "insertWith inserts into a not empty map" $
                let map = singleton 1 "PF" :: m Int String in
                let new_map = Map.insertWithKey (\k new old -> show k ++ new ++ old) 2 "S" map in
                Map.lookup 2 new_map @?= Just "S" 
        ], 

        testGroup "Unite test for delete" [
            testCase "delete doesn't change an empty map" $
                let map  = empty :: m Int String in
                let new_map = Map.delete 1 map in
                Map.null new_map @?= True,

            testCase "delete doesn't change a map if key doesn't exist" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.delete 2 map in
                (Map.size new_map     == 1         &&
                 Map.lookup 1 new_map == Just "PF") @?= True,

            testCase "delete deletes if key exists" $
                let map  = Map.fromList [(1, "PF"), (2, "S")] :: m Int String in
                let new_map = Map.delete 1 map in
                (Map.size new_map     == 1 &&
                 Map.lookup 2 new_map == Just "S") @?= True
        ], 

        testGroup "Unite test for adjust" [
            testCase "adjust doesn't change an empty map" $
                let map  = empty :: m Int String in
                let new_map = Map.adjust ("new " ++) 1 map in
                Map.null new_map @?= True,

            testCase "adjust doesn't change a map if key doesn't exist" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.adjust ("new " ++) 2 map in
                (Map.size new_map     == 1 &&
                 Map.lookup 1 new_map == Just "PF") @?= True,

            testCase "adjust updates value if key exists" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.adjust ("new " ++) 1 map in
                (Map.size new_map     == 1 &&
                 Map.lookup 1 new_map == Just "new PF") @?= True
        ], 

        testGroup "Unite test for adjustWithKey" [
            testCase "adjustWithKey doesn't change an empty map" $
                let map  = empty :: m Int String in
                let new_map = Map.adjustWithKey (\k x -> show k ++ " new " ++ x) 1 map in
                Map.null new_map @?= True,

            testCase "adjustWithKey doesn't change a map if key does not exist" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.adjustWithKey (\k x -> show k ++ " new " ++ x) 2 map in
                (Map.size new_map     == 1 &&
                 Map.lookup 1 new_map == Just "PF") @?= True,

            testCase "adjustWithKey updates value if key exists" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.adjustWithKey (\k x -> show k ++ " new " ++ x) 1 map in
                (Map.size new_map     == 1 &&
                 Map.lookup 1 new_map == Just "1 new PF") @?= True
        ], 

        testGroup "Unite test for update" [
            testCase "update doesn't change an empty map" $
                let map  = empty :: m Int String in
                let new_map = Map.update (\x -> if x == "PF" then Just "new PF" else Nothing) 1 map in
                Map.null new_map @?= True,

            testCase "update doesn't change a map if key does not exist" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.update (\x -> if x == "PF" then Just "new PF" else Nothing) 2 map in
                (Map.size new_map     == 1 &&
                 Map.lookup 1 new_map == Just "PF") @?= True,

            testCase "update updates value if key exists" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.update (\x -> if x == "PF" then Just "new PF" else Nothing) 1 map in
                (Map.size new_map     == 1 &&
                 Map.lookup 1 new_map == Just "new PF") @?= True,

            testCase "update deletes the key if function has Nothing as a return value" $
                let map  = singleton 1 "S" :: m Int String in
                let new_map = Map.update (\x -> if x == "PF" then Just "new PF" else Nothing) 1 map in
                Map.null new_map @?= True
        ], 

        testGroup "Unite test for updateWithKey" [
            testCase "updateWithKey does nothing on an empty map" $
                let map  = empty :: m Int String in
                let new_map = Map.updateWithKey (\k x -> if x == "PF" then Just (show k ++ "new PF") else Nothing) 1 map in
                Map.null new_map @?= True,

            testCase "updateWithKey does nothing if key does not exist" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.updateWithKey (\k x -> if x == "PF" then Just (show k ++ " new PF") else Nothing) 2 map in
                (Map.size new_map     == 1 &&
                Map.lookup 1 new_map == Just "PF") @?= True,

            testCase "updateWithKey updates the value if the key exists" $
                let map  = singleton 1 "PF" :: m Int String in
                let new_map = Map.updateWithKey (\k x -> if x == "PF" then Just (show k ++ " new PF") else Nothing) 1 map in
                (Map.size new_map     == 1 &&
                 Map.lookup 1 new_map == Just "1 new PF") @?= True,

            testCase "updateWithKey deletes the key if function returns Nothing" $
                let map  = singleton 1 "S" :: m Int String in
                let new_map = Map.updateWithKey (\k x -> if x == "PF" then Just (show k ++ "new PF") else Nothing) 1 map in
                Map.null new_map @?= True
        ], 

        testGroup "Unite test for member" [
            testCase "member returns False on an empty map" $
                let map = empty :: m Int String in
                Map.member 1 map @?= False,

            testCase "member returns False if key does not exist" $
                let map  = singleton 1 "PF" :: m Int String in
                Map.member 2 map @?= False,

            testCase "member returns True if key exists" $
                let map  = singleton 1 "PF" :: m Int String in
                Map.member 1 map @?= True
        ], 

        testGroup "Unite test for notMember" [
            testCase "notMember returns True on an empty map" $
                let map = empty :: m Int String in
                Map.notMember 1 map @?= True,

            testCase "notMember returns True if key does not exist" $
                let map  = singleton 1 "PF" :: m Int String in
                Map.notMember 2 map @?= True,

            testCase "notMember returns False if key exists" $
                let map  = singleton 1 "PF" :: m Int String in
                Map.notMember 1 map @?= False
        ], 

        testGroup "Unite test for null and size" [
            testCase "empty returns an empty map" $
                let map = empty :: m Int String in
                Map.null map @?= True,

            testCase "singleton returns a singleton map" $
                let map = singleton 1 "PF" :: m Int String in
                Map.size map @?= 1
        ] 
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
