import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        david = robot "David" 20 300
        charley = robot "Charley" 100 500
        kremlinBot = robot "Kbot" 0 (-1000)
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for 'printRobot'" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for 'robot' (init Robot)" $
            robot "Henry" 20 30 @?= ("Henry", 20, 30)

        , testCase "Test for 'getName'" $
            getName david @?= "David"

        , testCase "Test for 'getAttack'" $
            getAttack david @?= 20

        , testCase "Test for 'getHealth'" $
            getHealth david @?= 300

        , testCase "Test for 'setName'" $
            setName "Bart" walter @?= ("Bart", 50, 50)

        , testCase "Test for 'setAttack'" $
            setAttack 200 walter @?= ("Walter", 200, 50)

        , testCase "Test for 'setHealth'" $
            setHealth 140 walter @?= ("Walter", 50, 140)

        , testCase "Test for 'damage'" $
            damage walter 30 @?= ("Walter", 50, 20)

        , testCase "Test for 'isAlive' when robot is alive" $
            isAlive david @?= True

        , testCase "Test for 'isAlive' when robot isn't alive" $
            isAlive kremlinBot @?= False 

        , testCase "Test for 'fight' where robot was damaged" $
            fight david walter @?= ("Walter", 50, 30)

        , testCase "Test for 'fight' where robot haven't been damaged" $
            fight kremlinBot charley @?= charley

        , testCase "Test 1 for 'threeRoundFight'" $
            threeRoundFight charley david @?= ("Charley", 100, 480)

        , testCase "Test 2 for 'threeRoundFight'" $
            threeRoundFight david walter @?= ("David", 20, 250)

        , testCase "Test for 'neueRobotAttack'" $
            neueRobotAttack david @?= ("David", 20, (-1200)) 

        , testCase "Test for 'survivors'" $
            survivors @?= [robot "Vision" 500 1500, robot "Connor" 300 500]
       ]
