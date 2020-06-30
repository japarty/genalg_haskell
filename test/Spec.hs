import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Test obliczania przystosowania" $ do
        it "zwraca 0 dla [0,0,0,0] i [0,0,0,0]" $
            fitness [0,0,0,0] [0,0,0,0] `shouldBe` 0
        it "zwraca 4 dla [0,0,0,0] i [1,1,1,1]" $
            fitness [0,0,0,0] [1,1,1,1]`shouldBe` 4
        it "zwraca 8 dla [-1,-1,-1,-1] i [1,1,1,1]" $
            fitness [-1,-1,-1,-1] [1,1,1,1]`shouldBe` 8
        it "zwraca 20 dla [-1,-2,3,4] i [1,2,-3,-4]" $
            fitness [-1,-2,3,4] [1,2,-3,-4] `shouldBe` 20
        it "zwraca 150 dla [-108,25,13,-4] i [0,0,0,0]" $
            fitness [-108,25,13,-4] [0,0,0,0] `shouldBe` 150
            
    describe "Test mutowania" $ do
        it "zwraca [1,0,0,0] dla [0,0,0,0], 0 i 1" $
            mute [0,0,0,0] 0 1 `shouldBe` [1,0,0,0]
        it "zwraca [0,-5,0,0] dla [0,0,0,0], 1 i -5" $
            mute [0,0,0,0] 1 (-5) `shouldBe` [0,-5,0,0]
        it "zwraca [0,0,10,0] dla [0,0,0,0], 2 i 10" $
            mute [0,0,0,0] 2 10 `shouldBe` [0,0,10,0]
        it "zwraca [0,0,0,1] dla [0,0,0,0], 3 i 1" $
            mute [0,0,0,0] 3 1 `shouldBe` [0,0,0,1]
            
    describe "Test krzyżowania" $ do
        it "zwraca ([1,1,1,1],[0,0,0,0]) dla [0,0,0,0], [1,1,1,1] i 0" $
            cross [0,0,0,0] [1,1,1,1] 0 `shouldBe` ([1,1,1,1],[0,0,0,0])
        it "zwraca ([0,1,1,1],[1,0,0,0]) dla [0,0,0,0], [1,1,1,1] i 1" $
            cross [0,0,0,0] [1,1,1,1] 1 `shouldBe` ([0,1,1,1],[1,0,0,0])
        it "zwraca ([0,0,1,1],[1,1,0,0]) dla [0,0,0,0], [1,1,1,1] i 2" $
            cross [0,0,0,0] [1,1,1,1] 2 `shouldBe` ([0,0,1,1],[1,1,0,0])
        it "zwraca ([0,0,0,1],[1,1,1,0]) dla [0,0,0,0], [1,1,1,1] i 3" $
            cross [0,0,0,0] [1,1,1,1] 3 `shouldBe` ([0,0,0,1],[1,1,1,0])
        it "zwraca ([1,1,1,1],[0,0,0,0]) dla [0,0,0,0], [1,1,1,1] i 4" $
            cross [0,0,0,0] [1,1,1,1] 0 `shouldBe` ([1,1,1,1],[0,0,0,0])