module TypesSpec
  ( typesTests
  , typesProperties
  ) where

import           Data.Matrix
import           Honeypot.Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

instance Arbitrary Pos where
  arbitrary = do
    (Positive y) <- arbitrary
    (Positive x) <- arbitrary
    return (P y x)

instance Arbitrary Dir where
  arbitrary = elements [West, East, North, South]

instance Arbitrary Enemy where
  arbitrary = E <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Player where
  arbitrary = Player <$> arbitrary <*> arbitrary <*> arbitrary

instance (Num a) => Arbitrary (Matrix a) where
  arbitrary = do
    (P y x) <- arbitrary
    return (zero y x)

typesTests :: TestTree
typesTests = testGroup "TypeTests"
  [ stepTests
  ]

typesProperties :: TestTree
typesProperties = testGroup "TypeProperties"
  [ stepProperties
  ]

singlePosEnemy = E [] (P 0 0) []

firstStep = E [P 1 1] (P 0 0) []

secondStep = E [] (P 1 1) [P 0 0]

stepTests :: TestTree
stepTests = testGroup "stepTests"
  [ testCase "returns same enemy if path contains only one pos" $
    step singlePosEnemy @?= singlePosEnemy
  , testCase "returns enemy with current pos in past" $
    step firstStep @?= secondStep
  ]

stepProperties :: TestTree
stepProperties = testGroup "stepProperties"
  [ testProperty "after some steps enemy reaches initial state" stepProperty
  ]

stepProperty :: Enemy -> Bool
stepProperty enemy = elem enemy $ drop 1 (iterate step enemy)

playerViewProperties :: TestTree
playerViewProperties = testGroup "playerViewProperties"
  [ testProperty "size of playerSteps equals player distance from matrix edge" playerViewProperty
  ]

playerViewProperty :: Matrix a -> Player -> Property
playerViewProperty m p@(Player dir (P y x) _) =
  nrows m >= y && ncols m >= x ==>
  let edgeDist = case dir of
                   East  -> length (getRow y m) - x
                   West  -> x
                   South -> length (getCol x m) - y
                   North -> y
   in length (playerView m p) == edgeDist



