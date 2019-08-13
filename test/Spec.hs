import           Honeypot.Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unittests]

properties :: TestTree
properties = testGroup "Properties"
  [ typeProperties
  ]

unittests :: TestTree
unittests = testGroup "Unittests"
  [ typesTests
  ]


-- Types

instance Arbitrary Pos where
  arbitrary = do
    (Positive y) <- arbitrary
    (Positive x) <- arbitrary
    return (P y x)

instance Arbitrary Enemy where
  arbitrary = E <$> arbitrary <*> arbitrary <*> arbitrary

typesTests:: TestTree
typesTests = testGroup "TypeTests"
  [ stepTests
  ]

typeProperties :: TestTree
typeProperties = testGroup "TypeProperties"
  [ stepProperties
  ]

singlePosEnemy = E [] (P 0 0) []

firstStep = E [(P 1 1)] (P 0 0) []

secondStep = E [] (P 1 1) [(P 0 0)]

stepTests :: TestTree
stepTests = testGroup "stepTests"
  [ testCase "returns same enemy if path contains only one pos" $
    step singlePosEnemy @?= singlePosEnemy
  , testCase "returns enemy with current pos in past" $
    step firstStep @?= secondStep
  ]

stepProperties :: TestTree
stepProperties = testGroup "stepProperties"
  [ testProperty "after some steps enemy reaches initial state" $
    \enemy -> any (== enemy) $ drop 1 (iterate step enemy)
  ]
