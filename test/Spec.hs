import           Test.Tasty
import           TypesSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unittests]

properties :: TestTree
properties = testGroup "Properties"
  [ typesProperties
  ]

unittests :: TestTree
unittests = testGroup "Unittests"
  [ typesTests
  ]


