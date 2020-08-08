import           Data.List
import           Data.Ord
import           Lib
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck         as SC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps]

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ ut01
  , ut02
  , ut03
  , ut06
  , ut07
  , ut08
  , ut09
  , ut10
  , ut11
  , ut12
  , ut13
  , ut14
  , ut15
  , ut16
  , ut17
  , ut18
  , ut19
  , ut20
  , ut21
  ]

scProps :: TestTree
scProps = testGroup
  "(checked by SmallCheck)"
  [ SC.testProperty "Problem 04 : myLength"
    $ \list -> myLength (list :: [Int]) == length list
  , SC.testProperty "Problem 05 : myReverse"
    $ \list -> myReverse (list :: [Int]) == reverse list
  , SC.testProperty "Problem 11 & 13: encode"
    $ \list -> encodeModified (list :: [Int]) == encodeDirect list
  ]

ut01 :: TestTree
ut01 = testGroup
  "Problem 01: myLast"
  [ testCase "Length >= 1" $ myLast [1, 2, 3] @?= Just 3
  , testCase "Empty list" $ myLast [] @?= (Nothing :: Maybe Int)
  ]

ut02 :: TestTree
ut02 = testGroup
  "Problem 02: myButLast"
  [ testCase "Length >= 2" $ myButLast [1, 2, 3] @?= Just 2
  , testCase "Single element" $ myButLast [1] @?= (Nothing :: Maybe Int)
  , testCase "Empty list" $ myButLast [] @?= (Nothing :: Maybe Int)
  ]

ut03 :: TestTree
ut03 = testGroup
  "Problem 03: elementAt"
  [ testCase "Valid index" $ elementAt [1, 2, 3] 2 @?= Just 2
  , testCase "Invalid index" $ elementAt [1, 2] 3 @?= (Nothing :: Maybe Int)
  ]

ut06 :: TestTree
ut06 = testGroup
  "Problem 06: isPalindrome"
  [ testCase "Empty list" $ isPalindrome "" @?= True
  , testCase "Single element" $ isPalindrome [1] @?= True
  , testCase "Multiple elements, palindrome" $ isPalindrome "ABCBA" @?= True
  , testCase "Multiple elements, non-palindrome"
  $   isPalindrome [1, 2, 2, 3]
  @?= False
  ]

ut07 :: TestTree
ut07 = testGroup
  "Problem 07: flatten"
  [ testCase "Single element" $ flatten (Elem 1) @?= [1]
  , testCase "Nested list"
  $ flatten (List [List [List [Elem 1, Elem 2], Elem 3], List [Elem 4, Elem 5]])
  @?= [1, 2, 3, 4, 5]
  ]

ut08 :: TestTree
ut08 = testGroup
  "Problem 08: compress"
  [ testCase "Non-empty list" $ compress "aabbccaaad" @?= "abcad"
  , testCase "Empty list" $ compress [] @?= ([] :: [Int])
  ]

ut09 :: TestTree
ut09 = testGroup
  "Problem 09: pack"
  [ testCase "Non-empty list"
  $   pack [1, 1, 2, 2, 3, 4]
  @?= [[1, 1], [2, 2], [3], [4]]
  , testCase "Empty list" $ pack "" @?= []
  ]

ut10 :: TestTree
ut10 = testGroup
  "Problem 10: encode"
  [ testCase "Non-empty list"
  $   encode "aaabbc"
  @?= [(3, 'a'), (2, 'b'), (1, 'c')]
  , testCase "Empty list" $ encode [] @?= ([] :: [(Int, Int)])
  ]

ut11 :: TestTree
ut11 = testGroup
  "Problem 11: encodeModified"
  [ testCase "Non-empty list"
  $   encodeModified "aaabbc"
  @?= [Multiple 3 'a', Multiple 2 'b', Single 'c']
  , testCase "Empty list" $ encodeModified [] @?= ([] :: [Elem Int])
  ]

ut12 :: TestTree
ut12 = testGroup
  "Problem 12: decodeModified"
  [ testCase "Non-empty list"
  $   decodeModified [Multiple 3 3, Single 1, Multiple 2 2]
  @?= [3, 3, 3, 1, 2, 2]
  , testCase "Empty list" $ decodeModified [] @?= ([] :: [Int])
  ]

ut13 :: TestTree
ut13 = testGroup
  "Problem 13: encodeDirect"
  [ testCase "Non-empty list"
  $   encodeDirect "aaabbc"
  @?= [Multiple 3 'a', Multiple 2 'b', Single 'c']
  , testCase "Empty list" $ encodeDirect [] @?= ([] :: [Elem Int])
  ]

ut14 :: TestTree
ut14 = testGroup
  "Problem 14: dupli"
  [ testCase "Empty list" $ dupli [] @?= ([] :: [Int])
  , testCase "Non-empty list" $ dupli "abc" @?= "aabbcc"
  ]

ut15 :: TestTree
ut15 = testGroup
  "Problem 15: repli"
  [ testCase "Empty list" $ repli [] 3 @?= ([] :: [Int])
  , testCase "Non-empty list" $ repli "abc" 3 @?= "aaabbbccc"
  ]

ut16 :: TestTree
ut16 = testGroup
  "Problem 16: dropEvery"
  [ testCase "Normal drop" $ dropEvery [1, 2, 3, 4] 2 @?= [1, 3]
  , testCase "Large drop" $ dropEvery [1, 2, 3, 4] 5 @?= [1, 2, 3, 4]
  , testCase "Negative drop" $ dropEvery [1, 2, 3, 4] (-1) @?= ([] :: [Int])
  ]

ut17 :: TestTree
ut17 = testGroup
  "Problem 17: split"
  [ testCase "Normal split" $ split [1, 2, 3, 4, 5] 2 @?= ([1, 2], [3, 4, 5])
  , testCase "Large split" $ split [1, 2, 3, 4, 5] 6 @?= ([1, 2, 3, 4, 5], [])
  , testCase "Zero split" $ split [1, 2, 3, 4, 5] 0 @?= ([], [1, 2, 3, 4, 5])
  , testCase "Empty list" $ split [] 0 @?= ([] :: [Int], [] :: [Int])
  ]

ut18 :: TestTree
ut18 = testGroup
  "Problem 18: slice"
  [ testCase "Normal slice" $ slice [1, 2, 3, 4, 5] 2 4 @?= [2, 3, 4]
  , testCase "Left overflow" $ slice [1, 2, 3, 4, 5] (-1) 2 @?= [1, 2]
  , testCase "Right overflow" $ slice [1, 2, 3, 4, 5] 4 7 @?= [4, 5]
  , testCase "Left over right" $ slice [1, 2, 3, 4, 5] 4 3 @?= ([] :: [Int])
  , testCase "Empty list" $ slice [] 1 2 @?= ([] :: [Int])
  ]

ut19 :: TestTree
ut19 = testGroup
  "Problem 19: rotate"
  [ testCase "Left rotate" $ rotate [1, 2, 3, 4, 5] 2 @?= [3, 4, 5, 1, 2]
  , testCase "Right rotate" $ rotate [1, 2, 3, 4, 5] (-1) @?= [5, 1, 2, 3, 4]
  , testCase "Empty list" $ rotate [] 1 @?= ([] :: [Int])
  ]

ut20 :: TestTree
ut20 = testGroup
  "Problem 20: removeAt"
  [ testCase "Normal removal"
  $   removeAt [1, 2, 3, 4, 5] 2
  @?= (Just 2, [1, 3, 4, 5])
  , testCase "Left overflow"
  $   removeAt [1, 2, 3, 4, 5] 0
  @?= (Nothing, [1, 2, 3, 4, 5])
  , testCase "Right overflow"
  $   removeAt [1, 2, 3, 4, 5] 6
  @?= (Nothing, [1, 2, 3, 4, 5])
  , testCase "Empty list" $ removeAt [] 0 @?= (Nothing, [] :: [Int])
  ]

ut21 :: TestTree
ut21 = testGroup
  "Problem 21: insertAt"
  [ testCase "Normal insertion" $ insertAt 3 [1, 2, 4, 5] 3 @?= [1, 2, 3, 4, 5]
  , testCase "Left overflow" $ insertAt 1 [2, 3, 4, 5] (-1) @?= [1, 2, 3, 4, 5]
  , testCase "Right overflow" $ insertAt 5 [1, 2, 3, 4] 6 @?= [1, 2, 3, 4, 5]
  ]
