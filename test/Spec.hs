import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.List (transpose, intercalate)
import Lib (diamond)

tests = [testGroup "Trivial Props" [
                  testProperty "Letter A first" prop_first_char_is_A
                , testProperty "Letter A last" prop_last_char_is_A
           ]
         , testGroup "Character elements"[
           --testProperty "Contains all letters preceding" prop_contains_all_preceding_letters
           ]
         , testGroup "Whitespace" [
           testProperty "Middle WS" prop_middle_whitespace_correct
           ]
         , testGroup "Examples" [
           testProperty "Example 'A'" prop_a_example
         --, testProperty "Example 'B'" prop_b_example
           ]
         , testGroup "Symmetry" [
           testProperty "bordering whitespace" prop_bordering_whitespace_is_symmetric
         , testProperty "vertical boerdering WS" prop_vertical_whitespace_is_symmetric
         , testProperty "symmetric along horizontal" prop_horizontally_symmetric
           ]
         , testGroup "letters" [
           testProperty "letters appear on left side" prop_correct_letter_order_left_side
         , testProperty "letters appear on left side" prop_correct_letter_order_right_side
           ]
        ]

newtype Char' = Char' Char
  deriving (Show)
instance Arbitrary Char' where
  arbitrary = Char' <$> (elements ['A'..'Z'])
  --shrink (Char' c) =  map Char' $ init ['A'..c]
  
newtype NonTrivial = NT Char
  deriving (Show)

instance Arbitrary NonTrivial where
  arbitrary = NT <$> (elements ['C'..'Z'])
  shrink (NT c) =  map NT $ init ['C'..c]
  
  
fstChar = head . dropWhile (== ' ')

lastChar = fstChar . reverse

nonWS = filter (not . (`elem` ['\n', ' ']))

splitD s = splitAt ( (length $ lines s) `div` 2 + 1) $ lines s
-- === is a quickcheck thing
prop_a_example = diamond 'A' === "A"

prop_b_example = diamond 'B' ===  " A " ++ "\n"
                            ++ "B B" ++ "\n"
                            ++ " A "
                            
prop_first_char_is_A :: Char' -> Property
prop_first_char_is_A (Char' c) = (fstChar $ diamond c) === 'A'
prop_first_line_is_only_a :: Char' -> Property
prop_first_line_is_only_a (Char' c) = (nonWS $ diamond c) === "A"

prop_last_char_is_A :: Char' -> Property
prop_last_char_is_A  (Char' c) = (lastChar $ diamond c) === 'A'

takeSpace = takeWhile (== ' ')

prop_contains_all_preceding_letters :: Char' -> Bool  
prop_contains_all_preceding_letters (Char' c) = let res = (diamond c) in all (`elem` res) ['A'..c]

prop_bordering_whitespace_is_symmetric (Char' c) = let res = (diamond c) in all f $ lines res
  where f line = takeSpace line == (takeSpace $ reverse line)
        
prop_vertical_whitespace_is_symmetric (Char' c) = let res = (diamond c) in all f $ transpose $ lines res
  where f line = takeSpace line == (takeSpace $ reverse line)
--prop_selected_letter_in_middle :: Char' -> Bool
--prop_selected_letter_appears_twice :: Char' -> Bool
--prop_all_letter_counts_are_even :: Char' -> Bool
--prop_lines_contains_only_one_unique_letter :: Char' -> Bool
--prop_vertically_symmetric :: NonTrivial -> Bool
--prop_two_more_whitespace_line :: NonTrivial -> Bool
--prop_selected_letter_has_no_bordering_whitespace :: Char' -> Bool
prop_correct_letter_order_left_side (Char' c) = let (top, _) = (splitD $ diamond c) in
  (map fstChar top) === ['A'..c]
  
prop_correct_letter_order_right_side (Char' c) = let (top, _) = (splitD $ diamond c) in
  (map lastChar top) === ['A'..c]
  
prop_horizontally_symmetric (Char' c) = init top === reverse bottom
  where
    (top, bottom) = splitD $ diamond c
    
prop_vertically_symmetric (Char' c) = left === right
  where
    (left, right) = splitD $ transpose' $ diamond c
    transpose'  = unlines . transpose .lines
  
-- TODO: would be nice if it shrinked to smallest letter
-- is there no "OR" combinator for props? then could use `OR` (diamond c) == A
prop_middle_whitespace_correct (NT c) = counterexample (intercalate "," $ lines $ diamond c) $ conjoin $ tail $ init $ zipWith checkWS res [0..]
  where
    res = lines $ diamond c
    n = length ['A'..c]
    checkWS  xs    i = counterexample (xs ++ " @ " ++ show i) $  (safeTail $ init $ xs) === Just (replicate (i `mod` (n * 2)) ' ')
    succeed = True === True
    safeTail [] = Nothing
    safeTail (x:xs) = Just xs

    

exampleB =  " A " ++ "\n"
         ++ "B B" ++ "\n"
         ++ " A "

main = defaultMain tests
