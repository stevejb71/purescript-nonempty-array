module Main where

import Test.QuickCheck
import qualified Data.Array as A
import Data.Array.NonEmpty
import Debug.Trace
import Data.Maybe.Unsafe

foreign import undefined :: forall a. a

instance arbNonEmpty :: (Arbitrary a) => Arbitrary (NonEmpty a) where
  arbitrary = NonEmpty <$> arbitrary <*> arbitrary

qc :: String -> (NonEmpty Number -> Boolean) -> QC Unit
qc name f = do
    quickCheck (\x -> (f x) <?> name ++ " failed")
    return unit

qc_ll :: String -> (NonEmpty Number -> NonEmpty Number -> Boolean) -> QC Unit
qc_ll name f = do
    quickCheck (\x y -> (f x y) <?> name ++ " failed")
    return unit

qc_nl :: String -> (Number -> NonEmpty Number -> Boolean) -> QC Unit
qc_nl name f = do
    quickCheck (\x y -> (f x y) <?> name ++ " failed")
    return unit

main = do
    qc "Self equality" \x -> x == x
    qc "Inequality" \x -> x /= push 0 x
    qc "Length" \x -> length x == A.length (toArray x)
    qc "Pop - Length" \x -> A.length (pop x) == A.length (toArray x) - 1
    qc "Pop - Head" \(NonEmpty a as) -> let popped = pop (NonEmpty a as) in A.length popped == 0 || (fromJust $ A.head popped) == a
    qc "Take" \xs -> take (length xs) xs == toArray xs
    qc "Drop" \xs -> A.length (drop (length xs) xs) == 0
    qc "Drop 1" \xs -> drop 1 xs == tail xs
    qc "Reverse" \xs -> reverse (reverse xs) == xs
    qc_ll "Append - Length" \xs ys -> length (xs `append` ys) == length xs + length ys
    qc_nl "Cons - Length" \x xs -> length (x <| xs) == length xs + 1
    qc_nl "Cons - Head" \x xs -> head (x <| xs) == x
    qc_nl "Cons - Tail" \x xs -> tail (x <| xs) == toArray xs
    qc "Last" \(NonEmpty a as) -> let x = (NonEmpty a as) in (A.length as == 0 && last x == a) || (last x == fromJust (A.last as))
    qc "nub" \xs -> xs == nub (xs `append` xs)
    qc "nubBy" \xs -> xs == nubBy (==) (xs `append` xs)
    qc "functor - identity" \xs -> (\x -> x) <$> xs == xs 
    qc "functor - composition" \xs -> ((+) 1) <$> ((+) 2) <$> xs == ((+) 3) <$> xs