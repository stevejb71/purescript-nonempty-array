module Main where

main = do
    quickCheck $ \xs ys -> isSorted $ merge (sort xs) (sort ys)