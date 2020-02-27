module MergeSort (mergeSort) where

-- merge combines two sorted lists into a third
merge :: Ord a => [a] -> [a] -> [a]
merge as []   = as
merge [] bs   = bs
merge (a:as) (b:bs)
  | a < b     = a : merge as (b:bs)
  | otherwise = b : merge (a:as) bs

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = let leftLength  = length xs `div` 2
                    left        = take leftLength xs
                    right       = drop leftLength xs
                in merge (mergeSort left) (mergeSort right)
