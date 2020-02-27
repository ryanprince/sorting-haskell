module InsertionSort (insertionSort) where

-- Some impelementations of insertion sort are approximately O(n) for
-- nearly-sorted lists. This one is not, though it is about O(n) for
-- nearly-reverse-sorted lists.

-- Creates a sorted list by including an element in an already-sorted list.
insert :: Ord a => a -> [a] -> [a]
insert x sorted = let left  = takeWhile (<x) sorted
                      right = drop (length left) sorted
                  in left ++ x:right

insertionSort :: Ord a => [a] -> [a]
insertionSort xs =
  let iSort :: Ord a => [a] -> [a] -> [a]
      iSort sorted []           = sorted
      iSort sorted (x:unsorted) = iSort (insert x sorted) unsorted
  in iSort [] xs
