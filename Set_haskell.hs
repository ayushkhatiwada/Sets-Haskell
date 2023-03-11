-- Set data structure implemented in haskell 


-- Define Set data type
newtype Set a = Set [a]
    deriving (Show)


-- Transforms list into Set
-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList xs = Set (qSort (removeDup xs))

-- Quicksort algo
qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort (x:xs) = qSort [a | a <- xs, a <= x] ++ [x] ++ qSort [b | b <- xs, b > x]

-- Removes duplicates from list
removeDup :: (Ord a) => [a] -> [a]
removeDup [] = []
removeDup (x:xs) = x: removeDup (filter (/= x) xs)


-- Transforms Set into list
-- toList {2,1,4,3} => [1,2,3,4]     
toList :: Set a -> [a]              
toList (Set xs) = xs


-- Defines the equality operator (=) between 2 sets
-- Tests if two sets have the same elements.   
instance (Ord a) => Eq (Set a) where          
    s1 == s2 = qSort (removeDup (toList s1)) == qSort (removeDup (toList s2))  


-- The empty set
empty :: Set a
empty = Set []


-- Set with one element
singleton :: a -> Set a
singleton x = Set [x]


-- Inserts an element of type "a" into a Set 
insert :: (Ord a) => a -> Set a -> Set a
insert element (Set xs)
    | element `elem` xs = Set xs        
    | otherwise = Set (element : xs)


-- Joins two Sets together
union :: (Ord a) => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set ( removeDup (xs ++ ys) )


-- Returns the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set (inter xs ys)

inter :: (Ord a) => [a] -> [a] -> [a]
inter [] _ = []
inter _ [] = []
inter xs ys = filter (`elem` xs) ys


-- All the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set xs) (Set ys) = Set (diff xs ys)

diff :: (Ord a) => [a] -> [a] -> [a]            
diff xs [] = xs                                 
diff [] _ = []                                  
diff xs ys = filter (\x -> x `notElem` ys) xs   


-- Checks if element exists in Set
member :: (Ord a) => a -> Set a -> Bool
member element (Set xs) = element `elem` xs


-- Number of elements in Set
cardinality :: Set a -> Int
cardinality (Set xs) = length xs


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap func (Set xs) = Set (map func xs)


setfoldr :: (a -> b -> b) -> Set a -> b -> b                
setfoldr binaryFunc (Set xs) acc = foldr binaryFunc acc xs  


-- Powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }      
powerSet :: Set a -> Set (Set a)
powerSet (Set []) = Set [ Set [] ]
powerSet (Set (x:xs)) =  Set ( map Set (map (x:) (listPowerset xs) ++ listPowerset xs) )

listPowerset :: [a] -> [[a]]    -- powerset on lists   
listPowerset [] = [[]]
listPowerset (x:xs) = map (x:) (listPowerset xs) ++ listPowerset xs


-- Cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian (Set xs) (Set ys) = Set ([(x, y) | x <- xs, y <- ys])


-- Partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition predicate (Set xs) =  ( Set [x | x <- xs, predicate x], Set [y | y <- xs, not (predicate y)] )

