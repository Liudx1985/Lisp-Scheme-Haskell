data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
----------------------------------------------------
singleton :: a -> Tree a 
singleton x = Node x EmptyTree EmptyTree 

----------------------------------------------------
treeInsert :: (Ord a) => a -> Tree a -> Tree a 
treeInsert x EmptyTree = singleton x 
treeInsert x (Node a left right) 
      | x == a = Node x left right 
      | x < a  = Node a (treeInsert x left) right 
      | x > a  = Node a left (treeInsert x right)

----------------------------------------------------
treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem x EmptyTree = False 
treeElem x (Node a left right) 
    | x == a = True 
    | x < a  = treeElem x left 
    | x > a  = treeElem x right

instance Functor Tree where 
    fmap f EmptyTree = EmptyTree 
    fmap f (Node x leftsub rightsub) = 
        Node (f x) (fmap f leftsub) (fmap f rightsub)
        
main = do 
	let nums = [8,6,4,1,7,3,5] 
	let numsTree = foldr treeInsert EmptyTree nums 	
	print numsTree
	print (8 `treeElem` numsTree)