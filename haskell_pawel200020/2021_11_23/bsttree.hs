data BST a = Empty | Node a (BST a) (BST a) deriving Show

insertElement :: Ord a => a -> BST a -> BST a
insertElement a Empty = Node a Empty Empty
insertElement a (Node b left right) = if(b > a) then insertElement a left else insertElement a right

makeTree :: Ord a => [a] -> BST a
makeTree [] = Empty
makeTree (x : xs) = insertElement x (makeTree xs)

findElement :: Ord a=> a -> BST a -> BST a
findElement a Empty = Node a Empty Empty
findElement a (Node b left right)
  | b==a = Node b left right
  | b > a = findElement a left
  | b < a = findElement a right
-- subSize :: Ord a => a -> BST a -> Int
-- sub
