-- Do Tree Traversals and Built a Visitation List for each

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left root right) = root : preorder left ++ preorder right
-- NOTE: Need to use the ++ so each list gets built separately and then concatenated
-- after it hits bottom

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left root right) = inorder left ++ [root] ++ inorder right
-- NOTE:  Need to put root in its own list so we can concatenate

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left root right) = postorder left ++ postorder right ++ root : []
-- Similarly, here. Need to Cons root onto a list, since it'll be the bottom value
