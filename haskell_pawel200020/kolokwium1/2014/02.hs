data Op = Add | Mul | Neg
data CT a = Empty | Leaf a | Join (CT a) Op (CT a)

wf :: CT a -> Bool
wf Empty = False
wf (Join Empty Add r) = False
wf (Join l Add Empty) = False
wf (Join Empty Mul r) = False
wf (Join l Mul Empty) = False
wf (Join Empty Neg Empty) = False
wf (Join l Neg Empty) = wf l
wf (Join Empty Neg r) = wf r
wf (Join l Neg r) = False
wf (Join a _ b) = wf a && wf b
wf _ = True

eval :: Num a=> CT a -> a
eval tree = if not (wf tree) then error ("incorret tree") else eval2 tree

eval2 :: Num a=> CT a -> a
eval2 (Leaf a) = a
eval2 (Join a Add b) = eval2 a + eval2 b
eval2 (Join a Mul b) = eval2 a * eval2 b
eval2 (Join a Neg Empty) = negate (eval2 a)
eval2 (Join Empty Neg a) = negate (eval2 a)