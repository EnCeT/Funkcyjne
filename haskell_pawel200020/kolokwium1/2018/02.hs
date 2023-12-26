data Expr a = Value a
    | Add (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | P
eq :: (Eq a)=> Expr a -> Expr a -> Bool
eq _ P = True
eq P _ = True
eq (Value a) (Value b) = b==a
eq (Add a b) (Add x y) = eq a x && eq b y
eq (Mul a b) (Mul x y) = eq a x && eq b y
eq (Sub a b) (Sub x y) = eq a x && eq b y
eq _ _ = False