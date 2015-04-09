import VStack

data IExpr = IWaarde Integer
           | Telop IExpr IExpr
           | Vermenigvuldig IExpr IExpr

data BExpr = BWaarde Bool
           | En BExpr BExpr
           | Of BExpr BExpr

data Expr = BExpr BExpr | IExpr IExpr

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


-- A.

-- 1.
zetom :: Expr -> [VStackExpr]
zetom (BExpr x) = zetom' x
	where 
		zetom' (Main.BWaarde x) = [BPush x]
		zetom' (Main.En x y) = (zetom' x) ++ (zetom' y) ++ [VStack.En]
		zetom' (Main.Of x y) = (zetom' x) ++ (zetom' y) ++ [VStack.Of]
zetom (IExpr x) = zetom' x
	where
		zetom' (Main.IWaarde x) = [IPush x]
		zetom' (Main.Telop x y) = (zetom' x) ++ (zetom' y) ++ [VStack.Telop]
		zetom' (Main.Vermenigvuldig x y) = (zetom' x) ++ (zetom' y) ++ [VStack.Vermenigvuldig]


-- 2.
evalueer :: Expr -> Either String VStackWaarde
evalueer = vStack . zetom


-- B.

isCompleteTree :: (Tree x) -> Bool
isCompleteTree (Node _ EmptyTree EmptyTree) = True
isCompleteTree (Node _ EmptyTree _) = False
isCompleteTree (Node _ _ EmptyTree) = False
isCompleteTree (Node _ left right) = True && isCompleteTree left && isCompleteTree right

