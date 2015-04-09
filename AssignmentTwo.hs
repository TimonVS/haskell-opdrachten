import VStack

data IExpr = IWaarde Integer
           | Telop IExpr IExpr
           | Vermenigvuldig IExpr IExpr

data BExpr = BWaarde Bool
           | En BExpr BExpr
           | Of BExpr BExpr

data Expr = BExpr BExpr | IExpr IExpr

-- A.

-- 1.
-- | De `zetom` functie zet `IExpr` en `BExpr` (van type `Expr`) om in een lijst van `VStackExpr`. 
-- `zetom` wordt recursive aangeroepen bij `En`, `Of`, `Telop` en `Vermenigvuldig` om de input waardes om te zetten in `[VStackExpr]` zodat deze geconcatenate kunnen worden.

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

-- | Test voor `zetom`
zetomTest = zetom (IExpr (Main.IWaarde 5))
zetomTest2 = zetom (IExpr (Main.Telop (Main.IWaarde 5) (Main.IWaarde 7)))
zetomTest3 = zetom (IExpr (Main.Telop (Main.Telop (Main.IWaarde 1) (Main.IWaarde 2)) (Main.IWaarde 3)))


-- 2.
-- | 

evalueer :: Expr -> Either String VStackWaarde
evalueer = vStack . zetom


-- B.
-- | `isCompleteTree` gebruikt pattern matching om te kijken of een `Node` gelijk is (beide `EmptyTree` of een `Node` bevatten) en wordt vervolgens recursive aangeroepen.

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

isCompleteTree :: (Tree x) -> Bool
isCompleteTree (Node _ EmptyTree EmptyTree) = True
isCompleteTree (Node _ EmptyTree _) = False
isCompleteTree (Node _ _ EmptyTree) = False
isCompleteTree (Node _ left right) = True && isCompleteTree left && isCompleteTree right

-- | Test for `isCompleteTree`
isCompleteTreeTest = isCompleteTree (Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree))
isCompleteTreeTest2 = isCompleteTree (Node 1 (Node 2 (Node 4 EmptyTree EmptyTree) EmptyTree) (Node 3 EmptyTree EmptyTree))





