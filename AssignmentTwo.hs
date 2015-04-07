import VStack

data IExpr = Waarde Int
           | Telop IExpr IExpr
           | Vermenigvuldig IExpr IExpr

data BExpr = BWaarde Bool
           | En BExpr BExpr
           | Of BExpr BExpr

data Expr = BExpr | IExpr

zetom :: Expr -> [VStackExpr]