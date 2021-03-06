module VStack (VStackWaarde(..), VStackExpr(..), VStack, Programma, vStack) where

-- De waarden die op de stack kunnen staan. Deze waarden worden ook geretourneerd na
-- executie.
--
data VStackWaarde = IWaarde Integer
                  | BWaarde Bool
                  | Void
                    deriving Show

-- De expressies die de vStack 'begrijpt'
data VStackExpr = IPush Integer
                | BPush Bool
                | Telop
                | Vermenigvuldig
                | En
                | Of
                | Gelijk
                | Ongelijk
                | Niet
                 deriving Show

type VStack    = [VStackWaarde]
type Programma = [VStackExpr]

-- Executie van een gegevene programma. Retourneert of een foutmelding of de
-- waarde op de top van de stack na executie.
-- 
vStack :: Programma -> Either String VStackWaarde
vStack = execute []

errType :: String -> Either String a
errType opcode = Left $ " '" ++ opcode ++ "' opcode met ill-typed stack"

errUnderflow :: String -> Either String a
errUnderflow opcode = Left $ "Stack underflow met '" ++ opcode ++ "' opcode"

-- Executie van programma met een gegeven stack.
--
execute :: VStack -> Programma -> Either String VStackWaarde
execute [] []                                            = Right Void
execute (s:_) []                                         = Right s

execute s (IPush x : xs)                                 = execute (IWaarde x : s) xs
execute s (BPush x : xs)                                 = execute (BWaarde x : s) xs

execute (IWaarde s1 : IWaarde s2 : ss) (Telop : xs)      = execute (s':ss) xs
    where s' = IWaarde (s1 + s2)
execute (_:_:_) (Telop:_)                                = errType "Telop"
execute _ (Telop:_)                                      = errUnderflow "Telop"

execute (IWaarde s1 : IWaarde s2:ss) (Vermenigvuldig : xs) = execute (s':ss) xs
    where s' = IWaarde (s1 * s2)
execute (_:_:_) (Vermenigvuldig:_)                       = errType "Vermenigvuldig"
execute _ (Vermenigvuldig:_)                             = errUnderflow "Vermenigvuldig"

execute (BWaarde s1 : BWaarde s2 : ss) (En : xs)         = execute (s':ss) xs
    where s' = BWaarde (s1 && s2)
execute (_:_:_) (En:_)                                   = errType "En"
execute _ (En:_)                                         = errUnderflow "En"

execute (BWaarde s1 : BWaarde s2 : ss) (Of : xs)         = execute (s':ss) xs
    where s' = BWaarde (s1 || s2)
execute (_:_:_) (Of:_)                                   = errType "Of"
execute _ (Of:_)                                         = errUnderflow "Of"

-- | Opdracht A3 uitbreiding

execute (IWaarde s1 : IWaarde s2 : ss) (Gelijk : xs)     = execute (s':ss) xs
    where s' = BWaarde (s1 == s2)
execute (_:_:_) (Gelijk:_)                               = errType "Gelijk"
execute _ (Gelijk:_)                                     = errUnderflow "Gelijk"

execute (IWaarde s1 : IWaarde s2 : ss) (Ongelijk : xs)   = execute (s':ss) xs
    where s' = BWaarde (s1 /= s2)
execute (_:_:_) (Ongelijk:_)                             = errType "Ongelijk"
execute _ (Ongelijk:_)                                   = errUnderflow "Ongelijk"

execute (BWaarde s1 : ss) (Niet : xs)                    = execute (s':ss) xs
    where s' = BWaarde (not s1)
execute (_:_:_) (Niet:_)                                 = errType "Niet"
execute _ (Niet:_)                                       = errUnderflow "Niet"

itest = vStack [IPush 3, IPush 5, Telop]
btest = vStack [BPush True, BPush False, En]
ntest = vStack [IPush 4, IPush 2, IPush 2, Vermenigvuldig, Gelijk]
