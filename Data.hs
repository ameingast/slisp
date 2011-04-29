module Data where
    
import qualified Data.Map as M
    
data E =    I Integer   |
            S String    |
            ST String   |
            Q E         |
            F E         |
            K String E  |
            L [E]       
            
instance Show E where
    show (I i)      = show i
    show (S s)      = s
    show (ST s)     = show s
    show (Q e)      = "'" ++ show e
    show (F e)      = "#" ++ show e
    show (K k e)    = ":" ++ k ++ " " ++ show e
    show (L l)      = "(" ++ unwords (map show l) ++ ")"
    
instance Eq E where
    (I i) == (I j)      = i == j
    (S s) == (S t)      = s == t
    (ST s) == (ST t)    = s == t
    (Q e) == (Q g)      = e == g
    (F e) == (F g)      = e == g
    (L l) == (L k)      = l == k
    (K k e) == (K l f)  = k == l && e == f
    _ == _              = False
    
instance Ord E where
    compare (I i) (I j) = compare i j

type Signature      =   ([String],E)
type SymbolTable    =   M.Map String Signature

data SymbolType     =   BuiltinSymbol       |
                        BuiltinStateSymbol  |
                        ExternalSymbol      |
                        NoSymbol
 
emptyTable = M.empty
                  
fromI   ::  E -> Integer
fromI   =   read . show

fromS   ::  E -> String
fromS   =   show

fromST  ::  E -> String
fromST  =   show

fromQ       ::  E -> E
fromQ (Q e) =   e

fromK       ::  E -> String
fromK       =   show

fromL       ::  E -> [E]
fromL (L l) =   l

fromLispBool        ::  E -> Bool
fromLispBool (I i)  =   i /= 0
fromLispBool (L l)  =   l /= []
fromLispBool x      =   True

toLispBool          ::  Bool -> E
toLispBool True     =   I 1
toLispBool False    =   I 0