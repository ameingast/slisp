module SLISP.Data where
    
import qualified Data.Map as DM(Map, empty)
    
data Expression =      
  Fixnum Integer            |
  Symbol String             |
  Str String                |
  Quote Expression          |
  Function Expression       |
  Key String                |
  Map Expression Expression |
  Floatnum Double           |
  Infinity                  |
  NegInfinity               |
  List [Expression]       
            
instance Show Expression where
  show (Fixnum i) = show i
  show (Symbol s) = s
  show (Str s) = show s
  show (Quote e) = "'" ++ show e
  show (Function e) = "#" ++ show e
  show (Key k) = ":" ++ k
  show (Map k e) = show k ++ " " ++ show e
  show (List l) = "(" ++ unwords (map show l) ++ ")"
  show (Floatnum f) = show f
  show Infinity = "INF"
  show NegInfinity = "-INF"
    
instance Eq Expression where
  (Fixnum i) == (Fixnum j) = i == j
  (Floatnum a) == (Floatnum b) = a == b
  (Symbol s) == (Symbol t) = s == t
  (Str s) == (Str t) = s == t
  (Quote e) == (Quote g) = e == g
  (Function e) == (Function g) = e == g
  (List l) == (List k) = l == k
  (Key k) == (Key kk) = k == kk
  (Map k e) == (Map l f) = k == l && e == f
  Infinity == Infinity = True
  NegInfinity == NegInfinity = True
  _ == _ = False
    
instance Ord Expression where
  compare (Fixnum i) (Fixnum j) = compare i j
  compare (Symbol s) (Symbol t) = compare s t
  compare (Str s) (Str t) = compare s t
  compare (Floatnum a) (Floatnum b) = compare a b
  compare Infinity Infinity = EQ
  compare Infinity _ = GT
  compare NegInfinity NegInfinity = EQ
  compare NegInfinity _ = LT
  compare _ _ = LT

type Signature = ([String], Expression)
type SymbolTable = DM.Map String Signature

data SymbolType = 
  BuiltinSymbol       |
  BuiltinStateSymbol  |
  ExternalSymbol      |
  NoSymbol

type State = (SymbolTable, Expression)
type ListState = (SymbolTable, [Expression])

emptyTable :: DM.Map k a
emptyTable = DM.empty
                  
-- FIXME: fromExpression
fromFixnum :: Expression -> Integer
fromFixnum = read . show

fromFloatnum :: Expression -> Double
fromFloatnum = read . show

fromSClear :: Expression -> String
fromSClear (Symbol x) = x
fromSClear (Str x) = x
fromSClear x = show x

unlist :: Expression -> [Expression]
unlist (List l) = l
unlist x = error $ "Cannot unlist " ++ show x

fromLispBool :: Expression -> Bool
fromLispBool (Fixnum i) = i /= 0
fromLispBool (List l) = l /= []
fromLispBool _ = True

toLispBool :: Bool -> Expression
toLispBool True = Fixnum 1
toLispBool False = Fixnum 0

formatFun :: [String] -> Expression -> String
formatFun args body = "(lambda(" ++ unwords args ++ ") " ++ show body ++ ")"