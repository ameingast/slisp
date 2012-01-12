module SLISP.Eval(
  eval, 
  listEval
) where

import SLISP.Data

import Char(isDigit)
import Maybe(fromJust)
import qualified Data.Map as DM(Map, insert, delete, lookup, member)

listEval :: ListState -> [State]
listEval (_t,[]) = []
listEval (t,(e:es)) = let (t',e') = eval (t,e) in (t',e') : listEval (t',es)

lastState :: [State] -> Maybe (ListState)
lastState [] = Nothing
lastState xs = Just (last $ map fst xs, map snd xs)

eval :: State -> State
eval (t, Fixnum i) = (t, Fixnum i)
eval (t, Floatnum f) = (t, Floatnum f)
eval (t, Symbol s) = evalSymbol (t, (Symbol s)) []
eval (t, Str s) = (t, Str s)
eval (t, Quote e) = (t,e)
eval (t, Function e) = let (_,e') = eval (t,e) in (t, Function e')
eval (t, Key k) = (t, Key k)
eval (t, Map (Key k) e) = let (t',e') = eval (t,e) in (t', Map (Key k) e')
eval (_, Map _k _e) = error $ "Illegal eval call: Map (Symbol Expression) expected" 
eval (t, List []) = (t, List [])
eval (t, Infinity) = (t, Infinity)
eval (t, NegInfinity) = (t, NegInfinity)
eval (t, List ((Symbol e):es)) = evalSymbol (t, Symbol e) es
eval (t, List ((List l):es)) = let (t',l') = eval (t, List l) in eval (t', List (l':es))
eval (_, List es) = error $ "Illegal eval call: " ++ show es

evalSymbol :: State -> [Expression] -> State
evalSymbol (t, Symbol s) es =
  let els = map snd $ listEval (t,es)
  in case symbolType t s of
    NoSymbol -> error $ "Unbound symbol: '" ++ s ++ "'"
    BuiltinSymbol -> let f = fromJust $ lookup s builtin in (t,f els)
    BuiltinStateSymbol -> let f = fromJust $ lookup s builtinSpecial in f (t,es)
    ExternalSymbol ->
      let (args, expr) = fromJust $ DM.lookup s t
          els' = keyOrder args els
          pairs = zip args (map quote els')
      in eval (t,apply expr pairs)
evalSymbol _ _ = error "Type error: symbol expected."


keyOrder :: [String] -> [Expression] -> [Expression]
keyOrder [] exprs = exprs
keyOrder (a:as) exprs = 
  case keyOrder' a exprs of
    Just x  -> x : (keyOrder as exprs)
    Nothing -> keyOrder as exprs

keyOrder' :: String -> [Expression] -> Maybe Expression
keyOrder' _ [] = Nothing
keyOrder' a ((Map (Key a') e):_) | a == a' = Just e
keyOrder' a (_:es) = keyOrder' a es

quote :: Expression -> Expression
quote (Function x) = Function (Quote x)
quote x = Quote x

apply :: Expression -> [(String, Expression)] -> Expression
apply (List l) pairs = List $ map (\x -> apply x pairs) l
apply (Symbol s) pairs | elem s (map fst pairs) = snd $ head $ filter (\(s',_) -> s == s') pairs
apply (Function (Quote s)) pairs = Function $ Quote $ apply s pairs
apply x _ = x

symbolType :: SymbolTable -> String -> SymbolType
symbolType _ s | elem s (map fst builtin) = BuiltinSymbol
symbolType _ s | elem s (map fst builtinSpecial) = BuiltinStateSymbol
symbolType t s | DM.member s t = ExternalSymbol
symbolType _ _ = NoSymbol

builtin :: [(String, [Expression] -> Expression)]
builtin = [
    ("T", tryAppend (Fixnum 1)),
    ("NIL", tryAppend (Fixnum 0)),
    ("INF", tryAppend Infinity),
    ("-INF", tryAppend NegInfinity),
    ("+", foldl1NumOp (+)),
    ("*", foldl1NumOp (*)),
    ("-", foldl1NumOp (-)),
    ("/", foldl1FractOp (/)),
    ("div", foldl1NumOp div),
    ("not", toLispBool . not . fromLispBool . head),
    ("++", mapNumOp (+1)),
    ("--", mapNumOp (+(-1))),
    ("<=", foldl1BoolOp (<=)),
    (">=", foldl1BoolOp (>=)),
    ("<", foldl1BoolOp (<)),
    (">", foldl1BoolOp (>)),
    ("mod", foldl1NumOp (mod)),
    ("or", toLispBool . or . map fromLispBool),
    ("and", toLispBool . and . map fromLispBool),
    ("eq", foldl1BoolOp (==)),
    ("car", head . unlist . head),
    ("cdr", List . tail . unlist. head),
    ("cons", lispCons),
    ("append", List . foldl1 (++) . map unlist),
    ("key", lispKey),
    ("value", lispValue),
    ("list", List),
    ("@", List),
    ("function", Function . head)]

tryAppend :: Expression -> [Expression] -> Expression
tryAppend e [] = e
tryAppend e xs = List (e:xs)

numUnOp :: (Integer -> Integer) -> Expression -> Expression
numUnOp f x = Fixnum $ f $ fromFixnum x

numBinOp :: (Integer -> Integer -> Integer) -> Expression -> Expression -> Expression
numBinOp f x y = Fixnum $ f (fromFixnum x) (fromFixnum y)

boolBinOp :: (Expression -> Expression -> Bool) -> Expression -> Expression -> Expression
boolBinOp f x y = toLispBool $ f x y

mapNumOp :: (Integer -> Integer) -> [Expression] -> Expression
mapNumOp f [x] = numUnOp f x
mapNumOp f xs = List $ map (numUnOp f) xs

foldl1NumOp :: (Integer -> Integer -> Integer) -> [Expression] -> Expression
foldl1NumOp f xs = foldl1 (numBinOp f) xs

foldl1FractOp :: (Double -> Double -> Double) -> [Expression] -> Expression
foldl1FractOp f xs = foldl1 (fractBinOp f) xs

fractBinOp :: (Double -> Double -> Double) -> Expression -> Expression -> Expression
fractBinOp f x y = Floatnum $ f (fromFloatnum x) (fromFloatnum y)

foldl1BoolOp :: (Expression -> Expression -> Bool) -> [Expression] -> Expression
foldl1BoolOp f xs = foldl1 (boolBinOp f) xs

lispCons :: [Expression] -> Expression
lispCons (x:(List l):_) = List (x:l)
lispCons _ = error "Type error: (expression . list) expected."

lispKey :: [Expression] -> Expression
lispKey ((Map (Key k) _):_) = Symbol k
lispKey _ = error "Type error: Key expected."

lispValue :: [Expression] -> Expression
lispValue ((Map _ v):_) = v
lispValue _ = error "Type error: Key expected."

builtinSpecial :: [(String, (ListState -> State))]
builtinSpecial = [
    ("defun", lispDefun),
    ("setq", lispSetq),
    ("unsetq", lispUnsetq),
    ("let", lispLet),
    ("funcall", lispFuncall),
    ("apply", lispApply),
    ("eval", lispEval),
    ("quote", lispQuote),
    ("if", lispIf),
    ("cond", lispCond),
    ("lambda", lispLambda),
    ("env", lispEnv),
    ("error", error . show . map snd . listEval)]

lispDefun :: ListState -> State
lispDefun (t, ((Symbol s):(List a):b:_)) = (DM.insert s (map show a, b) t, Symbol s)
lispDefun _ = error "Type error: (Symbol . List . Expression) expected."

lispSetq :: ListState -> State
lispSetq (t ,((Symbol s):e:_)) = (DM.insert s ([],e) t, e)
lispSetq _ = error "Type error: (Symbol . Expression) expected."

lispUnsetq :: ListState -> State
lispUnsetq (t, ((Symbol s):_)) = (DM.delete s t, Fixnum 1)
lispUnsetq _ = error "Type error: Symbol expected."

lispLet :: ListState -> State
lispLet (t, List []:es) = (t,snd $ last $ listEval (t,es))
lispLet (t, List l:es) =
  let List [name,expr] = head l
      t' = DM.insert (show name) ([], expr) t
  in (t,snd $ lispLet (t', List (tail l):es))
lispLet _ = error "Type error: List expected."

lispFuncall :: ListState -> State
lispFuncall (t, (Function x):es) =
  let (t',els) = fromJust $ lastState $ listEval (t,x:es)
  in eval (t', List els)
lispFuncall _ = error "Type error: Function expected."

lispApply :: ListState -> State
lispApply (t,[Function x, Quote (List l)]) = 
  let (t',els) = fromJust $ lastState $ listEval (t,x:l)
  in eval (t', List $ els)
lispApply (t,[Function x,expr]) = 
  let (t',x') = eval (t,x)
      (t'',expr') = eval (t',expr)
  in eval (t'', List [x',expr'])
lispApply _ = error "Type error: Function expected."

lispEval :: ListState -> State
lispEval (t, es) = eval $ last $ listEval (t,es)

lispQuote :: ListState -> State
lispQuote (t,[x]) = (t,x)
lispQuote _ = error "Type error: Expression expected."

lispIf :: ListState -> State
lispIf (t,(c:a:b:_)) =  
  let (t',c') = eval (t,c)
  in if fromLispBool c' then eval (t',a) else eval (t',b)
lispIf _ = error "Type error: (Expression . Expression . Expression) expected."

lispCond :: ListState -> State
lispCond (t, List [c,a]:xs) =
  let (t',c') = eval (t,c)
  in if fromLispBool c' then eval (t',a) else lispCond (t',xs)
lispCond _ = error "Type error: List expected."

lispLambda :: ListState -> (DM.Map String ([String], Expression), Expression)
lispLambda (t, (List args):body:_) =
  let name = newLambdaName t "lambda_0"
  in (DM.insert name (map show args,body) t, Symbol name)
lispLambda _ = error "Type error: (List . Expression) expected."

newLambdaName :: SymbolTable -> String -> String
newLambdaName t s = 
  let (name,number) = break isDigit s
      name' = name ++ show (read number + 1 :: Integer)
  in case DM.lookup name t of
    Just _ -> newLambdaName t name'
    Nothing -> name'

lispEnv :: ListState -> State
lispEnv (t, x:_) = 
  let (t', x') = eval (t, x)
      word = (fromSClear x')
      fun = DM.lookup word t'
  in case fun of
    Nothing -> error $ "No such word: " ++ word
    Just (args, body) -> (t', Symbol $ formatFun args body)
lispEnv (_, xs) = 
  error $ "Type error on call 'env': Expression expected. Got " ++ show xs ++ " instead."