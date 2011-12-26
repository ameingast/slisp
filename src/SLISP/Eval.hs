module SLISP.Eval where

import SLISP.Data

import Char
import Maybe

import qualified Data.Map as M

listEval            ::  (SymbolTable,[E]) -> [(SymbolTable,E)]
listEval (t,[])     =   []
listEval (t,(e:es)) =   let (t',e') = eval (t,e) in (t',e') : listEval (t',es)

lastState       ::  [(SymbolTable,E)] -> Maybe (SymbolTable,[E])
lastState []    =   Nothing
lastState xs    =   Just (last $ map fst xs, map snd xs)

eval                    ::  (SymbolTable,E) -> (SymbolTable,E)
eval (t,I i)            =   (t,I i)
eval (t,S s)            =   evalSymbol (t,(S s)) []
eval (t,ST s)           =   (t,ST s)
eval (t,Q e)            =   (t,e)
eval (t,F e)            =   let (_,e') = eval (t,e) in (t, F e')
eval (t,K k e)          =   let (t',e') = eval (t,e) in (t',K k e')
eval (t,L [])           =   (t, L [])
eval (t,L ((S e):es))   =   evalSymbol (t,S e) es
eval (t,L ((L l):es))   =   let (t',l') = eval (t,L l) in eval (t',L (l':es))
eval (t,L es)           =   error $ "Illegal function call: " ++ show es

evalSymbol              ::  (SymbolTable,E) -> [E] -> (SymbolTable,E)
evalSymbol (t,S s) es   =   let els = map snd $ listEval (t,es)
    in  case symbolType t s of
            BuiltinSymbol ->
                let f = fromJust $ lookup s builtin in (t,f els)
            BuiltinStateSymbol ->
                let f = fromJust $ lookup s builtinSpecial in f (t,es)
            ExternalSymbol ->
                let (args,expr) = fromJust $ M.lookup s t
                    els' = keyOrder args els
                    pairs = zip args (map quote els')
                in  eval (t,apply expr pairs)
            NoSymbol ->
                error $ "Unbound symbol: '" ++ s ++ "'"

keyOrder                ::  [String] -> [E] -> [E]
keyOrder [] exprs       =   exprs
keyOrder (a:as) exprs   =   case keyOrder' a exprs of
                                Just x  -> x : (keyOrder as exprs)
                                Nothing -> keyOrder as exprs

keyOrder'                   ::  String -> [E] -> Maybe E
keyOrder' a []              =   Nothing
keyOrder' a ((K a' e):es)   |   a == a' = Just e
keyOrder' a (e:es)          =   keyOrder' a es

quote       ::  E -> E
quote (F x) =   F (Q x)
quote x     =   Q x

apply                   ::  E -> [(String,E)] -> E
apply (L l) pairs       =   L $ map (\x -> apply x pairs) l
apply (S s) pairs       |   elem s (map fst pairs)
                        =   snd $ head $ filter (\(s',_) -> s == s') pairs
apply (F (Q s)) pairs   =   F $ Q $ apply s pairs
apply x _               =   x


symbolType      ::  SymbolTable -> String -> SymbolType
symbolType t s  |   elem s (map fst builtin) = BuiltinSymbol
symbolType t s  |   elem s (map fst builtinSpecial) = BuiltinStateSymbol
symbolType t s  |   M.member s t = ExternalSymbol
symbolType t _  =   NoSymbol

builtin ::  [(String, [E] -> E)]
builtin =   [
    ("T",       tryAppend (I 1)),
    ("NIL",     tryAppend (I 0)),
    ("+",       foldl1NumOp (+)),
    ("*",       foldl1NumOp (*)),
    ("-",       foldl1NumOp (-)),
    ("/",       foldl1NumOp div),
    ("not",     toLispBool . not . fromLispBool . head),
    ("++",      mapNumOp (+1)),
    ("--",      mapNumOp (+(-1))),
    ("<=",      foldl1BoolOp (<=)),
    (">=",      foldl1BoolOp (>=)),
    ("<",       foldl1BoolOp (<)),
    (">",       foldl1BoolOp (>)),
    ("or",      toLispBool . or . map fromLispBool),
    ("and",     toLispBool . and . map fromLispBool),
    ("eq",      foldl1BoolOp (==)),
    ("car",     head . fromL . head),
    ("cdr",     L . tail . fromL. head),
    ("cons",    \(x:(L l):_) -> L (x:l)),
    ("append",  L . foldl1 (++) . map fromL),
    ("list",    L),
    ("@",       L),
    ("function",F . head)]

tryAppend       ::  E -> [E] -> E
tryAppend e []  =   e
tryAppend e xs  =   L (e:xs)

numUnOp         ::  (Integer -> Integer) -> E -> E
numUnOp f x     =   I $ f $ fromI x

numBinOp        ::  (Integer -> Integer -> Integer) -> E -> E -> E
numBinOp f x y  =   I $ f (fromI x) (fromI y)

boolBinOp       ::  (E -> E -> Bool) -> E -> E -> E
boolBinOp f x y =   toLispBool $ f x y

mapNumOp        ::  (Integer -> Integer) -> [E] -> E
mapNumOp f [x]  =   numUnOp f x
mapNumOp f xs   =   L $ map (numUnOp f) xs

foldl1NumOp         ::  (Integer -> Integer -> Integer) -> [E] -> E
foldl1NumOp f xs    =   foldl1 (numBinOp f) xs

foldl1BoolOp        ::  (E -> E -> Bool) -> [E] -> E
foldl1BoolOp f xs   =   foldl1 (boolBinOp f) xs

builtinSpecial  ::  [(String,((SymbolTable,[E]) -> (SymbolTable,E)))]
builtinSpecial  =   [
    ("defun",   lispDefun),
    ("setq",    lispSetq),
    ("let",     lispLet),
    ("funcall", lispFuncall),
    ("apply",   lispApply),
    ("eval",    lispEval),
    ("quote",   lispQuote),
    ("if",      lispIf),
    ("cond",    lispCond),
    ("lambda",  lispLambda),
    ("env",     lispEnv),
    ("error",   error . show . map snd . listEval)]

lispDefun (t,((S s):(L a):b:_)) = (M.insert s (map fromS a,b) t, S s)

lispSetq (t,((S s):e:_)) = (M.insert s ([],e) t, e)

lispLet (t, L []:es) =  (t,snd $ last $ listEval (t,es))
lispLet (t, L l:es)  =  let L [name,expr] = head l
                            t' = M.insert (fromS name) ([],expr) t
                        in  (t,snd $ lispLet (t', L (tail l):es))

lispFuncall (t,(F x):es) =  let (t',els) = fromJust $ lastState $ listEval (t,x:es)
                            in  eval (t', L $ els)

lispApply (t,[F x,Q (L l)]) =   let (t',els) = fromJust $ lastState $ listEval (t,x:l)
                                in  eval (t', L $ els)
lispApply (t,[F x,expr])    =   let (t',x') = eval (t,x)
                                    (t'',expr') = eval (t'',expr)
                                in  eval (t'', L [x',expr'])


lispEval (t, es) =  eval $ last $ listEval (t,es)

lispQuote (t,[x]) = (t,x)

lispIf (t,(c:a:b:_)) =  let (t',c') = eval (t,c)
                        in  if fromLispBool c' then eval (t',a) else eval (t',b)

lispCond (t,L [c,a]:xs) =   let (t',c') = eval (t,c)
                            in  if fromLispBool c' then eval (t',a) else lispCond (t',xs)

lispLambda (t,(L args):body:_) =    let name = newLambdaName t "lambda_0"
                                    in  (M.insert name (map fromS args,body) t,S name)

newLambdaName :: SymbolTable -> String -> String
newLambdaName t s = let (name,number) = break isDigit s
                        name' = name ++ show (read number + 1)
                    in  case M.lookup name t of
                            Just x -> newLambdaName t name'
                            Nothing -> name'

lispEnv (t,x:xs) =  let (t', x') = eval (t, x)
                    in  (t, snd $ fromJust $ M.lookup (fromS x') t')
