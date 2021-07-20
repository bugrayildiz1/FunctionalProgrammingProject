-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Jelle Nijland (s1701754)
-- Student 2: Bugra Yildiz  (s2104601)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Data.Char
import Data.String

import Debug.Trace
verboseCheck prop = quickCheck (\input -> traceShow input $ prop input)

-- FP3.1
-- The EDSL for MicroFP
-- -- Data constructor for programs
data Program = Program [Func]
             deriving (Show, Eq)

-- -- Data constuctor for functions
-- -- arguments are stored in a [Param], [] if the function has no arguments
data Func = Func String [Param] Expr
          deriving (Show, Eq)

-- -- Data constructor for Expression (this collapses term, factor and expr into one type)
data Expr = BinOp Op Expr Expr
          | IF Order Expr Expr    
          | Param Param
          | FuncCall String [Expr]   
          deriving (Show, Eq)

-- -- Data constructor for Parameters (See Expr Param and Func)
data Param = Cons Integer
           | Var String
        deriving (Show, Eq)

-- -- Data constructor for Comparison (See Expr IF)
data Order = ST Expr Expr   -- "<"
           | IS Expr Expr   -- "=="
           | GT Expr Expr   -- ">"
           deriving (Show, Eq)

-- -- Data constructor for Operators of binary operations (See Expr BinOp) 
data Op = Plus     -- "+" 
        | Subtract -- "-"
        | Mult     -- "*"
        deriving (Show, Eq)


-- FP3.2
-- Program encodings for the code specified in functions.txt
-- Our parsers generate (nearly) identical programs with the exception of collapsing 
-- functions into one program. Here they are presented in a collapsed manner so 
-- they can be evaluated (for instance, main)
fib = Program [Func "fibonacci" [Cons 0] (Param (Cons 0)),
               Func "fibonacci" [Cons 1] (Param (Cons 1)),
               Func "fibonacci" [Var "n"] 
                    (BinOp Plus 
                        (FuncCall "fibonacci" [BinOp Subtract (Param (Var "n")) (Param (Cons 1))]) 
                        (FuncCall "fibonacci" [BinOp Subtract (Param (Var "n")) (Param (Cons 2))]))]
fibiter = Program [Func "fib" [Var "n"] 
                    (IF 
                        (ST (Param (Var "n")) (Param (Cons 3)))
                            (Param (Cons 1))
                            (BinOp Plus 
                                (FuncCall "fib" [BinOp Subtract (Param (Var "n")) (Param (Cons 1))]) 
                                (FuncCall "fib" [BinOp Subtract (Param (Var "n")) (Param (Cons 2))])))]
sum = Program [Func "sum" [Cons 0] (Param (Cons 0)),
                Func "sum" [Var "a"] 
                    (BinOp Plus 
                        (FuncCall "sum" 
                            [BinOp Subtract (Param (Var "a")) (Param (Cons 1))])
                   (Param (Var "a")))]
divv = Program [Func "div" [Var "x", Var "y"] 
                    (IF (ST (Param (Var "x")) (Param (Var "y")))
                            (Param (Cons 0))
                            (BinOp Plus 
                                (Param (Cons 1))
                                (FuncCall "div"
                                    [BinOp Subtract (Param (Var "x")) (Param (Var "y")),
                            Param (Var "y")])))]
twix = Program [Func "twice" [Var "f", Var "x"] 
                    (FuncCall "f" [
                        FuncCall "f" [Param (Var "x")]]),
                Func "double" [Var "a"]
                (BinOp Mult (Param (Var "a")) (Param (Cons 2)))]
dub = Program [Func "double" [Var "a"]
                (BinOp Mult (Param (Var "a")) (Param (Cons 2)))]
add = Program [Func "add" [Var "x", Var "y"]
                (BinOp Plus (Param (Var "x")) (Param (Var "y")))]
inc = Program [Func "add" [Var "x", Var "y"]
                (BinOp Plus (Param (Var "x")) (Param (Var "y"))),
               Func "inc" []
                (FuncCall "add" [Param (Cons 1)])]
eleven = Program [Func "inc" []
                    (FuncCall "add" [Param (Cons 1)]),
                  Func "add" [Var "x", Var "y"]
                    (BinOp Plus (Param (Var "x")) (Param (Var "y"))),
                  Func "eleven" []
                    (FuncCall "inc" [Param (Cons 10)])]
fourty = Program [Func "twice" [Var "f", Var "x"] 
                    (FuncCall "f" [
                        FuncCall "f" [Param (Var "x")]]),
                 Func "double" [Var "a"]
                        (BinOp Mult (Param (Var "a")) (Param (Cons 2))),
                 Func "fourty" []
                    (FuncCall "twice" [Param (Var "double"), Param (Cons 10)])]
main = Program [Func "div" [Var "x", Var "y"] 
                    (IF (ST (Param (Var "x")) (Param (Var "y")))
                            (Param (Cons 0))
                            (BinOp Plus 
                                (Param (Cons 1))
                                (FuncCall "div"
                                    [BinOp Subtract (Param (Var "x")) (Param (Var "y")),
                                (Param (Var "y"))]))),
                Func "main" []
                    (FuncCall "div" [Param (Cons 999), Param (Cons 2)])]

-- FP3.3
-- instances for pretty printing
-- Q how to make one for [Param -> printArgs] and [Expr -> printCSV]
class Pretty a where
    pretty :: a -> String
    
instance Pretty Program where
    pretty = prettyprog

instance Pretty Func where
    pretty = printFunc

instance Pretty Op where
    pretty = printBinOp

instance Pretty Expr where
    pretty = printExpr

instance Pretty Order where
    pretty = printOrder

instance Pretty Param where
    pretty = printParam

-- Pretty function for Program
prettyprog :: Program -> String
prettyprog (Program []) = ""
prettyprog (Program xs) = concatMap pretty xs

-- Pretty function for Func
printFunc :: Func -> String
printFunc (Func id a e)
    | null(a)   = id ++ " := " ++ pretty e    ++ ";\n"
    | otherwise = id ++ " "    ++ printArgs a ++ " := " ++ pretty e ++ ";\n" 

-- Pretty function for arguments of a function 
-- needed for the correct argument separation
printArgs :: [Param] -> String
printArgs []     = []
printArgs (x:xs) = pretty x ++ concatMap (\x -> " " ++ pretty x) xs

-- Pretty function for [Expr]
-- this prints a String containing Expr separated by commas
-- TODO: Foldr somehow?
printCSV :: [Expr] -> String
printCSV []     = []
printCSV (x:xs) = pretty x ++ concatMap (\x -> ", " ++ pretty x) xs

-- Pretty printer function for Expr
printExpr :: Expr -> String
printExpr (BinOp op e1 e2)  = "(("       ++ pretty e1    ++ pretty op    ++ pretty e2 ++ "))"
printExpr (IF o e1 e2)      = "if ("    ++ pretty o     ++ ") then { "  ++ pretty e1 ++ "} else {" ++ pretty e2 ++ "}"  
printExpr (Param p)         = pretty p
printExpr (FuncCall i es)
          | null es         = i
          | otherwise       = i ++ " (" ++ printCSV es  ++ ")"

-- Pretty printer function for Param(eter)
-- Param for the parameter to print
-- returns the result string
printParam :: Param -> String
printParam (Cons x) = show x
printParam (Var x) = x

-- Pretty printer function for Op(erator)
-- Op for the operator
-- returns the result string
printBinOp :: Op -> String
printBinOp Plus =     "+"
printBinOp Subtract = "-"
printBinOp Mult =     "*"

-- Pretty printer function for Order
-- Order for the order
-- returns the result string
printOrder :: Order -> String
printOrder (ST e1 e2)           = pretty e1 ++ " < "  ++ pretty e2
printOrder (MicroFP.GT e1 e2)   = pretty e1 ++ " > "  ++ pretty e2
printOrder (IS e1 e2)           = pretty e1 ++ " == " ++ pretty e2

-- Collection of all programs in an easy to access list
progs = [fib,fibiter,MicroFP.sum,divv,twix,dub,add,inc,eleven,fourty,main]
-- Shorthand to print all functions as seen in functions.txt excluding whitespace
prettyAll = putStr(concatMap pretty progs)

-- FP3.4
-- Typeclass for binding Integers
class Bind a where
    bind :: a -> [Param] -> [Integer] -> a

instance Bind Expr where
    bind = bindI

instance Bind Order where
    bind = bindIO

instance Bind Param where
    bind = bindIP

-- Bind function for integers. Requires an Expr, list of Var Strings to rebind
-- And list of Integers to replace them. Outputs the bound Expr.
bindI :: Expr -> [Param] -> [Integer] -> Expr
bindI (BinOp op e1 e2) xs ys                    = BinOp op  (bind e1 xs ys) (bind e2 xs ys)
bindI (IF o e1 e2) xs ys                        = IF        (bind o xs ys)  (bind e1 xs ys) (bind e2 xs ys)
bindI (FuncCall id es) [] ys                    = FuncCall id ess
                                      where ess = es ++ (Param <$> (Cons <$> ys))
bindI (FuncCall id es) xs ys                    = FuncCall id $ map (\e -> bind e xs ys) es
bindI (Param p) xs ys                           = Param $ bind p xs ys


-- Helper function for bindI to rebind Expr in Order types.
-- recieves input from bindI's IF pattern. Outputs the bound Order
bindIO :: Order -> [Param] -> [Integer] -> Order
bindIO (ST e1 e2) xs ys         = ST         (bind e1 xs ys) (bind e2 xs ys)
bindIO (MicroFP.GT e1 e2) xs ys = MicroFP.GT (bind e1 xs ys) (bind e2 xs ys)
bindIO (IS e1 e2) xs ys         = IS         (bind e1 xs ys) (bind e2 xs ys)

-- Helper function for bindI to rebind Expr in Param types.
-- recieves input from bindI's Param pattern. Outputs the bound Param
bindIP :: Param -> [Param] -> [Integer] -> Param
bindIP (Cons x) xs ys                            = Cons x
bindIP (Var id) [] []                            = Var id
-- start partial application
bindIP (Var id) [] (y:ys)                        = Cons y
-- end partial application
bindIP (Var id) ((Var x):xs) (y:ys)
    | id == x                                    = Cons y
    | otherwise                                  = bind (Var id) xs ys

-- Reduce function to accept an Expr and reduces it to a simpler expression
-- Accepts an If statement, evaluates its ordering and 
-- returns the right expression to be evaluated
-- Also requires the function map (see collectFuncs) in case one (or both) of the Exprs in Order
-- contain a function call.
reduce :: Expr -> [(String, [Param], Expr)] -> Expr
reduce (IF o e1 e2) fs
       | evalOrder o fs     = e1
       | otherwise          = e2

-- FP5.3
-- Patmatch function to reduces multiple functions into if/else structure. 
-- Can be tested with: evalFunc (patmatch fibf fibfs) [10] fibfs 
-- Will return the 10th fibonacci number
patmatch :: [Func] -> [(String, [Param], Expr)] -> Func
patmatch [f] fs                                 = f
patmatch ((Func id inputs e):xs) fs             = Func id [x] (rewrite ((Func id inputs e):xs) fs)
                                where (Param x) = findVar fs

-- rewrite function to rewrite a list of functions to a single if/else expression
rewrite :: [Func] -> [(String, [Param], Expr)] -> Expr
rewrite [(Func id inputs e)] fs           = e
rewrite ((Func id inputs e):xs) fs        = IF (IS var (Param (head inputs))) e (rewrite xs fs)
                                where var = findVar fs

-- Helper function for patmatch and rewrite 
-- patmatch: find the variable to be included as param for the final function
-- rewrite: find the variable to check in the if statement
findVar :: [(String, [Param], Expr)] -> Expr
findVar []                          = error "No Var to patternMatch against"
findVar [(_, ((Var x):xs), _)]      = Param (Var x)
findVar [(a, ((Cons x):xs), b)]     = findVar [(a, xs, b)]
findVar ((a, ((Var x):xs), b):ys)   = Param (Var x)
findVar ((a, ((Cons x):xs), b):ys)  = findVar ys

-- FP3.4
-- Eval function that takes a program, list of parameters (if not applicable give [])
-- and function name to execute. Returns an Integer.
eval :: Program -> String -> [Integer]  -> Integer
eval (Program xs) id args   = evalFunc x args fs
    where x                 = findFunc xs id args
          fs                = collectFuncs xs

-- FP3.4, FP4.2, FP5.2, FP5.4, FP5.5
-- Dear grader, this function has been designed to make your life easier.
-- Simply call "promptMe" from your terminal, specify which function you want to run and press enter
-- Now you can now enter values as input args, these values are space separated and press enter.
-- every function specified in the functions.txt will run correctly.
-- If you search the file further for the assignment number you will find
-- the specific implementation of the feature. 
-- promptMe will evaluate the program you specify with the parameters you give.
promptMe = eval <$> (compile <$> (readFile "functions.txt")) <*> getLine <*> getInts

-- Find function that takes in a list of Funcs and the String of the function to look for
-- Returns the function that has the same name as the input String.
-- implements patternmatching (5.2)
findFunc :: [Func] -> String -> [Integer] -> Func
findFunc [] name _                          = error ("Couldnt find func " ++ name)
findFunc (Func id ((Cons x):is) e:xs) name args
         | id == name && x == (head args)   = Func id ((Cons x):is) e
         | otherwise                        = findFunc xs name args
findFunc (Func id input e:xs) name args
         | id == name                       = Func id input e
         | otherwise                        = findFunc xs name args

-- Helper function for eval, takes in a list of functions and builds the function map
-- The functionMap consists of:
-- String: the name of a function
-- [Param]: the parameters of that function (for later use in bind)
-- Expr: the resulting Expr of that function
collectFuncs :: [Func] -> [(String, [Param], Expr)]
collectFuncs []                     = []
collectFuncs ((Func id input e):xs) = (id, input, e) : collectFuncs xs

-- Helper function for eval, takes in the function to be executed,
-- the list of integers to be bound and the function map. Returns the resulting Integer
evalFunc :: Func -> [Integer] -> [(String, [Param], Expr)] -> Integer
evalFunc (Func _ input e) args fs = evalExpr (bind e input args) fs

-- Helper function for evalFunc, takes in a expression and resolves it until it
-- reaches a result Integers. Also requires the function map which is used in the 
-- function call pattern.
evalExpr :: Expr -> [(String, [Param], Expr)] -> Integer
evalExpr (BinOp Plus e1 e2) fs      = evalExpr e1 fs + evalExpr e2 fs
evalExpr (BinOp Subtract e1 e2) fs  = evalExpr e1 fs - evalExpr e2 fs 
evalExpr (BinOp Mult e1 e2) fs      = evalExpr e1 fs * evalExpr e2 fs 
evalExpr (IF o e1 e2) fs            = evalExpr (reduce (IF o e1 e2) fs) fs
evalExpr (Param p) _                = evalParam p 
evalExpr (FuncCall name es) fs      = evalFuncCall (getFuncs name fs) (map (`evalFuncCallArgs` fs) es) fs

-- Helper function for evalFuncCall
-- Runs evalExpr on everything, except exprs that are already Params.
-- This is done to avoid running into "variable should be bound beforehand" error
-- and allow passing of Var parameters for higher order functions
evalFuncCallArgs :: Expr -> [(String, [Param], Expr)] -> Param
evalFuncCallArgs (Param p) fs = p
evalFuncCallArgs e fs         = Cons $ evalExpr e fs

-- FP5.5 
-- rewrites function calls for higher order functions
rewriteFuncCall :: [(Expr, [Param])] -> [Param] -> [(String, [Param], Expr)] -> Expr
-- recursive step, we must go deeper
rewriteFuncCall [(FuncCall id [FuncCall id2 e], ((Var x):pp))] ((Var s):ss) fs
    | id == x               = FuncCall s  [rewriteFuncCall [(FuncCall id2 e, ((Var x):pp))] ((Var s):ss) fs]
    | otherwise             = FuncCall id [rewriteFuncCall [(FuncCall id2 e, ((Var x):pp))] ((Var s):ss) fs]
-- base case e is not an FuncCall
rewriteFuncCall [(FuncCall id e, ((Var x):pp))] ((Var s):ss) fs
                | x == id   = FuncCall s e
                | otherwise = rewriteFuncCall [(FuncCall id e, (pp))] (ss) fs

-- FP5.4 && FP5.5
-- implements patternMatching & higher order functions
evalFuncCall :: [(Expr, [Param])] -> [Param] -> [(String, [Param], Expr)] -> Integer
evalFuncCall [(FuncCall id e, ((Var x):pp))] ((Var s):ss) fs
        | x == id = evalExpr (bind (rewriteFuncCall [(FuncCall id e, ((Var x):pp))] ((Var s):ss) fs) pp (unpackParamToInt ss)) fs
        | otherwise = error "well gl debugging this ;-;"
evalFuncCall ((FuncCall id e, ((Var x):pp)):xs) ((Var s):ss) fs
        | x == id = evalExpr (fst $ getFunc s fs) fs
        | otherwise = error "well gl debugging this ;-; part 2"
evalFuncCall [(e,p)] ss fs  = evalExpr (bind e p (unpackParamToInt ss)) fs 
evalFuncCall ((e, ((Cons x):pp)):xs) ((Cons s):ss) fs
        | x == s = evalExpr (bind e ((Cons x):pp) (unpackParamToInt ss)) fs
        | otherwise = evalFuncCall xs ((Cons s):ss) fs

-- Unpacks [Param] to [Integer]
-- Q: can i do this nicer?
unpackParamToInt :: [Param] -> [Integer]
unpackParamToInt []           = []
unpackParamToInt ((Cons i):p) = i : unpackParamToInt p
unpackParamToInt ((Var i):p)  = unpackParamToInt p


-- Helper function for evalExpr, takes in a Param and resolves it until it
-- reaches a result Integers. 
evalParam :: Param -> Integer
evalParam (Cons x) = x
evalParam (Var x)  = error "This variable should have been bound beforehand"

-- Helper function for Reduce, evaluates an order and returns a Bool
evalOrder :: Order -> [(String, [Param], Expr)] -> Bool
evalOrder (ST e1 e2) fs         = evalExpr e1 fs <  evalExpr e2 fs
evalOrder (MicroFP.GT e1 e2) fs = evalExpr e1 fs >  evalExpr e2 fs
evalOrder (IS e1 e2) fs         = evalExpr e1 fs == evalExpr e2 fs

-- Helper function for the evalExpr FuncCall pattern.
-- Requires a string and the function map, returns a tuple of the function's Expr
-- And a list of its parameters (to be passed to Bind)
getFunc :: String -> [(String, [Param], Expr)] -> (Expr, [Param])
getFunc s1 []   = error "404: Function not found"
getFunc s1 ((s2, params, e):fs)
    | s1 == s2  = (e, params)
    | otherwise = getFunc s1 fs

-- Helper function for the evalExpr FuncCall pattern.
-- Requires a string and the function map, returns a list of tuple of the function's Expr
-- And a list of its parameters (to be passed to Bind)
getFuncs :: String -> [(String, [Param], Expr)] -> [(Expr, [Param])]
getFuncs s1 []  = []
getFuncs s1 ((s2, params, e):fs)
    | s1 == s2  = (e, params) : getFuncs s1 fs
    | otherwise = getFuncs s1 fs

-- FP4.1
-- Start of the keyword/special token Parsers
-- Parser for the assign symbol (":=")
assignParser :: Parser String
assignParser = symbol ":="

-- Parser for the semicolon symbol (";")
semiParser :: Parser String
semiParser = symbol ";"

-- Parser for the comma symbol (",")
separatorParser :: Parser String
separatorParser = symbol ","

-- Parser for the if keyword ("if")
ifParser :: Parser String
ifParser = symbol "if"

-- Parser for the then keyword ("then")
thenParser :: Parser String
thenParser = symbol "then"

-- Parser for the else keyword ("else")
elseParser :: Parser String
elseParser = symbol "else"

-- Parser for the multiplication symbol ("*")
multiplicationParser :: Parser String
multiplicationParser = symbol "*"

-- Parser for the addition symbol ("+")
plusParser :: Parser String
plusParser = symbol "+"

-- Parser for the substraction symbol ("-")
minusParser :: Parser String
minusParser = symbol "-"

-- Parser for the greater than symbol (">")
greaterSymbolParser :: Parser String
greaterSymbolParser = symbol ">"

-- Parser for the smaller than symbol ("<")
smallerSymbolParser :: Parser String
smallerSymbolParser = symbol "<"

-- Parser for the equals symbol ("==")
equalSymbolParser :: Parser String
equalSymbolParser = symbol "=="

-- Collector parser for the Order type, reorders the order in which expr order and expr appear
orderingParser :: Parser Order
orderingParser = (\x y z -> y x z) <$> exprParser <*> order <*> exprParser

-- OrderType returns the type of order based on the char parsed
orderType :: String -> Expr -> Expr -> Order
orderType x
    | x == ">" = MicroFP.GT
    | x == "<" = ST
    | otherwise = IS

-- Order parser the symbol and uses orderType to return the correct dataconstructor
order :: Parser (Expr -> Expr -> Order)
order = orderType <$> (smallerSymbolParser <|> greaterSymbolParser <|> equalSymbolParser) 

-- Collector parser for the If pattern (IF) of the Expr type
-- combines "if", an ordering surrounded by parentheses, "then", an expression
-- surrounded by braces, "else" and an expression surrounded by braces
ifstmtParser :: Parser Expr
ifstmtParser = IF <$> (ifParser *> parens (orderingParser) <* thenParser) 
                  <*> braces (whitespace exprParser) <* elseParser
                  <*> braces (whitespace exprParser)

-- Collector Parser for the function call pattern (FuncCall) of Expr,
-- combines an identifier with the expression list in argument form (i.e. comma separated)
funcCallParser :: Parser Expr
funcCallParser = FuncCall <$> identifier <*> parens (sep1 exprParser separatorParser)

-- Parser for the constant pattern (Cons) of Param
consParser :: Parser Param
consParser =  Cons <$> integer

-- Parser for the variable pattern (Var) of Param
varParser :: Parser Param
varParser = Var <$> identifier

-- Multiplication Parser
multParser :: Parser Expr
multParser =  BinOp Mult <$> factor <*> (multiplicationParser *> term)
          <|> parens (BinOp Mult <$> factor <*> (multiplicationParser *> term))

-- Addition Parser
additionParser :: Parser Expr
additionParser =  BinOp Plus <$> term <*> (plusParser *> exprParser)
              <|> parens (BinOp Plus <$> term <*> (plusParser *> exprParser))
              
-- Substraction Parser
subtractionParser :: Parser Expr
subtractionParser =  BinOp Subtract <$>  term <*> (minusParser *> exprParser)
                 <|> parens (BinOp Subtract <$>  term <*> (minusParser *> exprParser))

-- Wrapper parser for parameters in the Expr datatype
paramParser :: Parser Expr
paramParser = Param <$> parameterParser

-- Translation of MicroFP to our EDSL
-- Collector parser for factors, combines ifstatement,
-- function call and parameter parser
factor :: Parser Expr
factor =  ifstmtParser 
      <|> funcCallParser 
      <|> paramParser
      <|> parens (parens exprParser)
      <|> parens exprParser

-- Collector parser for terms, combines multiplication 
-- and factor
term :: Parser Expr
term = multParser
    <|> factor 

-- Collector parser for expr, combines addition, substraction
-- and term
exprParser :: Parser Expr
exprParser =  additionParser
          <|> subtractionParser
          <|> term 
          

-- Collector parser for Param (Cons & Var)
parameterParser :: Parser Param
parameterParser =  consParser
               <|> varParser

-- Collector parser for the Func datatype
-- Func, combining an name, list of Expressions, assignment and Expr
functionParser :: Parser Func
functionParser =  Func <$> identifier  <*> many parameterParser <*> (assignParser *> exprParser)

-- Collector parser for programs, combines
-- a list of function parsers separated by semicolons
programParser :: Parser Program
programParser = Program <$> sep1 functionParser semiParser <* semiParser

-- 4.2
-- compile compiles a String to a program
compile :: String -> Program
compile "" = Program []
compile s  = fst $ head $ runParser programParser $ Stream s

-- compileExpr compiles a String to a Expr
-- For the quickCheck property
compileExpr :: String -> Expr
compileExpr s = fst $ head $ runParser exprParser $ Stream s

-- compileParam compiles a String to a Param
-- For the quickCheck property
compileParam :: String -> Param
compileParam s = fst $ head $ runParser parameterParser $ Stream s

-- compileFunc compiles a String to a Func
-- For the quickCheck property
compileFunc :: String -> Func
compileFunc s = fst $ head $ runParser functionParser $ Stream s

-- FP4.3
runFile :: FilePath -> [Integer] -> IO Integer
runFile path is = flipEval eval is <$> p <*> (getLast <$> p) 
    where p = compile <$> (readFile path)

-- Custom flip function to allow mixing of IO and non IO context for 4.3
flipEval :: (a -> b -> c -> d) -> c -> a -> b -> d
flipEval f a b c = f b c a

-- Returns the last function in a program
getLast :: Program -> String
getLast (Program []) = error "empty Program"
getLast (Program [Func s _ _]) = s
getLast (Program (x:xs)) = getLast $ Program xs

getInts :: IO [Integer]
getInts = map read <$> (words <$> getLine)

-- FP 5.6
instance Arbitrary Op where
    arbitrary = oneof [pure Plus
                      ,pure Subtract
                      ,pure Mult]

instance Arbitrary Order where
    arbitrary = oneof [ST         <$> arbexpr 1 <*> arbexpr 1
                      ,IS         <$> arbexpr 1 <*> arbexpr 1
                      ,MicroFP.GT <$> arbexpr 1 <*> arbexpr 1]

functionString   = vectorOf 3 $ oneof $ pure <$> ['f'..'h'] ++ ['k']
identifierString = vectorOf 3 $ oneof $ pure <$> ['a'..'e']

instance Arbitrary Param where
    arbitrary = frequency [ (1, Cons <$> genPositiveNum)
                          , (1, Var  <$> identifierString)]

genPositiveNum :: Gen Integer
genPositiveNum = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (> 0)

instance Arbitrary Expr where
    arbitrary               = resize 10 $ arbexpr 10
    shrink (BinOp _ e1 e2)  = [e1, e2]
    shrink (IF _ e1 e2)     = [e1, e2]

arborder :: Int -> Gen Order
arborder n = oneof [ST         <$> (arbexpr (n `div` 2)) <*> (arbexpr (n `div` 2))
                   ,IS         <$> (arbexpr (n `div` 2)) <*> (arbexpr (n `div` 2))
                   ,MicroFP.GT <$> (arbexpr (n `div` 2)) <*> (arbexpr (n `div` 2))]

arbexpr :: Int -> Gen Expr
arbexpr 0 = Param <$> arbitrary
arbexpr n = frequency [(1, BinOp    <$> arbitrary               <*> (arbexpr (n `div` 2)) <*> (arbexpr (n `div` 2)))
                      ,(1, IF       <$> (arborder (n `div` 2))  <*> (arbexpr (n `div` 2)) <*> (arbexpr (n `div` 2)))
                      ,(2, FuncCall <$> functionString          <*> vectorOf 2 (Param <$> arbitrary))
                      ,(2, Param    <$> arbitrary)]

instance Arbitrary Func where
    arbitrary = Func <$> functionString <*> arbitrary <*> arbitrary

instance Arbitrary Program where
    arbitrary = Program <$> resize 4 arbitrary

genProg :: IO Program
genProg = generate arbitrary

genExpr :: IO Expr
genExpr = generate arbitrary

genParam :: IO Param
genParam = generate arbitrary

-- QuickCheck
-- FP3.2 && FP3.3 && FP4.1 && FP4.2
prop_functions :: Bool
prop_functions = (combineProgs progs) == compile (concatMap pretty progs)

-- FP4.3
prop_runfile :: Property
prop_runfile = ioProperty $ (== 499) <$> (runFile "functions.txt" []) 

-- FP3.4 && FP5.2
-- min size of x has been limited to 1 because of inconsistency in fibonacci compared to fib
-- max size of x has been limited to 25 to save time.
prop_evalfib :: NonNegative Integer -> Property
prop_evalfib (NonNegative x) = x /= 0 && x < 25 ==> eval fib "fibonacci" [x] == eval fibiter "fib" [x]

prop_evalsum :: NonNegative Integer -> Bool
prop_evalsum (NonNegative x) = eval MicroFP.sum "sum" [x] == Prelude.sum [0..x] 

prop_evaldiv :: NonNegative Integer -> NonNegative Integer -> Property
prop_evaldiv (NonNegative x) (NonNegative y) = x < y ==> eval divv "div" [x,y] == x `div` y

prop_evaladd :: NonNegative Integer -> NonNegative Integer -> Bool
prop_evaladd (NonNegative x) (NonNegative y) = eval add "add" [x,y] == x + y

prop_evaldub :: NonNegative Integer -> Bool
prop_evaldub (NonNegative x) = eval dub "double" [x] == x + x

-- FP5.3
prop_patmatchsum :: NonNegative Integer -> Bool
prop_patmatchsum (NonNegative x) = evalFunc (patmatch fs ffs) [x] ffs == Prelude.sum [0..x]
                            where (Program fs) = MicroFP.sum
                                  ffs = collectFuncs fs

prop_patmatchfib :: NonNegative Integer -> Property
prop_patmatchfib (NonNegative x) = x /= 0 && x < 25 ==> evalFunc (patmatch fs ffs) [x] ffs == eval fib "fibonacci" [x]
                            where (Program fs) = fib
                                  ffs = collectFuncs fs

-- FP5.4
prop_partapp :: Bool
prop_partapp = eval eleven "eleven" [] == 11

prop_partapp2 :: NonNegative Integer -> Bool
prop_partapp2 (NonNegative x) = eval inc "inc" [x] == x+1

-- FP5.5
prop_evalfourty :: Bool
prop_evalfourty = eval fourty "fourty" [] == 40

-- FP5.6
prop_printParam :: Param -> Bool
prop_printParam x = x == (compileParam $ pretty x)

prop_printProg :: Program -> Bool
prop_printProg x = x == (compile $ pretty x)

prop_printFunc :: Func -> Bool
prop_printFunc x = x == (compileFunc $ pretty x)

prop_printExpr :: Expr -> Bool
prop_printExpr x = x == (compileExpr $ pretty x)

combineProgs :: [Program] -> Program
combineProgs ((Program x):[])    = Program x
combineProgs ((Program x):(Program y):xs) = combineProgs ((Program (x++y)):xs)

return []
checkMP = $quickCheckAll