-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Jelle Nijland (s1701754)
-- Student 2: Bugra Yildiz  (s2104601)


{-
            Points  Description
   FP2.1    5 (S)   Define the parsers letter :: Parser Char that parses any (alphabetical) letter, and
                    dig :: Parser Char that parses any digit.
   FP2.2    2 (M)   The following parser combinators:
            [1pt] between :: Parser a -> Parser b -> Parser c -> Parser b runs the three
                    parsers in sequence, and returns the result of the second parser. Similar to
                    between in ParSec.
            [1pt] whitespace :: Parser a -> Parser a receives a parser p and uses it to parse
                    the input stream, while skipping all surrounding whitespaces (space, tab and
                    newline). For example, whitespace (char ’a’) can be used to parse the input
                    ’a’ surrounded by whitespace.
   FP2.3    3 (S)   The following parser combinators:
            [1pt] sep1 :: Parser a -> Parser b -> Parser [a]. The parser sep1 p s parses
                    one or more occurrences of p, separated by s. This can, for example, be used
                    to parse a comma separated list.
            [1pt] sep :: Parser a -> Parser b -> Parser [a]. The parser sep p s works as
                    sep1 p s, but parses zero or more occurrences of p.
            [1pt] option :: a -> Parser a -> Parser a. option x p tries to apply parser p;
                    upon failure it results in x. Similar to option in ParSec.
   FP2.4    10 (S)  The following parsers and combinators:
            [3pt] string :: String -> Parser String parses an given String, similar to
                    the function char.
            [2pt] identifier :: Parser String parses a given identifier surrounded by
                    whitespace.
            [2pt] integer :: Parser Integer parses an integer surrounded by whitespace.
            [1pt] symbol :: String -> Parser String parses a given String surrounded
                    by whitespaces.
            [1pt] parens :: Parser a -> Parser a parses something using the provided
                    parser between parentheses, i.e., (...).
            [1pt] braces :: Parser a -> Parser a parses something using the provided
                    parser between braces.
        Total 20
-}

{-# LANGUAGE TemplateHaskell #-}

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb
--import FPPrac.Trees

-- FP2.1
-- Defining the alphanumeric characters that will be supported in MicroFP
alphabet        = ['a'..'z'] ++ ['A'..'Z']
digit           = ['0'..'9']
ws              = " \t\n\r"
letters         = char <$> alphabet
digits          = char <$> digit
whitespaces     = char <$> ws
 
-- Parser to parse all letters availble in MicroFP
letter :: Parser Char
letter = foldl1 (<|>) letters

-- Parser to parse all digits available in MicroFP
dig :: Parser Char
dig = foldl1 (<|>) digits

--FP2.2
-- Between parsers a Stream using applicative style and only returns the middle parsers
-- This is the base parser combination to allow parsing of whitespace, braces and parentheses
between :: Parser a -> Parser b -> Parser c -> Parser b 
between p1 p2 p3 = p1 *> p2 <* p3

-- Whitespace parser to throw away whitespace surrounding a Stream
whitespace :: Parser a -> Parser a
whitespace p = between wp p wp
  where wp = many (foldl1 (<|>) whitespaces)

-- FP2.3
-- Seperator parser accepts two parsers, one which will be seen as the separator element 
-- between two instances of the the initial type parser. Returns a parser of type list
-- Parses one or more occurences of p, separated by s.
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (:) <$> p  <*> many (s *> p)

-- Parses zero or more occurences of p, separated by s.
sep :: Parser a -> Parser b -> Parser [a]
sep p s  = option [] $ sep1 p s

-- Tries to apply parser p upon failure it results in x.
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- FP2.4
-- String parser to parse a String. Passes all chars of a string to char
-- and collapses them into a Parser String
string :: String -> Parser String
string = foldr (\ x -> (<*>) ((:) <$> char x)) $ pure []

-- Identifier parser parses an identifier (consisting of a letter followed by 0 or more letters/digits)
identifier :: Parser String
identifier = whitespace $ (:) <$> letter <*> many (letter <|> dig)

-- Integer parser parses an integer and reads that integer to be used 
integer :: Parser Integer
integer =  read <$> whitespace (some dig) 

-- Symbol parser to parse symbols surrounded by whitespace
-- this parser is the base for later keywords and operators
symbol :: String -> Parser String
symbol x = whitespace $ string x

-- Parens parser to parse between parentheses "()""
parens :: Parser a -> Parser a
parens p = between (symbol "(") p (symbol ")")

-- Braces parser to parse between braces "{}"
braces :: Parser a -> Parser a
braces p = between (symbol "{") p (symbol "}")

-- QuickCheck tests
-- FP2.1
prop_letter :: Char -> Property
prop_letter x = x `elem` alphabet ==> propRunner letter [x] x ""

prop_dig :: NonNegative Int -> Bool
prop_dig (NonNegative x) = propRunner dig input output remain
        where input = show x
              output = head input
              remain = tail input

-- FP2.2
prop_between :: Char -> Char -> Char -> String -> Bool
prop_between left x right remain = propRunner p input x remain
        where p = between (char left) (char x) (char right)
              input = left:x:right:remain

prop_whitespace :: Char -> Property
prop_whitespace x = notElem x ws ==> listRun x ""
        where wsp = whitespace (char x)
              input = [' ', x]
              listRun = propRunner wsp input 

-- FP2.3
prop_sep1 :: Char -> Char -> Property
prop_sep1 x1 x2 = x1 `elem` alphabet && x2 `elem` alphabet ==> propRunner p input compare ""
        where p = sep1 (whitespace letter) (char ',')
              input = [x1,',',x2]
              compare = [x1,x2]

-- FP2.4
prop_string :: String -> Bool
prop_string s = propRunner (string s) s s ""

prop_identifier_correct :: Char -> Property
prop_identifier_correct x1 = x1 `elem` alphabet ==> propRunner identifier s s ""
        where s = [x1] ++ "aasdfghjk"

prop_identifier_incorrect :: Char -> Property
prop_identifier_incorrect x1 = x1 `notElem` alphabet && x1 `notElem` ws ==> null(runParser identifier (Stream s))
        where s = [x1] ++ "aasdfghjk"

prop_integer :: NonNegative Integer -> Bool
prop_integer (NonNegative x) = propRunner integer (show x) x ""

prop_symbol :: Char -> Property
prop_symbol x = isSymbol x  ==> propRunner (symbol [x]) ([x]++" asd") [x] "asd"

prop_parens :: Char -> Property
prop_parens x = x `notElem` ws ==> propRunner p input x ""
        where p = parens $ char x
              input = "("++[x]++")"

prop_braces :: Char -> Property
prop_braces x = x `notElem` ws ==> propRunner p input x ""
        where p = braces $ char x
              input = "{"++[x]++"}"

return []
checkBP = $quickCheckAll