-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Jelle Nijland (s1701754)
-- Student 2: Bugra Yildiz  (s2104601)

{-
            Points  Description
   FP1.1    2 (M)   The parser can receive a “stream” (see Stream) of Chars and result in some type a.
                    This implies that a parser is of type Parser a, where a is the type of the parse result.
   FP1.2    3 (M)   The parser has an appropriate Functor instance.
   FP1.3    1 (M)   The function char :: Char -> Parser Char parses a single (given) Char.
   FP1.4    2 (S)   The function failure :: Parser a is a parser that consumes no input and fails
                    (produces no valid parsing result).
   FP1.5    2 (M)   The parser has an Applicative instance for the sequence combinator.
   FP1.6    2 (M)   The parser has an Alternative instance that tries as few alternatives as possible (i.e.,
                    until the first parser succeeds).
   Total 12
-}

{-# LANGUAGE TemplateHaskell #-}

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

-- FP1.1
-- Data constructor for parsers, contains runParser function which ingests a Stream and 
-- returns a list of tuples with the type of the parser and the remaining Stream.
data Parser r = P {
   runParser :: Stream -> [(r, Stream)] 
} 

-- FP1.2
-- Functor instance for parsers to allow (<$>) on Parsers.
instance Functor Parser where
    fmap f p = P $ \x -> [(f r,s) | (r, s) <- runParser p x]

-- FP1.3
-- Base parser for a single char, ingests a single char and 
-- returns a parser for that char
char :: Char -> Parser Char
char c = P $ \(Stream xs) -> case xs of
                                "" -> []
                                (x:xss) -> if x == c then [(c, Stream xss)] else []

-- FP1.4
-- Failure parser so no input is consumed
failure :: Parser a
failure = P $ \_ -> []

-- FP1.5
-- Applicative instance for parsers to allow (<*>) on parsers
instance Applicative Parser where
   pure x = P p
         where p y = [(x, y)]
   p1 <*> p2 = P $ \s -> [(r1 r2, s2) | (r1, s1) <- runParser p1 s
                                      , (r2, s2) <- runParser p2 s1]

-- FP1.6
-- Alternative instance for parsers to allow (<|>) on parsers
instance Alternative Parser where
  empty = failure
  p1 <|> p2 = P $ \s -> case runParser p1 s of
                        []  -> runParser p2 s
                        res -> res

-- QuickCheck tests
-- base runner for parsers in quickCheck props
propRunner :: Eq a => Parser a -> String -> a -> String -> Bool
propRunner parser input compare remain = runParser parser (Stream input) == [(compare, Stream remain)]

-- FP1.1 & FP1.3
prop_runParser :: Char -> String -> Bool
prop_runParser c s = propRunner (char c) (c:s) c s 

-- FP1.2
prop_functor :: NonNegative Int -> Bool
prop_functor (NonNegative x) = propRunner functoredparser input (digitToInt $ head input) (tail input)
   where functoredparser = digitToInt <$> parser
         parser = char $ head input
         input = show x       

-- FP1.4
prop_fail :: Char -> Char -> String -> Property
prop_fail x y zs = x /= y ==> null res
   where res = runParser p (Stream $ y:zs)
         p = char x

-- FP1.5
prop_emptyapplicative :: Char -> String -> Bool
prop_emptyapplicative x xs = propRunner (pure x) xs x xs 

prop_validapplicative :: Char -> Char -> String -> Bool
prop_validapplicative x1 x2 xs = propRunner p (x1:x2:xs) (x1,x2) xs
   where p = (,) <$> char x1 <*> char x2

prop_invalidapplicative :: Char -> Char -> String -> Property
prop_invalidapplicative x1 x2 xs = differ ==> null (runParser p (Stream xs))
   where p = (,) <$> char x1 <*> char x2
         differ = length xs < 2 || f1 /= x1 || f2 /= x2
         (f1:f2:_) = xs

-- FP1.6
prop_alternative :: Char -> Char -> String -> Property
prop_alternative x1 x2 xs = x1 /= x2 ==> propRunner p (x1:xs) x1 xs
                                      && propRunner p (x2:xs) x2 xs
                                      && propRunner prevp (x1:xs) x1 xs
   where p = char x1 <|> char x2
         prevp = char x1 <|> char x1

return []
checkPC = $quickCheckAll