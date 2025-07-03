{-# LANGUAGE ApplicativeDo #-}

module Sexpr (
  Sexpr(Atom, List, String, Bool, Int),
  sexprP,
)where

import Control.Applicative
import Data.Int
import Data.Char
import Data.List
import Ewe

data Sexpr = Atom String | List [Sexpr] | String String | Bool Bool | Int Int64

instance Show Sexpr where
  show (Atom a) = a
  show (List l) = "(" ++ intercalate " " (map show l) ++ ")"
  show (String s) = show s
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Int n) = show n

sexprP :: Parser Char Sexpr
sexprP = listP '(' ')' <|> listP '[' ']' <|> stringP <|> intP <|> atomP <|> boolP

isAtom :: Char -> Bool
isAtom c = not (isSpace c || elem c ['(', ')', '[', ']', '"', '#'])

atomP :: Parser Char Sexpr
atomP = Atom <$> some (is isAtom)

intP :: Parser Char Sexpr
intP = (Int . read) <$> some (is isDigit)

isString :: Char -> Bool
isString c = c /= '"'

stringP :: Parser Char Sexpr
stringP = String <$> (tok '"' *> many (is isString) <* tok '"')

boolP :: Parser Char Sexpr
boolP = tok '#' *> ((tok 't' *> pure (Bool True)) <|> (tok 'f' *> pure (Bool False)))

listP :: Char -> Char -> Parser Char Sexpr
listP b e = do
  tok b
  whitespace
  elems <- many (whitespace *> sexprP)
  whitespace
  tok e
  pure (List elems)
