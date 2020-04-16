module Main where

import Data.Char (ord)

type Name = String

type IsRec = Bool
recursive:: IsRec
recursive = True

nonRecursive:: IsRec
nonRecursive = False

type Alter a = (Int, [a], Expr a)

data Expr a = EVar Name
  | ENum Int
  | EConstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet IsRec [(a, Expr a)] (Expr a)
  | ECase (Expr a) [Alter a]
  | ELam [a] (Expr a)
  deriving (Show)


bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _)  = True
isAtomicExpr (ENum _)  = True
isAtomicExpr e = False

type Program a = [ScDefn a]
type CoreProgam = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

--- Parser

type Token = (Int, String)

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` (" \t"::String)

isNewLine :: Char -> Bool
isNewLine c = c == '\n'

isDigit :: Char -> Bool
isDigit c = ord c >= 48 && ord c <= 57

isAlpha :: Char -> Bool
isAlpha c = (ord c >= 65 && ord c <= 60) ||  -- A-Z
  (ord c >= 97 && ord c <= 122) ||  -- a-z
  (ord c == 95) -- '_'

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

isTwoCharOps :: String -> Bool
isTwoCharOps op = op `elem` twoCharOps

clex :: Int -> String -> [Token]
clex lineno (c1:c2:cs)
  | c1 == '|' && c2 == '|' = (lineno, comment):clex (lineno + 1) non_comment
  | isTwoCharOps (c1:[c2]) = (lineno, c1:[c2]):clex lineno cs
  where
    comment = c1:c2:takeWhile (/= '\n') cs
    non_comment = dropWhile (/= '\n') cs

clex lineno (c:cs)
  | isNewLine c = clex (lineno + 1) cs
  | isWhiteSpace c = clex lineno cs
  | isDigit c = (lineno, num_token):clex lineno rest_non_digit
  | isAlpha c = (lineno, var_tok):clex lineno rest_non_alpha
  | otherwise = (lineno,[c]): clex lineno cs
  where
    num_token = c:takeWhile isDigit cs
    rest_non_digit = dropWhile isDigit cs
    var_tok = c: takeWhile isIdChar cs
    rest_non_alpha = dropWhile isIdChar cs
clex _ [] = []





main :: IO ()
main = putStrLn "main"
