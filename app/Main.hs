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


-- Parser
type Token1 = String
type Parser a = [Token1] -> [(a, [Token1])]


pLit :: String -> Parser String
pLit s (tok:toks)
  | s == tok = [(s, toks)]
  | otherwise = []
pLit _ [] = []

pVar :: Parser String
pVar (tok:toks) = [(tok, toks)]
pVar [] = []


pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pHelloOrGoodBye :: Parser String
pHelloOrGoodBye = pLit "hello" `pAlt` pLit "goodbye"

pThen1 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen1 f p1 p2 toks = do
  (a, toks1) <- p1 toks
  (b, toks2) <- p2 toks1
  return (f a b, toks2)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen f p1 p2 toks =
  [(f a b, tok2) | (a, tok1) <-p1 toks, (b, tok2) <- p2 tok1]

pThen2 ::  (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen2 f p1 p2 p3 toks = do
  (a, toks1) <- p1 toks
  (b, toks2) <- p2 toks1
  (c, toks3) <- p3 toks2
  return (f a b c, toks3)

pThen3 ::  (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen3 f p1 p2 p3 p4 toks = do
  (a, toks1) <- p1 toks
  (b, toks2) <- p2 toks1
  (c, toks3) <- p3 toks2
  (d, toks4) <- p4 toks3
  return (f a b c d, toks4)

pGreeting :: Parser (String, String)
pGreeting = pThen keep_first (pThen mk_pair pHelloOrGoodBye pVar) (pLit "!")
  where
    mk_pair hg name = (hg, name)
    keep_first hg_name _ = hg_name

pEmpty :: a -> Parser a
pEmpty x toks = [(x, ["..."])]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen combine p (pZeroOrMore p)
  where
    combine a b = a:b

pGreeting2 :: Parser [(String, String)]
pGreeting2 = pZeroOrMore pGreeting

pGreetingsN :: Parser Int
pGreetingsN = pGreeting2 `pApply` length

pApply :: Parser a -> (a->b) -> Parser b
pApply p f tok =  [(f a, toks) | (a, toks) <- p tok]

main :: IO ()
main = putStrLn "main"
