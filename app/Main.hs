module Main where

import Data.Char (ord)

type Name = String

type IsRec = Bool
recursive:: IsRec
recursive = True

nonRecursive:: IsRec
nonRecursive = False

type CoreProgram = Program Name
type Program a = [ScDefn a]

type CoreScDefn = ScDefn Name
type ScDefn a = (Name, [a], Expr a)

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


--- Printing
preludeDefs :: CoreProgram
preludeDefs = let
  s = EAp (EAp (EVar"f") (EVar "x")) (EAp (EVar "g") (EVar "x"))
  c = EAp (EVar "f") (EAp (EVar "g") (EVar "x"))
  t = EAp (EVar "f") (EAp (EVar "f") (EVar "x"))
  in
    [
      ("I", ["x"], EVar "x"),
      ("K", ["x",  "y"], EVar "x"),
      ("K1", ["x", "y"], EVar "y"),
      ("S", ["f", "g", "x"], s),
      ("compose", ["f", "g", "x"], c),
      ("twice" , ["f"], t)
    ]


-- pprExpr :: (Show a) => Expr a -> String
-- pprExpr (EVar x) = x
-- pprExpr (ENum x) = show x
-- pprExpr (EAp e1 e2) = (pprExpr e1) ++ " " ++ (pprAExpr e2)



-- mkMultiAp :: (Show a) => Int -> Expr a -> Expr a
-- mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
--   where
--     e2s = e2: e2s

-- Abstract data type for printing

data Iseq = INil
  | IStr String
  | IAppend Iseq Iseq
  | IIndent Iseq
  | INewline
  deriving (Show)

iNil :: Iseq
iNil = INil

iNewline :: Iseq
iNewline = INewline

iStr :: String -> Iseq
iStr s = IStr s

iAppend :: Iseq -> Iseq -> Iseq
iAppend seq1 seq2 = IAppend seq1 seq2

pprScDefn (name, var, expr) = pprExpr expr

pprExpr :: (Expr Name) -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (ENum n) = iStr (show n)
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) =
  iConcat [ iStr keyword, iNewline,
          iStr " ", iIndent (pprDefns defns), iNewline,
          iStr "in ", pprExpr expr]
  where
    keyword
      | isrec = "letrec"
      | otherwise = "let"

pprAExpr :: (Expr Name)  -> Iseq
pprAExpr e
 | isAtomicExpr e = pprExpr e
 | otherwise = iConcat [iStr "(", pprExpr e, iStr ")"]

pprDefns :: [(Name, Expr Name)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, Expr Name) -> Iseq
pprDefn (name, expr)
  = iConcat [iStr name, iStr "=", iIndent (pprExpr expr)]

iConcat :: [Iseq] -> Iseq
iConcat (iseq:iseqs) = foldl iAppend iseq iseqs
iConcat [] = INil

iInterleave sep iseqs = foldr f INil iseqs
  where
    f x y = iAppend (iAppend x sep) y

--- Parser

type Token = (Int, String)

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` (" \t"::String)

isNewline :: Char -> Bool
isNewline c = c == '\n'

iIndent e = IIndent e

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
  | isNewline c = clex (lineno + 1) cs
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

pLit1 :: String -> Parser String
pLit1 s (tok:toks)
  | s == tok = [(s, toks)]
  | otherwise = []
pLit1 _ [] = []

--pVar :: Parser String
--pVar (tok:toks) = [(tok, toks)]
--pVar [] = []

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

--pProgram :: Parser a -> Parser b -> Parser a
pProgram :: Parser a -> Parser sep -> Parser a
pProgram = pThen keep_first
  where
    keep_first a _ = a

pSep :: Parser String
pSep (c:toks)
  | c == ";" = [(c, toks)]
  | otherwise = []
pSep [] = []

--pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
--pOneOrMoreWithSep p1 p2 tok = pOneOrMore

pZeroOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pZeroOrMoreWithSep p1 p2 = pOneOrMoreWithSep p1 p2  `pAlt` pEmpty []

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen combine (pProgram p1 p2) (pZeroOrMoreWithSep p1 p2)
  where
    combine a b = a:b

pSat :: (String -> Bool) -> Parser String
pSat f (tok:toks)
  | f tok == True = [(tok, toks)]
  | otherwise   = []
pSat _ [] = []

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar = pSat (not . (flip elem $ keywords))

keywords :: [String]
keywords = ["let", "letrec", " ase", "in", "of", "Pack"]




main :: IO ()
main = putStrLn "main"
