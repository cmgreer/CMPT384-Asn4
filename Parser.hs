-- TODO: implement parseExpr

module Parser where

import Data.Char
import ProofGen (PF (..))

data Token = PropToken Char |
             Tilde |
             Ampersand |
             Bar |
             Arrow |
             Equal |
             LParen |
             RParen
             deriving Show

-- Convert a string into a list of tokens.
-- Invalid characters and whitespace will be ignored.

tokenize :: [Char] -> [Token]

tokenize [] = []

tokenize ('-':'>':more) = Arrow : (tokenize more)

tokenize (c:more)
    | isAlpha c    = (PropToken c) : (tokenize more)
    | c == '~'     = Tilde : (tokenize more)
    | c == '&'     = Ampersand : (tokenize more)
    | c == '|'     = Bar : (tokenize more)
    | c == '='     = Equal : (tokenize more)
    | c == '('     = LParen : (tokenize more)
    | c == ')'     = RParen : (tokenize more)
    | otherwise    = tokenize more

-- Transform a list of tokens into a preferred form for the parser.
-- The resulting list of tokens will be equivalent.

transform :: [Token] -> [Token]

transform [] = []

transform (LParen:(PropToken c):RParen:more) = LParen : (PropToken c) : RParen : (transform more)

transform ((PropToken c):more) = LParen : (PropToken c) : RParen : (transform more)

transform (t:more) = t : (transform more)

-- Parse an expression from a list of tokens.

parseExpr :: [Token] -> Maybe (PF, [Token])

parseExpr _ = Nothing

-- Parse a PF from a string.

parse :: [Char] -> Maybe PF

parse cs = case parseExpr (transform (tokenize cs)) of
    Just (p, []) -> Just p
    _ -> Nothing
