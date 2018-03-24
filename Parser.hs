-- BNF grammar:
--
-- <formula> ::= "(" <element> ")"
-- <element> ::= <prop> | "~" <formula> |
--               <formula> "&" <formula> | <formula> "|" <formula> |
--               <formula> "->" <formula> | <formula> "==" <formula>
-- <prop> ::= [a-zA-Z]

module Parser (parse) where

import Data.Char
import ProofGen (PF (..))

data Token = PropToken Char |
             Tilde |
             Ampersand |
             Bar |
             Arrow |
             DoubleEqual |
             LParen |
             RParen
             deriving Show

-- Convert a string into a list of tokens.
-- Invalid characters and whitespace will be ignored.

tokenize :: [Char] -> [Token]

tokenize [] = []

tokenize ('-':'>':more) = Arrow : (tokenize more)

tokenize ('=':'=':more) = DoubleEqual : (tokenize more)

tokenize (c:more)
    | isAlpha c    = (PropToken c) : (tokenize more)
    | c == '~'     = Tilde : (tokenize more)
    | c == '&'     = Ampersand : (tokenize more)
    | c == '|'     = Bar : (tokenize more)
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

-- Parse a formula from a list of tokens.

parseFormula :: [Token] -> Maybe (PF, [Token])

parseFormula (LParen:more) = case parseElem more of
    Just (e, RParen:more) -> Just (e, more)
    _ -> Nothing

parseFormula _ = Nothing

-- Parse an element from a list of tokens.

parseElem :: [Token] -> Maybe(PF, [Token])

parseElem ((PropToken c):more) = Just (Prop c, more)

parseElem (Tilde:more) = case parseFormula more of
    Just (f, more) -> Just (Neg f, more)
    _ -> Nothing

parseElem ts = case parseFormula ts of
    Just (f, Ampersand:more) -> case parseFormula more of
        Just (g, yetmore) -> Just (Conj f g, yetmore)
        _ -> Nothing
    Just (f, Bar:more) -> case parseFormula more of
        Just (g, yetmore) -> Just (Disj f g, yetmore)
        _ -> Nothing
    Just (f, Arrow:more) -> case parseFormula more of
        Just (g, yetmore) -> Just (Imp f g, yetmore)
        _ -> Nothing
    Just (f, DoubleEqual:more) -> case parseFormula more of
        Just (g, yetmore) -> Just (Equiv f g, yetmore)
        _ -> Nothing
    _ -> Nothing

-- Parse a PF from a string.

parse :: [Char] -> Maybe PF

parse cs = case parseFormula (transform (tokenize cs)) of
    Just (p, []) -> Just p
    _ -> Nothing
