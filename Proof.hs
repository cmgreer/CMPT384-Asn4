module Proof (PF (..), ProofConjecture (..), ProofResult (..),
  Rule (..), Result (..)) where

data PF = Prop Char | Neg PF | Conj PF PF |
          Disj PF PF | Imp PF PF | Equiv PF PF
          deriving Eq

data ProofConjecture = Conjecture [PF] [PF]

data ProofResult = Proof Rule Result ProofConjecture [ProofResult]

type Rule = [Char]

data Result = Proven | Refuted

-- Show a PF.

instance Show PF where
    show (Prop c) = c : []
    show (Neg p) = "~(" ++ show p ++ ")"
    show (Conj p1 p2) = "(" ++ show p1 ++ ")&(" ++ show p2 ++ ")"
    show (Disj p1 p2) = "(" ++ show p1 ++ ")|(" ++ show p2 ++ ")"
    show (Imp p1 p2) = "(" ++ show p1 ++ ")->(" ++ show p2 ++ ")"
    show (Equiv p1 p2) = "(" ++ show p1 ++ ")==(" ++ show p2 ++ ")"

-- Show a ProofConjecture.

instance Show ProofConjecture where
    show (Conjecture h g) = show h ++ " " ++ show g

-- Show a ProofResult.

instance Show ProofResult where
    show r = printPR r "" ""

printPR :: ProofResult -> [Char] -> [Char] -> [Char]

-- Rule 1a
printPR (Proof rule Proven (Conjecture h g) []) prefix indent =
    prefix ++ indent ++
    "Proved conjecture " ++ show (Conjecture h g) ++
    " by rule " ++ rule ++
    " with common proposition " ++ show (head (commonElems h g))

-- TODO: add hypothesis or goal formula being reduced
-- Rules 2a, 2b, 3a, 4b, 5b
printPR (Proof rule Proven conj [sub]) prefix indent =
    prefix ++ indent ++
    "Proved conjecture " ++ show conj ++
    " by rule " ++ rule ++
    " because" ++ (printPR sub "\n" ('\t':indent))

-- TODO: add hypothesis or goal formula being reduced
-- Rules 3b, 4a, 5a, 6a, 6b
printPR (Proof rule Proven conj [sub1, sub2]) prefix indent =
    prefix ++ indent ++
    "Proved conjecture " ++ show conj ++
    " by rule " ++ rule ++
    " because " ++
    (printPR sub1 "\n" ('\t':indent)) ++
    (printPR sub2 "\n" ('\t':indent))

-- Rule 1b
printPR (Proof "1b" Refuted conj []) prefix indent =
    prefix ++ indent ++
    "Refuted conjecture " ++ show conj ++ " by rule 1b"

-- TODO: add hypothesis or goal formula being reduced
-- Subrefutation
printPR (Proof rule Refuted conj subs) prefix indent =
    prefix ++ indent ++
    "Refuted conjecture " ++ show conj ++
    " by rule " ++ rule ++
    " because " ++ (printPR (head (findRefuted subs)) "\n" ('\t':indent))

printPR _ _ _ = "Print error: unrecognized result format"

-- Find common elements of two lists of PFs.

commonElems :: [PF] -> [PF] -> [PF]

commonElems [] _ = []
commonElems _ [] = []
commonElems (p:more) other = case elem p other of
    True -> p : (commonElems more other)
    _ -> commonElems more other

-- Find refuted ProofResults.

findRefuted :: [ProofResult] -> [ProofResult]

findRefuted [] = []

findRefuted ((Proof rule Refuted conj subs):more) =
    (Proof rule Refuted conj subs) : (findRefuted more)

findRefuted (p:more) = findRefuted more
