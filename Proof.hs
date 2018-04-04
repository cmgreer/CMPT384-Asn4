module Proof (PF (..), ProofConjecture (..), ProofResult (..),
  Rule (..), Result (..)) where

import Data.List

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
    " with common proposition " ++ show (head (commonProps h g))

-- Rules 2a, 2b, 3a, 4b, 5b
printPR (Proof rule Proven conj [sub]) prefix indent =
    prefix ++ indent ++
    "Proved conjecture " ++ show conj ++
    " with rule " ++ rule ++
    " by reducing " ++ show (head (findReduced conj sub)) ++
    " because" ++ (printPR sub "\n" ('\t':indent))

-- Rules 3b, 4a, 5a, 6a, 6b
printPR (Proof rule Proven conj [sub1, sub2]) prefix indent =
    prefix ++ indent ++
    "Proved conjecture " ++ show conj ++
    " with rule " ++ rule ++
    " by reducing " ++ show (head (findReduced conj sub1)) ++
    " because " ++
    (printPR sub1 "\n" ('\t':indent)) ++
    (printPR sub2 "\n" ('\t':indent))

-- Rule 1b
printPR (Proof "1b" Refuted conj []) prefix indent =
    prefix ++ indent ++
    "Refuted conjecture " ++ show conj ++ " by rule 1b"

-- Subrefutation
printPR (Proof rule Refuted conj subs) prefix indent =
    prefix ++ indent ++
    "Refuted conjecture " ++ show conj ++
    " with rule " ++ rule ++
    " by reducing " ++ show (head (findReduced conj (head subs))) ++
    " because " ++ (printPR (head (findRefuted subs)) "\n" ('\t':indent))

printPR _ _ _ = "Print error: unrecognized result format"

-- Find common propositions of two lists of PFs.

commonProps :: [PF] -> [PF] -> [PF]

commonProps [] _ = []
commonProps _ [] = []
commonProps ((Prop p):more) second = case elem (Prop p) second of
    True -> (Prop p) : (commonProps more second)
    _ -> commonProps more second
commonProps (p:more) second = commonProps more second

-- Find PFs that are in the first list but not the second.

onlyFirst :: [PF] -> [PF] -> [PF] -> [PF]

onlyFirst [] _ accum = accum
onlyFirst first [] accum = first
onlyFirst (p:more) second accum = case elem p second of
    True -> onlyFirst more (delete p second) accum
    _ -> onlyFirst more second (p:accum)

-- Find PF(s) that were reduced.

findReduced :: ProofConjecture -> ProofResult -> [PF]

findReduced (Conjecture h1 g1) (Proof _ _ (Conjecture h2 g2) _) = case onlyFirst h1 h2 [] of
    (p:more) -> (p:more)
    _ -> case onlyFirst g1 g2 [] of
        (p:more) -> (p:more)
        _ -> []

-- Find refuted ProofResults.

findRefuted :: [ProofResult] -> [ProofResult]

findRefuted [] = []

findRefuted ((Proof rule Refuted conj subs):more) =
    (Proof rule Refuted conj subs) : (findRefuted more)

findRefuted (p:more) = findRefuted more
