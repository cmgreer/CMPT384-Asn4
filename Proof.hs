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

-- Show the steps of a ProofResult.

instance Show ProofResult where
    show r = resultHelper r ("", "")

resultHelper :: ProofResult -> ([Char], [Char]) -> [Char]

resultHelper (Proof rule res (Conjecture h g) []) (prefix, indent) =
    prefix ++ indent ++
    show res ++ " conjecture with hypothesis list " ++ show h ++ " and goal list " ++ show g ++
    " by rule " ++ rule

resultHelper (Proof rule res (Conjecture h g) [sub]) (prefix, indent) =
    prefix ++ indent ++
    show res ++ " conjecture with hypothesis list " ++ show h ++ " and goal list " ++ show g ++
    " by rule " ++ rule ++
    " because " ++ (resultHelper sub ("\n", ('\t':indent)))

resultHelper _ _ = "unknown"

-- Show a Result.

instance Show Result where
    show Proven = "Proved"
    show Refuted = "Refuted"
