module Proof (PF (..), ProofConjecture (..), ProofResult (..),
  Rule (..), Result (..)) where

data PF = Prop Char | Neg PF | Conj PF PF |
          Disj PF PF | Imp PF PF | Equiv PF PF
          deriving (Show, Eq)

data ProofConjecture = Conjecture [PF] [PF]
                       deriving Show

data ProofResult = Proof Rule Result ProofConjecture [ProofResult] deriving Show

type Rule = [Char]

data Result = Proven | Refuted deriving Show
