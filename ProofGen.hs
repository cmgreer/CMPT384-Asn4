module ProofGen (PF (..), ProofConjecture (..), ProofResult (..)) where

data PF = Prop Char | Neg PF | Conj PF PF |
          Disj PF PF | Imp PF PF | Equiv PF PF
          deriving Show

data ProofConjecture = Conjecture [PF] [PF]
                       deriving Show

--data ProofResult = proven/refuted, Rule num, Sub-proofs/refutation
data ProofResult = Proven Rule [ProofResult] |
                   Refuted Rule ProofResult
type Rule = [Char]

--Format rules: Prop's should come at start of list
wang :: ProofConjecture -> ProofResult

wang (Conjecture ((Prop p):h) g) = case rule1 (Conjecture ((Prop p):h) g) of
  (Just result) -> result
  --_

--Temp code unfinished
rule1 :: ProofConjecture -> Maybe ProofResult

rule1 (Conjecture ((Prop p):h) g) = Nothing
-- | elem (Prop p) g = 
