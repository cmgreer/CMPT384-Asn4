module ProofGen (PF (..), ProofConjecture (..), ProofResult (..),
  Rule (..), Result (..), wang) where

data PF = Prop Char | Neg PF | Conj PF PF |
          Disj PF PF | Imp PF PF | Equiv PF PF
          deriving (Show, Eq)

data ProofConjecture = Conjecture [PF] [PF]
                       deriving Show

--data ProofResult = proven/refuted, Rule num, Sub-proofs/refutation
data ProofResult = NoSub Rule Result ProofConjecture |
                   OneSub Rule Result ProofResult |
                   TwoSub Rule Result ProofResult ProofResult

type Rule = [Char]

data Result = Proven | Refuted


--Format rules: Prop's should come at start of list
wang :: ProofConjecture -> ProofResult

wang (Conjecture ((Prop p):h) g) = case rule1 (Conjecture ((Prop p):h) g) of
  (Just result) -> result
  _ -> wang ((Conjecture (h++(Prop p):[]) g))

--Temp code unfinished
rule1 :: ProofConjecture -> Maybe ProofResult

rule1 (Conjecture ((Prop p):h) g)
 | elem (Prop p) g = Just (NoSub "1a" Proven (Conjecture ((Prop p):h) g))
 | otherwise = rule1 (Conjecture h g)
