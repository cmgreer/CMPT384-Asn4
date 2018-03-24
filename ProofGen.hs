module ProofGen (PF (..), ProofConjecture (..), ProofResult (..),
  Rule (..), Result (..), wang) where

data PF = Prop Char | Neg PF | Conj PF PF |
          Disj PF PF | Imp PF PF | Equiv PF PF
          deriving (Show, Eq)

data ProofConjecture = Conjecture [PF] [PF]
                       deriving Show

data ProofResult = NoSub Rule Result ProofConjecture |
                   OneSub Rule Result ProofResult |
                   TwoSub Rule Result ProofResult ProofResult

type Rule = [Char]

data Result = Proven | Refuted

wang :: ProofConjecture -> ProofResult

wang (Conjecture ((Prop p):h) g) = case rule1 (Conjecture ((Prop p):h) g) of
  (Just result) -> result
  _ -> wang ((Conjecture (h++(Prop p):[]) g))

rule1 :: ProofConjecture -> Maybe ProofResult
rule1 (Conjecture [] []) = Just (NoSub "1b" Refuted (Conjecture [] []))
rule1 (Conjecture h []) = Nothing
rule1 (Conjecture [] g) = Nothing
rule1 (Conjecture ((Prop p):h) g)
 | elem (Prop p) g = Just (NoSub "1a" Proven (Conjecture ((Prop p):h) g))
 | otherwise = case rule1 (Conjecture h g) of
   Just (NoSub rule result (Conjecture hs gs)) -> Just (NoSub rule result (Conjecture (hs++(Prop p):[]) gs))
   result -> result
rule1 (Conjecture (p:h) g) = case rule1 (Conjecture h g) of
    Just (NoSub rule result (Conjecture hs gs)) -> Just (NoSub rule result (Conjecture (hs++p:[]) gs))
    result -> result
