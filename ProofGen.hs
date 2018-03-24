module ProofGen (PF (..), ProofConjecture (..), ProofResult (..),
  Rule (..), Result (..), wang) where

data PF = Prop Char | Neg PF | Conj PF PF |
          Disj PF PF | Imp PF PF | Equiv PF PF
          deriving (Show, Eq)

data ProofConjecture = Conjecture [PF] [PF]
                       deriving Show

data ProofResult = Proof Rule Result ProofConjecture [ProofResult]

type Rule = [Char]

data Result = Proven | Refuted

wang :: ProofConjecture -> ProofResult

wang (Conjecture ((Prop ph):h) ((Prop pg):g)) = case rule1 (Conjecture ((Prop ph):h) ((Prop pg):g)) of
  (Just result) -> result
  _ -> wang (Conjecture (h++(Prop ph):[]) (g++(Prop pg):[]))

--oneSubRules
wang (Conjecture ((Neg p):h) g) = case rule1 (Conjecture ((Neg p):h) g) of
  (Just result) -> result
  _ -> case wang (Conjecture h (p:g)) of
    (Proof rule result conjecture subproofs) ->
      (Proof "2a" result (Conjecture ((Neg p):h) g) [(Proof rule result conjecture subproofs)])

wang (Conjecture ((Prop p):h) g) = case rule1 (Conjecture ((Prop p):h) g) of
  (Just result) -> result
  _ -> wang (Conjecture (h++(Prop p):[]) g)
wang (Conjecture h ((Prop p):g)) = case rule1 (Conjecture h ((Prop p):g)) of
  (Just result) -> result
  _ -> wang (Conjecture h (g++(Prop p):[]))

--TwoSubRules

rule1 :: ProofConjecture -> Maybe ProofResult
rule1 (Conjecture [] []) = Just (Proof "1b" Refuted (Conjecture [] []) [])
rule1 (Conjecture h []) = Just (Proof "1b" Refuted (Conjecture [] []) [])
rule1 (Conjecture [] g) = Just (Proof "1b" Refuted (Conjecture [] []) [])
rule1 (Conjecture ((Prop p):h) g)
 | elem (Prop p) g = Just (Proof "1a" Proven (Conjecture ((Prop p):h) g) [])
 | otherwise = case rule1 (Conjecture h g) of
   Just (Proof rule result (Conjecture hs gs) prs) -> Just (Proof rule result (Conjecture (hs++(Prop p):[]) gs) prs)
   result -> result
rule1 (Conjecture (p:h) g) = case rule1 (Conjecture h g) of
    Just (Proof rule result (Conjecture hs gs) prs) -> Just (Proof rule result (Conjecture (hs++p:[]) gs) prs)
    result -> result
