module Wang (wang) where

import Proof (PF (..), ProofConjecture (..), ProofResult (..),
  Rule (..), Result (..))

wang :: ProofConjecture -> ProofResult

wang (Conjecture ((Prop ph):h) ((Prop pg):g)) = case rule1 (Conjecture ((Prop ph):h) ((Prop pg):g)) of
  (Just result) -> result
  _ -> wang (Conjecture (h++(Prop ph):[]) (g++(Prop pg):[]))

--oneSubProof
wang (Conjecture ((Neg p):h) g) = case rule1 (Conjecture ((Neg p):h) g) of
  (Just result) -> result
  _ -> case wang (Conjecture h (p:g)) of
    (Proof rule result conjecture subproofs) ->
      (Proof "2a" result (Conjecture ((Neg p):h) g) [(Proof rule result conjecture subproofs)])
wang (Conjecture h ((Neg p):g)) = case rule1 (Conjecture h ((Neg p):g)) of
  (Just result) -> result
  _ -> case wang (Conjecture (p:h) g) of
    (Proof rule result conjecture subproofs) ->
      (Proof "2b" result (Conjecture ((Neg p):h) g) [(Proof rule result conjecture subproofs)])
wang (Conjecture ((Conj p q):h) g) = case rule1 (Conjecture ((Conj p q):h) g) of
  (Just result) -> result
  _ -> case wang (Conjecture h (p:q:g)) of
    (Proof rule result conjecture subproofs) ->
      (Proof "3a" result (Conjecture ((Conj p q):h) g) [(Proof rule result conjecture subproofs)])
wang (Conjecture h ((Disj p q):g)) = case rule1 (Conjecture h ((Disj p q):g)) of
  (Just result) -> result
  _ -> case wang (Conjecture h (p:q:g)) of
    (Proof rule result conjecture subproofs) ->
      (Proof "4b" result (Conjecture h ((Disj p q):g)) [(Proof rule result conjecture subproofs)])
wang (Conjecture h ((Imp p q):g)) = case rule1 (Conjecture h ((Imp p q):g)) of
  (Just result) -> result
  _ -> case wang (Conjecture (p:h) (q:g)) of
    (Proof rule result conjecture subproofs) ->
      (Proof "5b" result (Conjecture h ((Imp p q):g)) [(Proof rule result conjecture subproofs)])

--TwoSubProofs
wang (Conjecture h ((Conj p q):g)) = case rule1 (Conjecture h ((Conj p q):g)) of
  (Just result) -> result
  _ -> case wang (Conjecture h (p:g)) of
    (Proof rule Refuted conjecture subproofs) ->
      (Proof "3b" Refuted (Conjecture h ((Conj p q):g))
      [(Proof rule Refuted conjecture subproofs)])
    (Proof rule1 Proven conjecture1 subproofs1) -> case wang (Conjecture h (q:g)) of
      (Proof rule2 Refuted conjecture2 subproofs2) ->
        (Proof "3b" Refuted (Conjecture h ((Conj p q):g))
        [(Proof rule2 Refuted conjecture2 subproofs2)])
      (Proof rule2 Proven conjecture2 subproofs2) ->
        (Proof "3b" Proven (Conjecture h ((Conj p q):g))
        [(Proof rule1 Proven conjecture1 subproofs1),
        (Proof rule2 Proven conjecture2 subproofs2)])
wang (Conjecture ((Disj p q):h) g) = case rule1 (Conjecture ((Disj p q):h) g) of
  (Just result) -> result
  _ -> case wang (Conjecture (p:h) g) of
    (Proof rule Refuted conjecture subproofs) ->
      (Proof "4a" Refuted (Conjecture ((Disj p q):h) g)
      [(Proof rule Refuted conjecture subproofs)])
    (Proof rule1 Proven conjecture1 subproofs1) -> case wang (Conjecture (q:h) g) of
      (Proof rule2 Refuted conjecture2 subproofs2) ->
        (Proof "4a" Refuted (Conjecture ((Disj p q):h) g)
        [(Proof rule2 Refuted conjecture2 subproofs2)])
      (Proof rule2 Proven conjecture2 subproofs2) ->
        (Proof "4a" Proven (Conjecture ((Disj p q):h) g)
        [(Proof rule1 Proven conjecture1 subproofs1),
        (Proof rule2 Proven conjecture2 subproofs2)])
wang (Conjecture ((Imp p q):h) g) = case rule1 (Conjecture ((Imp p q):h) g) of
  (Just result) -> result
  _ -> case wang (Conjecture (q:h) g) of
    (Proof rule Refuted conjecture subproofs) ->
      (Proof "5a" Refuted (Conjecture ((Imp p q):h) g)
      [(Proof rule Refuted conjecture subproofs)])
    (Proof rule1 Proven conjecture1 subproofs1) -> case wang (Conjecture h (p:g)) of
      (Proof rule2 Refuted conjecture2 subproofs2) ->
        (Proof "5a" Refuted (Conjecture ((Imp p q):h) g)
        [(Proof rule2 Refuted conjecture2 subproofs2)])
      (Proof rule2 Proven conjecture2 subproofs2) ->
        (Proof "5a" Proven (Conjecture ((Imp p q):h) g)
        [(Proof rule1 Proven conjecture1 subproofs1),
        (Proof rule2 Proven conjecture2 subproofs2)])
wang (Conjecture ((Equiv p q):h) g) = case rule1 (Conjecture ((Equiv p q):h) g) of
  (Just result) -> result
  _ -> case wang (Conjecture (p:q:h) g) of
    (Proof rule Refuted conjecture subproofs) ->
      (Proof "6a" Refuted (Conjecture ((Equiv p q):h) g)
      [(Proof rule Refuted conjecture subproofs)])
    (Proof rule1 Proven conjecture1 subproofs1) -> case wang (Conjecture h (p:q:g)) of
      (Proof rule2 Refuted conjecture2 subproofs2) ->
        (Proof "6a" Refuted (Conjecture ((Equiv p q):h) g)
        [(Proof rule2 Refuted conjecture2 subproofs2)])
      (Proof rule2 Proven conjecture2 subproofs2) ->
        (Proof "6a" Proven (Conjecture ((Equiv p q):h) g)
        [(Proof rule1 Proven conjecture1 subproofs1),
        (Proof rule2 Proven conjecture2 subproofs2)])
wang (Conjecture h ((Equiv p q):g)) = case rule1 (Conjecture h ((Equiv p q):g)) of
  (Just result) -> result
  _ -> case wang (Conjecture (p:h) (q:g)) of
    (Proof rule Refuted conjecture subproofs) ->
      (Proof "6b" Refuted (Conjecture h ((Equiv p q):g))
      [(Proof rule Refuted conjecture subproofs)])
    (Proof rule1 Proven conjecture1 subproofs1) -> case wang (Conjecture (q:h) (p:g)) of
      (Proof rule2 Refuted conjecture2 subproofs2) ->
        (Proof "6b" Refuted (Conjecture h ((Equiv p q):g))
        [(Proof rule2 Refuted conjecture2 subproofs2)])
      (Proof rule2 Proven conjecture2 subproofs2) ->
        (Proof "6b" Proven (Conjecture h ((Equiv p q):g))
        [(Proof rule1 Proven conjecture1 subproofs1),
        (Proof rule2 Proven conjecture2 subproofs2)])

--Cycle lists
wang (Conjecture ((Prop p):h) g) = case rule1 (Conjecture ((Prop p):h) g) of
  (Just result) -> result
  _ -> wang (Conjecture (h++(Prop p):[]) g)
wang (Conjecture h ((Prop p):g)) = case rule1 (Conjecture h ((Prop p):g)) of
  (Just result) -> result
  _ -> wang (Conjecture h (g++(Prop p):[]))


rule1 :: ProofConjecture -> Maybe ProofResult
rule1 (Conjecture [] []) = Just (Proof "1b" Refuted (Conjecture [] []) [])
rule1 (Conjecture h [])
  | onlyProps h = Just (Proof "1b" Refuted (Conjecture h []) [])
  | otherwise = Nothing
rule1 (Conjecture [] g)
  | onlyProps g = Just (Proof "1b" Refuted (Conjecture [] g) [])
  | otherwise = Nothing
rule1 (Conjecture ((Prop p):h) g)
 | elem (Prop p) g = Just (Proof "1a" Proven (Conjecture ((Prop p):h) g) [])
 | otherwise = case rule1 (Conjecture h g) of
   Just (Proof rule result (Conjecture hs gs) prs) -> Just (Proof rule result (Conjecture (hs++(Prop p):[]) gs) prs)
   result -> result
rule1 (Conjecture (p:h) g) = case rule1 (Conjecture h g) of
    Just (Proof rule Proven (Conjecture hs gs) prs) -> Just (Proof rule Proven (Conjecture (hs++p:[]) gs) prs)
    --p is not a Prop therefore rule 1b does not apply.
    Just (Proof "1b" Refuted (Conjecture hs gs) prs) -> Nothing
    result -> result

onlyProps :: [PF] -> Bool
onlyProps [] = True;
onlyProps ((Prop p):more) = onlyProps more
onlyProps _ = False
