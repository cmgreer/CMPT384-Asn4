data PF = Prop Char | Neg PF | Conj PF PF |
               Disj PF PF | Imp PF PF | Equiv PF PF deriving Show

data ProofConjecture = Conjecture [PF] [PF]
