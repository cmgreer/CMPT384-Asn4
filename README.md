# Propositional Formula Syntax

For a full BNF grammar, please see Parser.hs

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119.

## Propositions

A proposition MUST match the regular expression [a-zA-Z]

Good : a

Good : B

Bad : foo

Bad : 3

## Operations

~A means "not A"

A&B means "conjunction of A and B"

A|B means "disjunction of A and B"

A->B means "A implies B"

A==B means "A is equivalent to B"

## Rules

Every propositional formula (except invididual propositions) MUST be enclosed in parentheses.

Invididual propositions MAY be enclosed in parentheses.

Extraneous parentheses MUST NOT be included.

Good : A

Good : (A)

Good : (A&B)

Good : ((A&B)->(A|B))

Good : ((A)&(B))

Bad : A&B

Bad : ((A&B))

Whitespace will be ignored.

Good : (A & B)

Good : ( A == B )

## Examples

Classic tautology : (P | (~P))

Modus ponens : (((P -> Q) & P) -> Q)

Modus tollens: (((P -> Q) & (~Q)) -> (~P))
