Attributed parser combinators
extending PC TO ATTRIBUTE gra

###Abstract
Parser are a useful tool which, with the rise of 
today parsers are used to create AST 
with AGParsers we wanted to create a generic parsing framework which allows manipulation
We use ideas from Attribute grammars and TQL

traversals, transformers and collectors

###Introduction
motivation => problem (effeciently)

1 pass compiler?
dynamic programming

problem: 
transformers, traversers
context sensitive parsing
avoid multiple passes

why are attribute grammars good

how to implement

quickly related work (litt review)


###Attribute Grammars
```
Expr1 → Expr2 + Term    [ Expr1.value = Expr2.value + Term.value ]
Expr → Term             [ Expr.value = Term.value ]
Term1 → Term2 * Factor  [ Term1.value = Term2.value * Factor.value ]
Term → Factor           [ Term.value = Factor.value ]
Factor → "(" Expr ")"   [ Factor.value =  Expr.value ]
Factor → integer        [ Factor.value = strToInt(integer.str) ]
```
synthesized and inherited attributes

###Parser combinators
general introduction

####flatMap and context sensitive parsing


###Implementation
augment parser combinators
have an environment / accumulator available if it is needed 

####AGSig
####AGAlgebra
####AGGrammar
call to syntactic functions instead of building a parse tree

####new operators
^^

\>>^^

^^>>

\>>^^>>

~

\>>~

~>>

\>>~>>

###Examples
RepMin
Typing (Tranformer)
Html (Collector)


###related work
Kiama (AG) (Jonas: object algebra to attribute grammar)
TQL
haskell paper
Monad paper (Reader, Env)

###Staging with LMS??

####CharParsers example $a^nb^nc^n$
