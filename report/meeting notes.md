####week 0
parsing + attribute grammar
attribute grammar = general framework to compute things on trees
attribute = property of a specific node
haskell implementation exists which is efficient when sharing is involved as it avoids re-computing the same values

first: implementation
then: stage the implementation

Eric TQL: traverser + transformer: f(t:Tree)(t' (changed tree), m (monoid = result))
-> report will be ready soon

generic traverals = recursion schemes in Scala based on attribute grammar

####week 1
Haskell:
class Eq a //generic class with 1 type variable
data Foo = Foo { x :: Integer, str :: String} //a type and its valued
instance Eq Foo where ... //like a class in Java
type Name = String

####week 2
synthesized attributes: bottom up
inherited attributes: top down
what if cyclic dependencies -> availability of attributes depends on parse direction?

AG => do everything at once: parsing + compilation
Knuth => attribute grammar for 1-pass compiler
define AG on existing tree

for each production rule -> 1 syntactic function

Algebra = helper structure for fold

Mano blog post => generic fold
do type inference of simply typed lambda calculus

synthesize multiple transformations into 1?
write query languages with AG?
TQL -> low level tricks for optimization
reflexion API -> only basic mechanism for traversals and transformers

combine semantic functions which depend on each into one big transformation

pretty print?
value.parse("...").runAG(s)

algebraic dynamic approach

####week 3
Monad < Functor
Fix -> recursive data structure

List[A] = recursive data type
Functor combinator:
Fix[ListF] ~ ListF[Fix[ListF]]

we know fold on lists -> could be implemented for generic data structures -> user only needs to provide map functionality?

####week 4
AG paper: algebra defines how to combine nodes
Monoid = a specific Algebra
generalized fold -> stage it -> fuse it

staging -> program generator
