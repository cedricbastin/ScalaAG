Attributed parser combinators
extending PC TO ATTRIBUTE gra

###Abstract
Parser are a useful tool which, with the rise of non structured data are still use
However constructing parse tree can be expensive and even us
In most cases the parse tree will be not be used by itself but rather methods are applied to extract content or transform the nodes.
We will present a generic parsing framework which can be used to define parser which can be composed with different semantic functions which will be applied during the parsing.
The AGParsers, where "AG" , from our library  we wanted to create a generic parsing framework which allows manipulation
We use ideas from Attribute grammars as well as the environment monad to 

traversals, transformers and collectors

###Introduction
Parsing is still an interesting topic as programming languages evolve and some compiler developers might still be thinking about a **once pass compiler** or at least apply several parsing steps at the same time. Those step require knowledge about the their parents or even some information calculated over the whole tree
Parser combinators have shown that it is possible to have an compostable and easy to use framework which allows to quickly create parser for a input of you choice. The implementation in Scala is very easy to use and allows the creation of very specific parsers
We wanted 
motivation => problem (efficiently)

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
Attribute grammars were introduced by Donald Knuth in 1967. They formalize the a set of attributes over a formal grammar such that each production rule can be evaluated to such attributes. Thus an attribute grammar is defined over the nodes of an abstract syntax tree such that the corresponding nodes can be transformed into a corresponding value. So me of those rules are not simply constructors but need extended knowledge of a larger subset of the parse tree.

Such attributes could be defined as follows:
```
Expr1 → Expr2 + Term    [ Expr1.value = Expr2.value + Term.value ]
Expr → Term             [ Expr.value = Term.value ]
Term1 → Term2 * Factor  [ Term1.value = Term2.value * Factor.value ]
Term → Factor           [ Term.value = Factor.value ]
Factor → "(" Expr ")"   [ Factor.value =  Expr.value ]
Factor → integer        [ Factor.value = strToInt(integer.str) ]
```
There are 2 different kinds of attributed, the synthesized and inherited attributed. As the name suggest the inherited attributes are used to pass semantic information to the child branches of a node and synthesized attributes are used to pass semantic information up in the parse tree. As parsers are generally construction a tree structure from a linear onput they either have to work left-to-right or right-to-left and thus some of the needed information might potentially be unavailable at the time fo parsing. Lazy evaluation in functional programming can be used to overcome these kind of restrictions as long as no cyclic dependencies between then attribute rules occur.

Furthermore as the syntactic definition of a formal grammar might be more permissive than actual language is related to, an attribute grammar could be used to validate the parsed content.

As the attributes are an abstract way of decorating the abstract syntax tree one can easily decouple the attribute rules from the grammar such that different attribute grammar can be used over the same formal grammar providing different computations over a parse tree without the need to rewrite the grammar.

###Monad (state and reader)

###Monad transformer?
"It would be ideal if we could somehow take the standard State monad and add failure handling to it, without resorting to the wholesale construction of custom monads by hand. "
"A monad transformer is similar to a regular monad, but it’s not a standalone entity: instead, it modifies the behaviour of an underlying monad."
"Monad transformers can be used to compose features encapsulated by monads "


###Monadic Parsing?
"scala implementation "almost monadic""
"In practice, however, using seq leads to parsers with nested
tuples as results, which are messy to manipulate.
The problem of nested tuples can be avoided by adopting a monadic sequencing
combinator (commonly known as bind) which integrates the sequencing of parsers
with the processing of their result values:"

"internal DSLs are limited by the syntax of the host language (constraint by features of programming language)"
"external DSLs are only limited by the parser"
monad transformers allow to stack monads

###CFG vs CSG


###Parser combinators
Compared to the clumsy parser generator tools which generate parser from a context free grammar, Scala took inspiration from Haskell and its parser combinators. Parser generators a re a domain specific language 
The Scala Parser combinators is a library for compositional parsing
Since Scala 2.11 they have been factored out of the scala langauge into a separate library which can be used in your projects by including them with sbt:
`libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"`
There are a set of combinator which can be used to create compoed parser such as the sequencial composition combinator `parserA ~ parserB` which will return a ParseResult of both `parserA` and `parserB` concatenated (in a ~(a:A, b:B) object).


####flatMap and context sensitive parsing
One importatn parser combinator is the `flatMap` combinator, also called `into` or `>>`:
`def >>[U](fq: T => Parser[U]) = into(fq) = flatMap(fq)`
This combinator helps to overcome some limitations of traditional parsers described earlier as it allows local context sensitivity during parsing. For instance does this method allow to extract the `message length` information form a message header such that the body parser knows when to stop reading input. Another example would be a message dispatcher which would switch to use a different body parser implementation depending on the message type described in the header.

###Implementation
augment parser combinators
have an environment / accumulator available if it is needed 

####AGSig
####AGAlgebra
####AGGrammar
call to syntactic functions instead of building a parse tree

####issues
validation
where to push the environment

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
(Jonas: object algebra to attribute grammar)
####Kiama
*Kiama is a Scala library for language processing which allows analysis and transformation on structured data using formal languages processing paradigms such as attribute grammars and tree rewriting.*
Through the use of Scala macros, Kiama augments existing tree structures with named or unnamed attribute function which can then be used to evaluate local or global properties.
`def attr[T,U] (f : T => U) : CachedAttribute[T,U] = macro AttributionCoreMacros.attrMacro[T,U,CachedAttribute[T,U]]`
This makes the relation between the parents and child nodes in a tree explicit and thus allows the different attribute method to access each other. We wanted to avoid constructing an additional structure on top of the tree structure and actually completely dismiss the original parse tree if it is not needed for afterwards. With the AGParser it is possible to create parent-child and child-parents calls however do they need to be explicit.
Kiama is also able to treat more general graphs and 

####TQL
haskell paper
Monad paper (Reader, Env)

###Staging with LMS??

####CharParsers example $a^nb^nc^n$

###references:
attribute grammars: 1967 Semantics of context-free languages, by Don Knuth, 
combinator parsing (Wadler, 1985; Hutton, 1992; Fokker, 1995),
Monadic Parser Combinator, Hutton+Meijer 1996, https://www.cs.nott.ac.uk/~gmh/monparsing.pdf 
scala parser combinators: https://github.com/scala/scala-parser-combinators
Kiama: https://code.google.com/p/kiama/

