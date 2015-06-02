<!-- extending parser combinators to attribute grammar-->
#Attributed parser combinators

###Abstract
Through the rise of so called "Big Data" as well as the corresponding analytics, requirements for fast and efficient data retrieval and manipulation are suggesting new advancement with parsers. Those conditions make parsing techniques regain importance in order to allow faster data access and treatment.
However constructing parse trees can be expensive as not all of the parsed content might be of interest or if the data in its basic form is not useful and needs to be transformed any way.
We will present a generic parsing framework in Scala which can be used to define a parsing grammar which can be composed with different semantic functions which will be applied during the parsing. The AGParsers (attribute grammar parsers) use a polyvalent structure to allow to user to easily extend it and adapt it to it's own use-cases.
We use ideas from attribute grammars to combine syntactic and dynamic functionality during parsing thus creating a reader framework which can carry partial result information through the parse tree which can be used directly during parsing. Thus the user can define exactly which structure he would like to be created during parsing without the need of several manipulating passes over the parsed tree structure.
The described functionality is achieved by augmenting the existing scala parser combinator to be able to carry additional information during parsing which can be used and augmented depending on the given parse rule and the related semantic function.

**Keywords**
attribute grammar, parser combinators, scala

**Note**
environment, answer and attributes are used synonumously 

###Introduction (& motivation)
Parsing is still an interesting topic as programming languages evolve and some compiler developers might still be thinking about a **one-pass compiler** or at least apply several manipulating steps at the same time. Those steps require more knowledge about the tree structure in general which can be acquired by defining several manipulation recursive functions over the parse tree which pass around the additional information needed. Another method to achieve this is to use attribute grammar to augment the existing nodes in the tree with the needed information which can then be accessed several times.
Parser combinators have shown that it is possible to have an compostable and easy to use parsing framework which allows to quickly create parser for an input of you choice. without the need to write complicated parser by hand or even use parser generating tools. We wanted to achieve the same ease of use by providing a parsing framework with more functionality

On the other hand nowadays parsing is used the most frequently in combination with information coming from the web such as html where only part of the information is potentially useful which can only be extracted 
motivation => problem (efficiently)

<!--dynamic programming-->

traversals, transformers and collectors

problem: 
transformers, traversers
context sensitive parsing
avoid multiple passes

why are attribute grammars good
Even though attributes grammar have been around for almost as long as functional programming 
The *credit-card transformation* helped to avoid multiple passes over a tree by tupling results and making then evaluated lazily.

Scala Combinator have been discovered and used in a variety of programming languages since 

During the development of the project we considered various implementation which 
We will also cover related work where some research has been done 


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

###Monads
<!--general-->
A monad in functional programming represents an encapsulated computation and its result such that it can be composed or pipelined with other monads. Monads are used in functional programming to abstract over the side effects of a computation and avoid mutation. A Monad only consists if a type constructor as well as 2 operations namely `unit` and `bind`. Those semantics allow monads to be easily compassable and allow a large array or manipulations. The operations on a monad can access and modify or augment its content before it is returned.

<!--monadic parser-->
Monads can also be used for parsing and mainly for parser combinators due to their compositional nature. In general the use of parser combinators would lead to a nested structure of tuples as a parse result which could either be a successful or a failure. To avoid those nested structured and repetitive checks one can use a monad which would automatically cascade a parse failure without explicit handling.
`type M a = String -> [(a, String)]`
A similarly technique has also been used for the Scala Parser combinator even though, due to to a less principled implementation, they can only be considered as partly monadic.

<!--state and reader-->
Monads can also be used to encapsulate state such that the programmer can include state information with each result, in non-functional programming this would represent information stored in variables which are not part of the parameters of a functions.
$unit: T \rightarrow S \rightarrow T \times S $
Another example is the reader monad (also called environment monad) which allows the pipelined monads to share an environment which they can read from and augment with new values.
$unit: T \rightarrow E \rightarrow T $
for instance the result of reader together with the input still left to read. This has been described in the paper "Monads for functional programming" by Philip Wadler. In general the state monad can carry any intermediate result of a partially applied evaluation whereas the reader contains individual collected result such as an environment of things encountered during a computation

<!--monad transformers-->
For the creation of the augmented parser combinator framework some influence was taken from monad transformers which are type constructors which take a monad as argument and give a monad as result. This allows to compose monads in order to get a new monad structures combining the features if the underlying ones. For instance the reader monad transformer
$unit: A \rightarrow E \rightarrow M A $
which take a environemnt and some monad as input and apply the reader transformation to the content of the monad given as argument. 

<!--###Monadic Parsing?
"internal DSLs are limited by the syntax of the host language (constraint by features of programming language)"
"external DSLs are only limited by the parser"
monad transformers allow to stack monads-->

###CFG vs CSG

$G = (V\,, \Sigma\,, R\,, S\,) $

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

###Design & Interface
The implementation is based in top of the parser combinator library such that basic combinators can be reused and more advanced features such as token parsing does not need to be redefined and 
augment parser combinators
have an environment / accumulator available if it is needed 

####AGSig
AGSignature describes the very general aspect of an abstract algebra, namely the abstract data type used to pass information between the semantic function
```scala
trait AGSig {
  type Answer
  def combine(a1:Answer, a2:Answer):Answer
}
```

The following part of the framework is already less general and is best explained using and example

####AGAlgebra
```scala
trait MyAlgebra extends AGSig {
  type Answer = List[String] //e.g. environment


  def start(tag:String, ps:List[Property], a:Answer) = tag :: a //return answer??
```

####AGGrammar
```scala
trait AGSig {
  type Answer
  def combine(a1:Answer, a2:Answer):Answer
}
```
call to syntactic functions instead of building a parse tree

####issues
As the input for a parser is generally a linear structure we need pipe the collected attributes of the *Answer* environment to the correct parser combinators. For instance when constructing an environment of all the parent nodes 
```
def Leaf:AGParser = ValueParser
def Node:AGParser = Node ~ ValueParser ~ Node
```
We cannot simply propagate the environment from left to right as the collected values of the left hand side are not parents of the right hand side. Thus we needed to a way to let the programmer express details about the structure being parsed and 

With the use of semantic functions in a parser one can easily see that the added value over syntactic information also requires for more in depth validation techniques as 

####Implementation
Due to the added environment for attribute results which needs to be passed around between parser combinators and possibly used or augmented the standard combinators need to be adapted for this use. The changes mostly influence the *sequence* and *map* combinators.
We decided to use the double arrow notation *>>* to express whenever the environment is passed on between parser to between semantic functions applied during a map. 

####sequencing: ~ / \>>~ / ~>> / \>>~>>
As explained above the inputed of a parser if often unstructured and linear and thus requires the programmer to encode the hierarchy and scope of a specific combinator manually. 
For instance the sequencing combinator can be used to combine 2 smaller parser, however each of those parser might potentially change the answer. The given notation is not to be confused with the `~>` *drop-right* combinator!
For instance the "flow" of the Answer object of the operator `parserA >>~>> parserB` can be symbolized by `parserA >> parserB >>` which means that the potentially updated environment flows both between the 2 parsers and it is also passed on to whatever follow then. On the contrary `parserA ~>> parserB` means that only the 
The full interaction can be expressed in a graph
<img src="ans_piping.png" width="500px" style="align:center"\>
Some confusion might occure due to the left associativity of parser combinators such that
`parserA ~>> parserB ~ parserC` means the same than `parserA ~ (parserB >>~ parserC)`
 Additionally the programmer might need to use paratheses in order to keep the environment in the correct scope.


####mapping: ^^ / \>>^^ / ^^>> / \>>^^>>
Whenever a mapping function is applied on the result of a parser this functionality might influence the attribute environment which thus needs updating. On the other hand some semantic functions applied to a parse result might also need to read the attribute environment in order to apply a specific functionality. We used the same notation to express the different between mapping function which need to access the environment in order to apply the semantic function and the ones that augment the existing environment with new attribute values.
The function signatures look as follows:

```scala
def ^^[U](f: T => U) = map(f)
def >>^^[U](f: (T, Answer) => U) = mapWithAns(f)
def ^^>>[U](f: T => U, add:(Answer, U) => Answer) = mapIntoAns(f, add)
def >>^^>>[U](f: (T, Answer) => (Answer, U)) = mapWithAnsIntoAns(f)
```

####ParseResult
Obviously the attributes included the "Answer" need to be stored somewhere such that they can be passed around implicitly without whenever they are not used in the 

###Examples
The use of the framework is best illustrated by concrete examples which explain the use of the "Answer" environment. We will show several example to explain the use of AGParser as collector and transformers.

####Typing
Calculating the type of an expression is a typical case of a transformation which will return different information of a syntactic tree than what's given at the input.

####RepMin
Repmin is a famous example where, with a tree given as input we want to create the same tree 
We use it as a prime example to show that the result of the semantic functions can be pipelined through the tree. We think that it is an interesting example which shows that attributes can be used anywhere in the tree and that the constructed can be substantially different from the original object without ever constructing an intermediate representation.
This example was implemented on a tree of integers. In the given example the only attribute we need to keep track of is the minimum value found so far which converges to the global minimum. As the global minimum is not immediately available we cannot reconstruct the corresponding nodes based on local information. Due to this constraint we build semantic functions to return itself a function which will construct the final tree only once the minimum value is found. This creates some kind of call-back tree similar to continuation passing style where each returned function waits to be fed with the found minimum. Once the whole tree as been read we also have found the global 

```scala
trait RepMinSig extends AGSig {
  type TreeF = Answer => Tree //evaluate when minimum is found

  def node(a1:TreeF, i:Answer, a2:TreeF):TreeF
  def leaf: TreeF
}
```
The signature gives an abstract high level definition of the computation one might want to perform on each node.

```scala
trait RepMinGrammar extends AGParsers with RepMinSig {
  def Root:AGParser[Tree] = { //fully evaluate at the end!
    TreeP >>^^ {case (t, ans) => t(ans)} |
      lift(failure("tree not parsable!"))
  }
  def TreeP:AGParser[TreeF] = {NodeP | LeafP}
  def NodeP:AGParser[TreeF] = {
    lift("(") >>~>> TreeP >>~>> ValP >>~>> TreeP >>~>> lift(")") >>^^>> {
      case (s1 ~ a1 ~ a ~ a2 ~ s2, ans) => (combine(ans, a), node(a1, a, a2))
    }
  }
```
As you can see the root parser is used to capture the global minimum and use it to evaluate the constructed function which will yield as a result a tree of the same shape as the original one with each value replaced with the minimum.

```scala
trait RepMinAlgebra extends RepMinSig {
  type Answer = Int

  def node(l:TreeF, a:Answer, r:TreeF): TreeF = {case i:Int => Node(l(i), i, r(i))}
  def leaf = {case _:Int => Leaf}
}
```
The algebra give a concrete implementation of the abstract semantic function


Html (Collector)

###Limitations and future work
With the current framework all attributes need to be declared explicitly in the Answer type as they are not directly associated with the nodes themselves but rather present some global information of the parsing process which can be used and augmented by consecutive parsers.
As the parsing process generally works from left to right the only way to use attributes from a right hand side parser (e.g. value of a tree node) is by using the the lazy evaluation workaround used by the `RepMin` example. 
Using several attribute algebras at once is tedious and requires manual composition as the Answer type needs to be adapted by hand to carry both calculated values.
The current way of manually specifying where the Answer result should be 

###related work
(Jonas: object algebra to attribute grammar)
####Kiama
*Kiama is a Scala library for language processing which allows analysis and transformation on structured data using formal languages processing paradigms such as attribute grammars and tree rewriting.*
Through the use of Scala macros, Kiama augments existing tree structures with named or unnamed attribute function which can then be used to evaluate local or global properties.
`def attr[T,U] (f : T => U) : CachedAttribute[T,U] = macro AttributionCoreMacros.attrMacro[T,U,CachedAttribute[T,U]]`
This makes the relation between the parents and child nodes in a tree explicit and thus allows the different attribute method to access each other. We wanted to avoid constructing an additional structure on top of the tree structure and actually completely dismiss the original parse tree if it is not needed for afterwards. With the AGParser it is possible to create parent-child and child-parents calls however do they need to be explicit.
Kiama is also able to treat more general graphs and thus to handle cyclic references

####TQL
The tree query language presented by Eric Beguet offered an interesting insight on tree traversals and transformers even though they have been applied in the less general context of Scala meta trees. It can use different traversal techniques such as top-down or bottom-up or even break a specific traversal depending on the 

haskell paper
Monad paper (Reader, Env)

####Generalising Tree Traversals to DAGs
In their paper Bahr and Axelsson present generalized recursion schemes based on attribute grammars which can be applied to trees and graphs, namely DAGs. The main issue with attribute grammars on DAGs is that the different attributes might be recomputed for Nodes which share the same branches and the paper presents a method to avoid this. The fold operations presented in previous work cannot be applied as some parameters of shared nodes might need 

####DynaProg
Some inspiration has been taken from the work done by Thierry Coppey who created a parsing framework which would use dynamic programing in order to construct the most efficient parsing tree. Most importantly we 
Mostly the separation of the framework into an abstract signature, a concrete algebra and a grammar combining using the abstract definition such that it can be composed with any concrete algebra has been influenced by this work.


###Staging with LMS
As the AGParsers add another layer of abstraction on top of the already quite slow implementation of the Parser combinators we thought that it would be a good idea to stage the framework using LMS such that efficient code can be emitted. This method has been proven very helpful and fast in the development of *FastParser* (which is macro based and still 2x faster than the LMS version).
However does there not yet exist a finished LMS implementation of the *StandardTokenParsers* which could have been used as a solid starting point. Thus we tried to apply the method to a more basic CharParsers example which is implemented and has been tested.
However do to a limit of time we did finish the implementation of the staged version of the AGParsers even though it would be interesting to see the efficiency gain.

####CharParsers example $a^nb^nc^n$

###Acknoledgments
I would like to thank both my official and unofficial supervisor for the very interesting and fruitful discussions. Through weekly meetings we were able to explore the different options as well a limitations caused by either the language or the choice of implementation such that the development of the project also helped to explore the more theoretical side of the 

###references:
attribute grammars: 1967 Semantics of context-free languages, by Don Knuth, 
combinator parsing (Wadler, 1985; Hutton, 1992; Fokker, 1995),
Monadic Parser Combinator, Hutton+Meijer 1996, https://www.cs.nott.ac.uk/~gmh/monparsing.pdf 
scala parser combinators: https://github.com/scala/scala-parser-combinators
Kiama: https://code.google.com/p/kiama/
TQL by Eric
DynaProg - Thierry Coppey
why attribute grammars matter: https://wiki.haskell.org/The_Monad.Reader/Issue4/Why_Attribute_Grammars_Matter
