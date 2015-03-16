package agPC

/**
 * Created by cedricbastin on 16/03/15.
 */

/*
| is the alternation combinator. It says “succeed if either the left or right operand parse successfully”
~ is the sequential combinator. It says “succeed if the left operand parses successfully, and then the right parses successfully on the remaining input”
~> says “succeed if the left operand parses successfully followed by the right, but do not include the left content in the result”
<~ is the reverse, “succeed if the left operand is parsed successfully followed by the right, but do not include the right content in the result”
^^=> is the transformation combinator. It says “if the left operand parses successfully, transform the result using the function on the right”
rep => simply says “expect N-many repetitions of parser X” where X is the parser passed as an argument to rep
 */

/*
RegexParsers
    def floatingPointNumber: Parser[String] = {
        """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    }
    def operator: Parser[(Float, Float) => Float] = ("*" | "/" | "+" | "-") ^^ {
        case "+" => (x, y) => x + y
        case "-" => (x, y) => x - y
        case "*" => (x, y) => x * y
        case "/" => (x, y) => if (y > 0) (x / y) else 0.f
    }
 */

object ParserCombinator {
  println("hegrsldfsgdflo")
}
