package agStaged

/**
 * Created by cedricbastin on 24/05/15.
 */

import scala.virtualization.lms.common._
import lms._
import lms.util._
import stagedparsec.ScalaGenStringReaderOps
import stagedparsec.StringReader
import stagedparsec.StringReaderOps
import stagedparsec.StringReaderOpsExp

/**
 * This trait contains basic parsers for characters
 * as input elements
 */
trait AGCharParsers
  extends AGStagedParsers
  with CharOps
  with StringReaderOps {

  /**
   * The elementary parser. Accepts a character if it passes
   * the given predicate
   */
  def acceptIf(p: Rep[Elem] => Rep[Boolean]):AGParser[Char] = AGParser[Char] { (ans, in) =>
    if (in.atEnd) AGFailure[Char](in)
    else if (p(in.first)) AGSuccess(in.first, in.rest, ans) //pipe ans?
    else AGFailure[Char](in)
  }

//  def accept(e: Rep[Elem]): Parser[Char] = acceptIf(_ == e)
//
//  /**
//   * elementary recognisers. Parse a character, simply return
//   * the next index to be processed from
//   */
//  def acceptIdx(e: Rep[Elem]): Parser[Int] = acceptIfIdx(_ == e)
//
//  def acceptIfIdx(p: Rep[Elem] => Rep[Boolean]) = Parser[Int] { in =>
//    if (in.atEnd) Failure[Int](in)
//    else if (p(in.first)) Success(in.offset, in.rest)
//    else Failure[Int](in)
//  }
//
//  def isLetter(c: Rep[Char]): Rep[Boolean] =
//    (c >= unit('a') && c <= unit('z')) ||
//      (c >= unit('A') && c <= unit('Z'))
//
//  def letter: Parser[Char] = acceptIf(isLetter)
//  def letterIdx = acceptIfIdx(isLetter)
//
//  def isDigit(c: Rep[Char]): Rep[Boolean] =
//    c >= unit('0') && c <= unit('9')
//
//  def digit: Parser[Char] = acceptIf(isDigit)
//  def digit2Int: Parser[Int] = digit map (c => (c - unit('0')).toInt)
//  def digitIdx = acceptIfIdx(isDigit)

}

trait AGCharParsersExp
  extends AGCharParsers
  with AGStagedParsersExp
  with CharOpsExp
  with StringReaderOpsExp

trait ScalaGenAGCharParsers
  extends ScalaGenAGStagedParsers
  with ScalaGenCharOps
  with ScalaGenStringReaderOps {
  val IR: AGCharParsersExp
}