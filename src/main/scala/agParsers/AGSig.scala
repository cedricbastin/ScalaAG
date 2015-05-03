package agParsers

/**
 * Created by cedricbastin on 26/04/15.
 */
trait AGSig {
  /**
   * Answer contains information which is piped around during parsing
   * It can contain the result of the current parsing
   * it can contain an environment of inherited attributes
   * it can contain an environment of synthecised attributed
   * the user can define those environments/ attributes as he likes
   */

  type Answer
  type AnswerF = Answer => Answer //lazily computed environment
  def combine(a1:Answer, a2:Answer):Answer
}