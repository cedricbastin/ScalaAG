package agParsers

/**
 * Created by cedricbastin on 26/04/15.
 */
trait AGSig {
  //ADP example:
  //type Alphabet // input type
  //type Answer // output type

  type Answer //contains environment somehow! we don't want to add another type parameter to AGParser which would also only give limited functionality
  //def defaultAnswer: Option[Answer]
  //could contain something like Answer(inheritedEnv, Value, synthezisedEnv)
  type TopEnv
  type TopEnvElem
  type BotEnv
  type BotEnvElem
  //def addToTopEnv[E](a:Answer, e:TopEnv): Answer
  //def addToBotEnv[E](a:Answer, e:BotEnv): Answer
}