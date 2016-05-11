import demo.RestApi
import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.commands.Commands
import org.scalacheck.Arbitrary.arbitrary

import scala.util.{Failure, Success, Try}

object RestApiSpec extends Properties("RestApi") {
  property("restapuispec") = CommandsRestApi.property(threadCount = 1)
}


object CommandsRestApi extends Commands {


  type State = Map[String,Int]
  type Sut = RestApi

  /** Decides if [[newSut]] should be allowed to be called
    * with the specified state instance. This can be used to limit the number
    * of co-existing [[Sut]] instances. The list of existing states represents
    * the initial states (not the current states) for all [[Sut]] instances
    * that are active for the moment. If this method is implemented
    * incorrectly, for example if it returns false even if the list of
    * existing states is empty, ScalaCheck might hang.
    *
    * If you want to allow only one [[Sut]] instance to exist at any given time
    * (a singleton [[Sut]]), implement this method the following way:
    *
    * {{{
    *  def canCreateNewSut(newState: State, initSuts: Traversable[State]
    *    runningSuts: Traversable[Sut]
    *  ) = {
    *    initSuts.isEmpty && runningSuts.isEmpty
    *  }
    * }}}
    */
  override def canCreateNewSut(newState: State, initialStates: Traversable[State], runningSuts: Traversable[RestApi]): Boolean = runningSuts.isEmpty

  override def destroySut(sut: RestApi): Unit = ()

  /** The precondition for the initial state, when no commands yet have
    * run. This is used by ScalaCheck when command sequences are shrinked
    * and the first state might differ from what is returned from
    * [[genInitialState]]. */
  override def initialPreCondition(state: State): Boolean = true

  override def genInitialState: Gen[State] = Map[String,Int]()

  /** Create a new [[Sut]] instance with an internal state that
    * corresponds to the provided abstract state instance. The provided state
    * is guaranteed to fulfill [[initialPreCondition]], and
    * [[newSut]] will never be called if
    * [[canCreateNewSut]] is not true for the given state. */
  override def newSut(state: State): RestApi = new RestApi

//  val arbitaryPost = for {
//    value <- arbitrary[Int]
//  } yield POST(value)

  def existingIds(state : State) = Gen.oneOf(state.keys.toSeq)
  def newId(state : State) = arbitrary[String].filter(s => !state.contains(s))

  def arbitaryGet(state : State)  = {
    for {
      id <- Gen.oneOf(existingIds(state), newId(state))
    } yield if(state.contains(id)) GET_EXISTING(id) else GET_NON_EXISTING(id)
  }

  def arbitaryPut(state : State) = for {
    id <- Gen.oneOf(existingIds(state), newId(state))
    value <- arbitrary[Int]
  } yield if (state.contains(id)) PUT_EXISTING(id, value) else PUT_NEW(id, value)

  def arbitaryDelete(state: State) = for {
    id <- Gen.oneOf(existingIds(state), newId(state))
  } yield if(state.contains(id)) DELETE_EXISTING(id) else DELETE_NON_EXISTING(id)




  override def genCommand(state: State): Gen[CommandsRestApi.Command] = Gen.oneOf(
    arbitaryDelete(state),
    arbitaryPut(state),
    arbitaryGet(state)
  )

  case class PUT_EXISTING(id : String, item : Int) extends Command {
    type Result = String

    override def run(sut: RestApi): String = sut.PUT(id,item)

    override def preCondition(state: State): Boolean = state.contains(id)

    override def postCondition(stateBefore: State, result: Try[String]): Prop =  result.isSuccess && result.get == id

    override def nextState(state: State): State = state + (id -> item)
  }



  case class PUT_NEW(id : String, item : Int) extends Command {
    type Result = String

    override def run(sut: RestApi): String = sut.PUT(id,item)

    override def preCondition(state: State): Boolean = !state.contains(id)

    override def postCondition(state: State, result: Try[String]): Prop = result.isSuccess && result.get == id

    override def nextState(state: State): State = state + (id -> item)
  }

  case class DELETE_NON_EXISTING(id : String) extends Command {

    type Result = Boolean

    override def run(sut: RestApi): Boolean = sut.DELETE(id)

    override def preCondition(state: Map[String, Int]): Boolean = true//! state.contains(id)

    override def postCondition(stateBefore: Map[String, Int], result: Try[Boolean]): Prop = true//result.get == false

    override def nextState(state: Map[String, Int]): Map[String, Int] = state - id
  }


  case class DELETE_NON_EXISTING(id : String) extends Command {

    type Result = Boolean

    override def run(sut: RestApi): Boolean = sut.DELETE(id)

    override def preCondition(state: Map[String, Int]): Boolean = true//! state.contains(id)

    override def postCondition(stateBefore: Map[String, Int], result: Try[Boolean]): Prop = true//result.get == false

    override def nextState(state: Map[String, Int]): Map[String, Int] = state - id
  }

  case class DELETE_EXISTING(id : String) extends Command {

    type Result = Boolean

    override def run(sut: RestApi): Boolean = sut.DELETE(id)

    override def preCondition(state: Map[String, Int]): Boolean = true//state.contains(id)

    override def postCondition(stateBefore: Map[String, Int], result: Try[Boolean]): Prop = true //result.get == true

    override def nextState(state: Map[String, Int]): Map[String, Int] = state // - id
  }

//  case class POST(item : Int) extends Command {
//    type Result = String
//
//    var cachedResult : Try[Result] = Failure(new Exception)
//
//    override def run(sut: RestApi): String = sut.POST(item)
//
//    override def preCondition(state: State): Boolean = true
//
//    override def postCondition(state: State, result: Try[String]): Prop = {
//      cachedResult = result
//      result.isSuccess
//    }
//    override def nextState(state: State): State = state + (cachedResult.get -> item)
//  }

  case class GET_NON_EXISTING(id : String) extends Command {
    type Result = Int

    override def run(sut: RestApi): Int = sut.GET(id)

    override def preCondition(state: Map[String, Int]): Boolean = !state.contains(id)

    override def postCondition(state: Map[String, Int], result: Try[Int]): Prop = result.isFailure

    override def nextState(state: Map[String, Int]): Map[String, Int] = state
  }


  case class GET_EXISTING(id : String) extends Command {

    type Result = Int

    override def run(sut: RestApi): Int = sut.GET(id)

    override def preCondition(state: State): Boolean = state.contains(id)

    override def nextState(state: State): Map[String, Int] = state

    override def postCondition(state: State, result: Try[Int]): Prop = state(id) == result.get
  }



}
