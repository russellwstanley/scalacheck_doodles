import demo.{RestApi, RestApiJava}
import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.commands.Commands
import org.scalacheck.Arbitrary.arbitrary

import scala.util.{Failure, Success, Try}

object RestApiSpec extends Properties("RestApi") {
  property("restapuispec") = CommandsRestApi.property(threadCount = 1)
}
object CommandsRestApi extends Commands {

  type State = Map[String,Int]
  type Sut = RestApiJava

  override def canCreateNewSut(newState: State, initialStates: Traversable[State], runningSuts: Traversable[Sut]): Boolean = true

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = true

  override def genInitialState: Gen[State] = Map[String,Int]()

  override def newSut(state: State): Sut = new RestApiJava()

  def ids(state : State) = {
    if(state.isEmpty) newId(state)
    else Gen.oneOf(existingIds(state),newId(state))
  }

  def existingIds(state : State) = Gen.oneOf(state.keys.toSeq)
  def newId(state : State) = Gen.alphaStr.filter(s => !state.contains(s))

  def arbitaryGet(state : State)  = {
    for {
      id <- ids(state)
    } yield GET(id)
  }

  def arbitaryPut(state : State) = for {
    id <- ids(state)
    value <- arbitrary[Int]
  } yield PUT(id,value)

  def arbitaryDelete(state: State) = for {
    id <- ids(state)
  } yield DELETE(id)

  override def genCommand(state: State): Gen[CommandsRestApi.Command] = Gen.oneOf(
    arbitaryDelete(state),
    arbitaryPut(state),
    arbitaryGet(state)
  )

  case class PUT(id : String, item : Int) extends Command {
    type Result = String

    override def run(sut: Sut): String = sut.PUT(id,item)

    override def preCondition(state: State): Boolean = true

    override def postCondition(stateBefore: State, result: Try[String]): Prop =  result.isSuccess && result.get == id

    override def nextState(state: State): State = state + (id -> item)
  }

  case class DELETE(id : String) extends Command {

    type Result = Boolean

    override def run(sut: Sut): Boolean = sut.DELETE(id)

    override def preCondition(state: Map[String, Int]): Boolean = true

    override def postCondition(stateBefore: Map[String, Int], result: Try[Boolean]): Prop = {
      result.isSuccess && result.get == stateBefore.contains(id)
    }
    override def nextState(state: Map[String, Int]): Map[String, Int] = state - id
  }

  case class GET(id : String) extends Command {
    type Result = Int

    override def run(sut: Sut): Int = sut.GET(id)

    override def preCondition(state: Map[String, Int]): Boolean = true

    override def postCondition(state: Map[String, Int], result: Try[Int]): Prop = {
      if(! state.contains(id)) result.isFailure
      else result.isSuccess && result.get == state(id)
    }

    override def nextState(state: Map[String, Int]): Map[String, Int] = state
  }




}
