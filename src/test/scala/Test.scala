import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object ScalaCheckDemo extends Properties("RestDemo") {

  property("rest verbs should work correctly") = forAll { l: List[Int] =>
    l.reverse.reverse == l
  }

}
