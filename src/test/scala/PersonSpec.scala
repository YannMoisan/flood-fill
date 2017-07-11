import org.scalatest.{FlatSpec, Matchers}

class PersonSpec extends FlatSpec with Matchers {
  "The Person object" should "parse" in {
    1 shouldEqual(1)
  }
}