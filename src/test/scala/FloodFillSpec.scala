import org.scalatest._

class FloodFillSpec extends FlatSpec with Matchers {
  "The FloodFill object" should "return size" in {
    val grid = Seq(
      "****",
      "*oo*",
      "*o**",
      "oooo")

    FloodFill.size(0, 0, grid) shouldEqual(0)

    FloodFill.size(1, 1, grid) shouldEqual(7)
  }

  "The FloodFill object" should "return size with a 1x1 grid" in {
    FloodFill.size(0, 0, Seq("*")) shouldEqual(0)
    FloodFill.size(0, 0, Seq("o")) shouldEqual(1)
  }

  "The FloodFill object" should "return size with a 1x3 grid" in {
    FloodFill.size(0, 0, Seq("*", "*", "o")) shouldEqual(0)

    FloodFill.size(0, 0, Seq("o", "o", "*")) shouldEqual(2)
    FloodFill.size(0, 1, Seq("o", "o", "*")) shouldEqual(2)
    FloodFill.size(0, 2, Seq("o", "o", "*")) shouldEqual(0)
  }

  "The FloodFill object" should "return size with a 3x1 grid" in {
    FloodFill.size(0, 0, Seq("***")) shouldEqual(0)

    FloodFill.size(0, 0, Seq("oo*")) shouldEqual(2)
    FloodFill.size(1, 0, Seq("oo*")) shouldEqual(2)
    FloodFill.size(2, 0, Seq("oo*")) shouldEqual(0)
  }

}
