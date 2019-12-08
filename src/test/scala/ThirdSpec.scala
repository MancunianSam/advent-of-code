
import org.scalatest._
import Third.findShortestPath

class ThirdSpec extends FlatSpec with Matchers {
  "The shortest path " should "match the correct values" in {
      val firstWire = List("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51")
      val secondWire = List("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
    val shortestPath: Int = findShortestPath(firstWire, secondWire)
    shortestPath should be (135)
  }
}
