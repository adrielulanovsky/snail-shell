import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SnailShellSpec extends AnyFlatSpec with Matchers {
  import SnailShell.createSnailShell

  "createSnailShell" should "generate a snail shell pattern for X=5" in {
    val result = createSnailShell(5)
    val expected: Array[Array[Int]] = Array(
      Array( 1,  2,  3,  4, 5),
      Array(16, 17, 18, 19, 6),
      Array(15, 24, 25, 20, 7),
      Array(14, 23, 22, 21, 8),
      Array(13, 12, 11, 10, 9)
    )
    result shouldEqual expected
  }

  "createSnailShell" should "generate a snail shell pattern for X=4" in {
    val result = createSnailShell(5)
    val expected: Array[Array[Int]] = Array(
      Array( 1,  2,  3, 4),
      Array(12, 13, 14, 5),
      Array(11, 16, 15, 6),
      Array(10,  9,  8, 7)
    )
    result shouldEqual expected
  }

  it should "generate an array with 1 element for X=1" in {
    val result = createSnailShell(1)
    result shouldEqual Array(Array(1))
  }

  it should "generate an empty array for X=0" in {
    val result = createSnailShell(1)
    result shouldEqual Array.empty
  }
}
