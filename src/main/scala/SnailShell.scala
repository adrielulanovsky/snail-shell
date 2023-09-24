import MatrixUtil._

import scala.util.Try
import scala.util.control.TailCalls.{TailRec, done, tailcall}

object SnailShell extends App {
  /**
   * To solve this problem, I decided to go with a functional programming approach. I decided to use Vector (immutable)
   * instead of Array for the auxiliary functions, and created a type alias Matrix for ease of use.
   *
   * The algorithm uses 4 mutually recursive auxiliary functions: leftToRight, upToDown, rightToLeft, and downToUp.
   * These functions represent updating a row or a column of the matrix in the named direction, and then calling the next
   * helper function with the correct direction.
   *
   * Since they're mutually recursive, I decided to use TailRec to avoid stack overflow errors for large N.
   */

  def createSnailShell(n: Int): Array[Array[Int]] = {
    val matrix: Matrix = snailShell(n)
    matrix.map(_.toArray).toArray
  }

  def snailShell(n: Int): Matrix = {
    val matrix: Matrix = Vector.fill(n)(Vector.fill(n)(1))
    leftToRight(matrix, 1, 0).result
  }

  private def downToUp(matrix: Matrix, initial: Int, x: Int): TailRec[Matrix] = {
    val startY = matrix.length - x - 2 //One up previous
    val endY = x + 1
    if (startY < endY) done(matrix)
    else {
      val updatedMatrix = (startY to endY by -1).foldLeft(matrix)((m, y) => m.update(x)(y)(initial - y + startY))
      tailcall(leftToRight(
        updatedMatrix,
        initial + startY - endY + 1,
        endY
      ))
    }
  }

  private def rightToLeft(matrix: Matrix, initial: Int, y: Int): TailRec[Matrix] = {
    val startX = y - 1 //One left of previous
    val endX = matrix.length - y - 1
    if (startX < endX) done(matrix)
    else {
      val updatedMatrix = (startX to endX by -1).foldLeft(matrix)((m, x) => m.update(x)(y)(initial - x + startX))
      tailcall(downToUp(
        updatedMatrix,
        initial + startX - endX + 1,
        endX
      ))
    }
  }

  private def upToDown(matrix: Matrix, initial: Int, x: Int): TailRec[Matrix] = {
    val startY = matrix.length - x //One below previous
    val endY = x
    if (endY < startY) done(matrix)
    else {
      val updatedMatrix = (startY to endY).foldLeft(matrix)((m, y) => m.update(x)(y)(initial + y - startY))
      tailcall(rightToLeft(
        updatedMatrix,
        initial + endY - startY + 1,
        endY
      ))
    }
  }

  private def leftToRight(matrix: Matrix, initial: Int, y: Int): TailRec[Matrix] = {
    val startX = y
    val endX = matrix.length - y - 1
    if (endX < startX) done(matrix)
    else {
      val updatedMatrix = (startX to endX).foldLeft(matrix)((m, x) => m.update(x)(y)(initial + x - startX))
      tailcall(upToDown(
        updatedMatrix,
        initial + endX - startX + 1,
        endX
      ))
    }
  }

  val n = args.headOption.flatMap(n => Try(n.toInt).toOption).getOrElse(0)
  println(snailShell(n).show)
}