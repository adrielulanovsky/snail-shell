object MatrixUtil {
  type Matrix = Vector[Vector[Int]]

  def toMatrix(s: String): Matrix = {
    // Split the input string into rows
    val rows = s.stripMargin.trim.split("\n")

    // Convert each row into a vector of integers
    rows.map(row => row.split(" ").map(_.toInt).toVector).toVector
  }

  implicit class MatrixOps(val matrix: Matrix) extends AnyVal {
    def update(i: Int)(j: Int)(value: Int): Matrix = matrix.updated(j, matrix(j).updated(i, value))

    def show: String = {
      val rows = for {
        row <- matrix
      } yield {
        row.mkString(",\t")
      }
      rows.mkString("\n")
    }
  }
}
