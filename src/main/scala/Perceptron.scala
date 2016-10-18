import java.util.Random

case class Point(x: Double, y: Double)
case class PointWithSign(p: Point, sign: Boolean)

object Perceptron {
  val generator = new Random()

  def getPoint() = {
    Point(generator.nextDouble() * 2 - 1, generator.nextDouble() * 2 - 1)
  }

  def getPoints(n: Int) = Iterator.fill(n)(getPoint()).toVector

  def getFunctionFromWeights(weights: List[Double]): Double => Double = ???

  def getFunction(a: Point, b: Point) =
    (x: Double) =>
      b.y - ((b.y - a.y) / (b.x - a.x)) + ((b.y - a.y) / (b.x - a.x)) * x

  def isAboveFunction(p: Point, f: Double => Double): Boolean =
    f(p.x) < p.y

  def isMisclassified(p: PointWithSign, f: Double => Double): Boolean =
    isAboveFunction(p.p, f) != p.sign

  def getRandomMisclassified(points: Vector[PointWithSign], f: Double => Double): PointWithSign = {
    val misclassified = points.filter(p => isMisclassified(p, f))
    misclassified(generator.nextInt(misclassified.size))
  }

  def go(weights: List[Double], points: Vector[PointWithSign], tF: Double => Double): List[Double] = {
    val learningFunction = getFunctionFromWeights(weights)
    val misclassified = points.filter(p => isMisclassified(p, learningFunction))

    println(s"Current weights: $weights")
    if (true) {
      println(s"num points: ${points.size} num misclassified: ${misclassified.size} ratio: ${misclassified.size / points.size}")
    }

    // readjust weights based on picking random misclassified point
    if (misclassified.nonEmpty) {
      val mP = getRandomMisclassified(points, tF)
      val isAbove = isAboveFunction(mP.p, tF)
      val y = if (isAbove) 1 else -1
      go(weights.zip(1.0 :: List(mP.p.x, mP.p.y)).map{ case (w, x) => w + y * x }, points, tF)
    }
    else
      weights
  }

  /** d: dimension, n: number of points for learning **/
  def pla(d: Int, n: Int): Double => Double = {
    val initialWeights = 1.0 :: Iterator.fill(d)(0.0).toList
    val tF = getFunction(getPoint(), getPoint())
    val points = getPoints(n).map(p => PointWithSign(p, isAboveFunction(p, tF)))
    val weights = go(initialWeights, points, tF)

    getFunctionFromWeights(weights)
  }

  def main(args: Array[String]): Unit = {
  }
}
