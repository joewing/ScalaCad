package net.joewing.scalacad

import breeze.linalg.{DenseMatrix, DenseVector}

case class Vertex(x1: Double, x2: Double, x3: Double) {

  override def toString: String = f"[$x1%.4f, $x2%.4f, $x3%.4f]"

  private def rx(radians: Double): DenseMatrix[Double] = DenseMatrix(
    DenseVector(1.0, 0.0, 0.0),
    DenseVector(0.0, math.cos(radians), -math.sin(radians)),
    DenseVector(0.0, math.sin(radians), math.cos(radians))
  )

  private def ry(radians: Double): DenseMatrix[Double] = DenseMatrix(
    DenseVector(math.cos(radians), 0.0, math.sin(radians)),
    DenseVector(0.0, 1.0, 0.0),
    DenseVector(-math.sin(radians), 0.0, math.cos(radians))
  )

  private def rz(radians: Double): DenseMatrix[Double] = DenseMatrix(
    DenseVector(math.cos(radians), -math.sin(radians), 0.0),
    DenseVector(math.sin(radians), math.cos(radians), 0.0),
    DenseVector(0.0, 0.0, 1.0)
  )

  def scaled(x: Double = 1.0, y: Double = 1.0, z: Double = 1.0): Vertex = {
    Vertex(x1 * x, x2 * y, x3 * z)
  }

  def rotated(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Vertex = {
    val v = rx(x) * ry(y) * rz(z) * DenseVector(x1, x2, x3)
    Vertex(v(0), v(1), v(2))
  }

  def moved(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Vertex =
    Vertex(x1 + x, x2 + y, x3 + z)

  def dot(other: Vertex): Double = x1 * other.x1 + x2 * other.x2 + x3 * other.x3

  def dotSelf: Double = dot(this)

  def cross(other: Vertex): Vertex = Vertex(
    x2 * other.x3 - x3 * other.x2,
    x3 * other.x1 - x1 * other.x3,
    x1 * other.x2 - x2 * other.x1
  )

  def +(other: Vertex): Vertex = Vertex(x1 + other.x1, x2 + other.x2, x3 + other.x3)

  def -(other: Vertex): Vertex = Vertex(x1 - other.x1, x2 - other.x2, x3 - other.x3)

  def *(s: Double): Vertex = Vertex(x1 * s, x2 * s, x3 * s)

  def /(s: Double): Vertex = Vertex(x1 / s, x2 / s, x3 / s)

  def length: Double = math.sqrt(dotSelf)

  def unit: Vertex = if (length > 0) this / length else this

  def interpolate(other: Vertex, d: Double): Vertex = this + (other - this) * d

  def negated: Vertex = Vertex(-x1, -x2, -x3)

  def approxEqual(other: Vertex, epsilon: Double = Vertex.epsilon): Boolean = {
    math.abs(x1 - other.x1) < epsilon && math.abs(x2 - other.x2) < epsilon && math.abs(x3 - other.x3) < epsilon
  }

  def min(right: Vertex): Vertex = Vertex(math.min(x1, right.x1), math.min(x2, right.x2), math.min(x3, right.x3))

  def max(right: Vertex): Vertex = Vertex(math.max(x1, right.x1), math.max(x2, right.x2), math.max(x3, right.x3))

  def collinear(a: Vertex, b: Vertex, epsilon: Double = Vertex.epsilon): Boolean =
    (b - a).cross(this - a).length < epsilon

  // Find 't' such that 'a + t(b - a) = this'.
  // If this returns t in (0, 1), then this is between a and b.
  def solve(a: Vertex, b: Vertex): Double = {
    if (math.abs(a.x1 - b.x1) > Vertex.epsilon) (x1 - a.x1) / (b.x1 - a.x1)
    else if (math.abs(a.x2 - b.x2) > Vertex.epsilon) (x2 - a.x2) / (b.x2 - a.x2)
    else if (math.abs(a.x3 - b.x3) > Vertex.epsilon) (x3 - a.x3) / (b.x3 - a.x3)
    else 0.0
  }

  // Returns true if this is strictly between a and b.
  def between(a: Vertex, b: Vertex, epsilon: Double = Vertex.epsilon): Boolean = {
    val t = solve(a, b)
    t >= epsilon && t <= 1 - epsilon
  }
}

object Vertex {
  val epsilon: Double = 1e-9

  implicit object VertexOrdering extends Ordering[Vertex] {
    def compare(x: Vertex, y: Vertex): Int = {
      if (math.abs(x.x1 - y.x1) < epsilon) {
        if (math.abs(x.x2 - y.x2) < epsilon) {
          if (math.abs(x.x3 - y.x3) < epsilon) 0
          else if (x.x3 < y.x3) -1 else 1
        } else if (x.x2 < y.x2) -1 else 1
      } else if (x.x1 < y.x1) -1 else 1
    }
  }
}
