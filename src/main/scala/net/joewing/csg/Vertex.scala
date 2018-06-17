package net.joewing.csg

import breeze.linalg.{DenseMatrix, DenseVector}

case class Vertex(x1: Double, x2: Double, x3: Double) {

  override def toString: String = f"[$x1%.2f, $x2%.2f, $x3%.2f]"

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

  def moved(d1: Double, d2: Double, d3: Double): Vertex = Vertex(x1 + d1, x2 + d2, x3 + d3)

  def dot(other: Vertex): Double = x1 * other.x1 + x2 * other.x2 + x3 * other.x3

  def cross(other: Vertex): Vertex = Vertex(
    x2 * other.x3 - x3 * other.x2,
    x3 * other.x1 - x1 * other.x3,
    x1 * other.x2 - x2 * other.x1
  )

  def +(other: Vertex): Vertex = Vertex(x1 + other.x1, x2 + other.x2, x3 + other.x3)

  def -(other: Vertex): Vertex = Vertex(x1 - other.x1, x2 - other.x2, x3 - other.x3)

  def *(s: Double): Vertex = Vertex(x1 * s, x2 * s, x3 * s)

  def /(s: Double): Vertex = Vertex(x1 / s, x2 / s, x3 / s)

  def length: Double = math.sqrt(dot(this))

  def unit: Vertex = this / length

  def interpolate(other: Vertex, d: Double): Vertex = this + (other - this) * d

  def negated: Vertex = Vertex(-x1, -x2, -x3)

  def approxEqual(other: Vertex, epsilon: Double = 1e-6): Boolean = {
    math.abs(x1 - other.x1) < epsilon && math.abs(x2 - other.x2) < epsilon && math.abs(x3 - other.x3) < epsilon
  }
}
