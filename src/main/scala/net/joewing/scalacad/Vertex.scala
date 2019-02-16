package net.joewing.scalacad

final case class Vertex(x: Double, y: Double, z: Double) {

  override def toString: String = f"[$x%.4f, $y%.4f, $z%.4f]"

  def scaled(x: Double = 1.0, y: Double = 1.0, z: Double = 1.0): Vertex = {
    Vertex(this.x * x, this.y * y, this.z * z)
  }

  def rotated(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Vertex = {
    val cx = math.cos(x)
    val cy = math.cos(y)
    val cz = math.cos(z)
    val sx = math.sin(x)
    val sy = math.sin(y)
    val sz = math.sin(z)
    Vertex(
      this.x * cy * cz - this.y * cy * sz + this.z * sy,
      this.x * (sx * sy * cz + cx * sz) + this.y * (cx * cz - sx * sy * sz) - this.z * sx * cz,
      this.x * (sx * sz - cx * sy * cz) + this.y * (cx * sy * sz + sx * cz) + this.z * cx * cy
    )
  }

  def moved(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Vertex =
    Vertex(this.x + x, this.y + y, this.z + z)

  def dot(other: Vertex): Double = x * other.x + y * other.y + z * other.z

  def dotSelf: Double = dot(this)

  def cross(other: Vertex): Vertex = Vertex(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x
  )

  def +(other: Vertex): Vertex = Vertex(x + other.x, y + other.y, z + other.z)

  def -(other: Vertex): Vertex = Vertex(x - other.x, y - other.y, z - other.z)

  def *(s: Double): Vertex = Vertex(x * s, y * s, z * s)

  def /(s: Double): Vertex = Vertex(x / s, y / s, z / s)

  def length: Double = math.sqrt(dotSelf)

  def unit: Vertex = if (length > 0) this / length else this

  def negated: Vertex = Vertex(-x, -y, -z)

  def approxEqual(other: Vertex, epsilon: Double = Vertex.epsilon): Boolean = {
    math.abs(x - other.x) < epsilon && math.abs(y - other.y) < epsilon && math.abs(z - other.z) < epsilon
  }

  def min(right: Vertex): Vertex = Vertex(math.min(x, right.x), math.min(y, right.y), math.min(z, right.z))

  def max(right: Vertex): Vertex = Vertex(math.max(x, right.x), math.max(y, right.y), math.max(z, right.z))

  def collinear(a: Vertex, b: Vertex, epsilon: Double = Vertex.epsilon): Boolean =
    (b - a).cross(this - a).dotSelf < epsilon * epsilon

  // Find 't' such that 'a + t(b - a) = this'.
  // If this returns t in (0, 1), then this is between a and b.
  def solve(a: Vertex, b: Vertex): Double = {
    if ((a.x - b.x).abs > Vertex.epsilon) (x - a.x) / (b.x - a.x)
    else if ((a.y - b.y).abs > Vertex.epsilon) (y - a.y) / (b.y - a.y)
    else if ((a.z - b.z).abs > Vertex.epsilon) (z - a.z) / (b.z - a.z)
    else 0.0
  }

  // Returns true if this is strictly between a and b.
  def between(a: Vertex, b: Vertex, epsilon: Double = Vertex.epsilon): Boolean = {
    val t = solve(a, b)
    t >= epsilon && t <= 1 - epsilon && collinear(a, b, epsilon)
  }
}

object Vertex {
  val epsilon: Double = 1e-9

  val max: Vertex = Vertex(Double.MaxValue, Double.MaxValue, Double.MaxValue)
  val min: Vertex = Vertex(Double.MinValue, Double.MinValue, Double.MinValue)
}
