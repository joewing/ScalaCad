package net.joewing.scalacad

final case class RobustVertex(x: RobustFloat, y: RobustFloat, z: RobustFloat) {
  def cross(b: RobustVertex): RobustVertex = RobustVertex(
    y * b.z - z * b.y,
    z * b.x - x * b.z,
    x * b.y - y * b.x
  )

  def dot(b: RobustVertex): RobustFloat = x * b.x + y * b.y + z * b.z

  def dotSelf: RobustFloat = dot(this)

  def unit: RobustVertex = {
    val scale = 1.0 / dotSelf.sqrt
    RobustVertex(x * scale, y * scale, z * scale)
  }

  def toVertex: Vertex = Vertex(x.toDouble, y.toDouble, z.toDouble)

  def +(right: RobustVertex): RobustVertex = RobustVertex(x + right.x, y + right.y, z + right.z)
  def -(right: RobustVertex): RobustVertex = RobustVertex(x - right.x, y - right.y, z - right.z)
  def *(right: RobustFloat): RobustVertex = RobustVertex(x * right, y * right, z * right)
  def /(right: RobustFloat): RobustVertex = RobustVertex(x / right, y / right, z / right)

  def negated: RobustVertex = RobustVertex(-x, -y, -z)

  def min(right: RobustVertex): RobustVertex = RobustVertex(x.min(right.x), y.min(right.y), z.min(right.z))
  def max(right: RobustVertex): RobustVertex = RobustVertex(x.max(right.x), y.max(right.y), z.max(right.z))

  // Find 't' such that 'a + t(b - a) = this'.
  // If this returns t in (0, 1), then this is between a and b.
  def solve(a: RobustVertex, b: RobustVertex, epsilon: Double): RobustFloat = {
    if ((a.x - b.x).abs > epsilon) (x - a.x) / (b.x - a.x)
    else if ((a.y - b.y).abs > epsilon) (y - a.y) / (b.y - a.y)
    else if ((a.z - b.z).abs > epsilon) (z - a.z) / (b.z - a.z)
    else 0.0
  }

  def between(a: RobustVertex, b: RobustVertex, epsilon: Double = RobustVertex.epsilon): Boolean = {
    val t = solve(a, b, epsilon)
    t >= epsilon && t <= 1 - epsilon && (b - a).cross(this - a).dotSelf < epsilon * epsilon
  }
}

object RobustVertex {

  val epsilon: Double = 1e-9
  val min: RobustVertex = RobustVertex(RobustFloat.min, RobustFloat.min, RobustFloat.min)
  val max: RobustVertex = RobustVertex(RobustFloat.max, RobustFloat.max, RobustFloat.max)

  def apply(vertex: Vertex): RobustVertex = RobustVertex(
    RobustFloat(vertex.x),
    RobustFloat(vertex.y),
    RobustFloat(vertex.z)
  )
}


