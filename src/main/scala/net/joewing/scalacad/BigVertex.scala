package net.joewing.scalacad

final case class BigVertex(x: BigDecimal, y: BigDecimal, z: BigDecimal) {
  def cross(b: BigVertex): BigVertex = BigVertex(
    y * b.z - z * b.y,
    z * b.x - x * b.z,
    x * b.y - y * b.x
  )

  def dot(b: BigVertex): BigDecimal = x * b.x + y * b.y + z * b.z

  def dotSelf: BigDecimal = dot(this)

  def toVertex: Vertex = Vertex(x.toDouble, y.toDouble, z.toDouble)

  def +(right: BigVertex): BigVertex = BigVertex(x + right.x, y + right.y, z + right.z)
  def -(right: BigVertex): BigVertex = BigVertex(x - right.x, y - right.y, z - right.z)
  def *(right: BigDecimal): BigVertex = BigVertex(x * right, y * right, z * right)
  def /(right: BigDecimal): BigVertex = BigVertex(x / right, y / right, z / right)
}

object BigVertex {
  def apply(v: Vertex): BigVertex = BigVertex(v.x, v.y, v.z)
}
