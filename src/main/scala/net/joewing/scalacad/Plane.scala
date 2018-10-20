package net.joewing.scalacad

final case class Plane(normal: Vertex, w: Double) {

  def flip: Plane = Plane(normal.negated, -w)

  // This determines how "thick" the plane is.
  private val epsilon: Double = Vertex.epsilon
  private val fastMath: Boolean = true
  private val slowMath: Boolean = false

  def classify(v: Vertex): Int = {
    val t = normal.dot(v) - w
    if (t < -epsilon) Plane.Back
    else if (t > epsilon) Plane.Front
    else Plane.Coplanar
  }

  def splitPolygon(polygon: Polygon3d, result: Plane.SplitResult): Unit = {
    val types = polygon.vertices.map(classify)
    types.foldLeft(Plane.Coplanar)(_ | _) match {
      case pt if pt == Plane.Coplanar =>
        if (normal.dot(polygon.normal) > 0) {
          // Coplanar front
          result.coFront += polygon
        } else {
          // Coplanar back
          result.coBack += polygon
        }
      case pt if pt == Plane.Front    =>
        result.front += polygon
      case pt if pt == Plane.Back     =>
        result.back += polygon
      case _                          =>
        val fs = scala.collection.mutable.ArrayBuffer[Vertex]()
        val bs = scala.collection.mutable.ArrayBuffer[Vertex]()
        polygon.vertices.indices.foreach { i =>
          val j = (i + 1) % polygon.vertices.length
          val ti = types(i)
          val tj = types(j)
          val vi = polygon.vertices(i)
          val vj = polygon.vertices(j)
          if (ti != Plane.Back) fs += vi
          if (ti != Plane.Front) bs += vi
          if ((ti | tj) == Plane.Spanning) {
            val t = (w - normal.dot(vi)) / normal.dot(vj - vi)
            val v = vi.interpolate(vj, t)
            fs += v
            bs += v
          }
        }
        result.front += Polygon3d(fs)
        result.back += Polygon3d(bs)
    }
  }

  def split(polygons: Seq[Polygon3d], result: Plane.SplitResult): Unit = {
    polygons.foreach { polygon =>
      splitPolygon(polygon, result)
    }
  }

  def split(polygons: Seq[Polygon3d]): Plane.SplitResult = {
    val result = Plane.SplitResult()
    split(polygons, result)
    result
  }

}

object Plane {

  case class SplitResult(
    front: scala.collection.mutable.ArrayBuffer[Polygon3d] = scala.collection.mutable.ArrayBuffer(),
    back: scala.collection.mutable.ArrayBuffer[Polygon3d] = scala.collection.mutable.ArrayBuffer(),
    coFront: scala.collection.mutable.ArrayBuffer[Polygon3d] = scala.collection.mutable.ArrayBuffer(),
    coBack: scala.collection.mutable.ArrayBuffer[Polygon3d] = scala.collection.mutable.ArrayBuffer()
  )

  val Coplanar: Int = 0
  val Front: Int = 1
  val Back: Int = 2
  val Spanning: Int = 3

  def apply(polygon: Polygon3d): Plane = Plane(polygon.normal, polygon.normal.dot(polygon.vertices.head))

  def apply(a: Vertex, b: Vertex, c: Vertex): Plane = {
    val n = (b - a).cross(c - a).unit
    val w = -a.dot(n)
    Plane(n, w)
  }

  def apply(vertices: Seq[Vertex]): Plane = {
    val Seq(a, b, c) = vertices.take(3)
    apply(a, b, c)
  }

  def det3x3(
    a: Double, b: Double, c: Double,
    d: Double, e: Double, f: Double,
    g: Double, h: Double, i: Double
  ): Double = {
    a * e * i + b * f * g + c * d * h - c * e * g - b * d * i - a * f * h
  }

  def det4x4(
    x11: Double, x12: Double, x13: Double, x14: Double,
    x21: Double, x22: Double, x23: Double, x24: Double,
    x31: Double, x32: Double, x33: Double, x34: Double,
    x41: Double, x42: Double, x43: Double, x44: Double
  ): Double = {
    val m1 = x11 * det3x3(x22, x23, x24, x32, x33, x34, x42, x43, x44)
    val m2 = x12 * det3x3(x21, x23, x24, x31, x33, x34, x41, x43, x44)
    val m3 = x13 * det3x3(x21, x22, x24, x31, x32, x34, x41, x42, x44)
    val m4 = x14 * det3x3(x21, x22, x23, x31, x32, x33, x41, x42, x43)
    m1 - m2 + m3 - m4
  }
}
