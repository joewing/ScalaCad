package net.joewing.scalacad

final case class Plane(normal: Vertex, w: Double) {
  def flip: Plane = Plane(normal.negated, -w)

  // This determines how "thick" the plane is.
  private val epsilon: Double = Vertex.epsilon

  def classify(v: Vertex): Int = {
    val t = normal.dot(v) - w
    if (t < -epsilon) Plane.Back
    else if (t > epsilon) Plane.Front
    else Plane.Coplanar
  }

  def splitPolygon(polygon: Polygon, result: Plane.SplitResult): Unit = {
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
        result.front += Polygon(fs)
        result.back += Polygon(bs)
    }
  }

  def split(polygons: Seq[Polygon]): Plane.SplitResult = {
    val result = Plane.SplitResult()
    polygons.foreach { polygon =>
      splitPolygon(polygon, result)
    }
    result
  }
}

object Plane {

  // These are mutable to improve performance during construction.
  case class SplitResult(
    front: scala.collection.mutable.ArrayBuffer[Polygon] = scala.collection.mutable.ArrayBuffer(),
    back: scala.collection.mutable.ArrayBuffer[Polygon] = scala.collection.mutable.ArrayBuffer(),
    coFront: scala.collection.mutable.ArrayBuffer[Polygon] = scala.collection.mutable.ArrayBuffer(),
    coBack: scala.collection.mutable.ArrayBuffer[Polygon] = scala.collection.mutable.ArrayBuffer()
  )

  val Coplanar: Int = 0
  val Front: Int = 1
  val Back: Int = 2
  val Spanning: Int = 3

  def apply(polygon: Polygon): Plane = Plane(polygon.normal, polygon.normal.dot(polygon.vertices.head))
}
