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

  def splitPolygon(polygon: Polygon3d, result: Plane.SplitResult): Unit = {
    val len = polygon.vertices.length
    val types = Array.ofDim[Int](len)
    var hasFront = false
    var hasBack = false
    var i = 0
    while (i < len) {
      types(i) = classify(polygon.vertices(i))
      hasFront = hasFront || (types(i) == Plane.Front)
      hasBack = hasBack || (types(i) == Plane.Back)
      i += 1
    }
    if (!hasFront && !hasBack) {
      if (normal.dot(polygon.normal) > 0) {
        result.coFront += polygon
      } else {
        result.coBack += polygon
      }
    } else if (hasFront && !hasBack) {
      result.front += polygon
    } else if (hasBack && !hasFront) {
      result.back += polygon
    } else {
      val fs = scala.collection.mutable.ArrayBuffer.empty[Vertex]
      val bs = scala.collection.mutable.ArrayBuffer.empty[Vertex]
      i = 0
      while (i < len) {
        val j = (i + 1) % len
        val ti = types(i)
        val tj = types(j)
        val vi = polygon.vertices(i)
        val vj = polygon.vertices(j)
        if (ti != Plane.Back) fs += vi
        if (ti != Plane.Front) bs += vi
        if ((ti | tj) == Plane.Spanning) {
          val diff = vj - vi
          val t = (w - normal.dot(vi)) / normal.dot(diff)
          val v = vi + diff * t
          fs += v
          bs += v
        }
        i += 1
      }
      result.front += Polygon3d(fs)
      result.back += Polygon3d(bs)
    }
  }

  def split(polygons: IndexedSeq[Polygon3d]): Plane.SplitResult = {
    val result = new Plane.SplitResult()
    var i = 0
    val len = polygons.length
    while (i < len) {
      splitPolygon(polygons(i), result)
      i += 1
    }
    result
  }

}

object Plane {

  final class SplitResult {
    val front: scala.collection.mutable.ArrayBuffer[Polygon3d] = scala.collection.mutable.ArrayBuffer.empty
    val back: scala.collection.mutable.ArrayBuffer[Polygon3d] = scala.collection.mutable.ArrayBuffer.empty
    val coFront: scala.collection.mutable.ArrayBuffer[Polygon3d] = scala.collection.mutable.ArrayBuffer.empty
    val coBack: scala.collection.mutable.ArrayBuffer[Polygon3d] = scala.collection.mutable.ArrayBuffer.empty
  }

  val Coplanar: Int = 0
  val Front: Int = 1
  val Back: Int = 2
  val Spanning: Int = 3

  def apply(polygon: Polygon3d): Plane = Plane(polygon.normal, polygon.normal.dot(polygon.vertices.head))
}
