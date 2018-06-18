package net.joewing.csg

final case class Plane(normal: Vertex, w: Double) {
  def flip: Plane = Plane(normal.negated, -w)

  private val epsilon: Double = 1e-5

  def classify(v: Vertex): Int = {
    val t = normal.dot(v) - w
    if (t < -epsilon) Plane.Back
    else if (t > epsilon) Plane.Front
    else Plane.Coplanar
  }

  private def triangulate(vertices: Seq[Vertex], result: scala.collection.mutable.ArrayBuffer[Facet]): Unit = {
    vertices.tail.sliding(2).foreach { case Seq(a, b) =>
      result += Facet(vertices.head, a, b)
    }
  }

  def splitFacet(facet: Facet, result: Plane.SplitResult): Unit = {
    val types = facet.vertices.map(classify)
    types.foldLeft(Plane.Coplanar)(_ | _) match {
      case pt if pt == Plane.Coplanar =>
        if (normal.dot(facet.normal) > 0) {
          // Coplanar front
          result.coFront += facet
        } else {
          // Coplanar back
          result.coBack += facet
        }
      case pt if pt == Plane.Front    =>
        result.front += facet
      case pt if pt == Plane.Back     =>
        result.back += facet
      case _                          =>
        val fs = scala.collection.mutable.ArrayBuffer[Vertex]()
        val bs = scala.collection.mutable.ArrayBuffer[Vertex]()
        facet.vertices.indices.foreach { i =>
          val j = (i + 1) % facet.vertices.length
          val ti = types(i)
          val tj = types(j)
          val vi = facet.vertices(i)
          val vj = facet.vertices(j)
          if (ti != Plane.Back) fs.append(vi)
          if (ti != Plane.Front) bs.append(vi)
          if ((ti | tj) == Plane.Spanning) {
            val t = (w - normal.dot(vi)) / normal.dot(vj - vi)
            val v = vi.interpolate(vj, t)
            fs.append(v)
            bs.append(v)
          }
        }
        triangulate(fs, result.front)
        triangulate(bs, result.back)
    }
  }

  def split(facets: Seq[Facet]): Plane.SplitResult = {
    val result = Plane.SplitResult()
    facets.foreach { facet =>
      splitFacet(facet, result)
    }
    result
  }
}

object Plane {

  // These are mutable to improve performance during construction.
  case class SplitResult(
    front: scala.collection.mutable.ArrayBuffer[Facet] = scala.collection.mutable.ArrayBuffer(),
    back: scala.collection.mutable.ArrayBuffer[Facet] = scala.collection.mutable.ArrayBuffer(),
    coFront: scala.collection.mutable.ArrayBuffer[Facet] = scala.collection.mutable.ArrayBuffer(),
    coBack: scala.collection.mutable.ArrayBuffer[Facet] = scala.collection.mutable.ArrayBuffer()
  )

  val Coplanar: Int = 0
  val Front: Int = 1
  val Back: Int = 2
  val Spanning: Int = 3

  def apply(facet: Facet): Plane = Plane(facet.normal, facet.normal.dot(facet.v1))
}
