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

  def triangulate(vertices: Seq[Vertex]): Seq[Facet] = {
    vertices.tail.sliding(2).collect { case Seq(a, b) =>
      Facet(vertices.head, a, b)
    }.toList
  }

  def splitFacet(facet: Facet): Plane.SplitResult = {
    val types = facet.vertices.map(classify)
    types.foldLeft(Plane.Coplanar)(_ | _) match {
      case pt if pt == Plane.Coplanar =>
        if (normal.dot(facet.normal) > 0) {
          // Coplanar front
          Plane.SplitResult(coFront = Seq(facet))
        } else {
          // Coplanar back
          Plane.SplitResult(coBack = Seq(facet))
        }
      case pt if pt == Plane.Front    =>
        Plane.SplitResult(front = Seq(facet))
      case pt if pt == Plane.Back     =>
        Plane.SplitResult(back = Seq(facet))
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
        Plane.SplitResult(front = triangulate(fs), back = triangulate(bs))
    }
  }

  def split(facets: Seq[Facet]): Plane.SplitResult = {
    val empty = Plane.SplitResult()
    facets.foldLeft(empty) { case (acc, facet) =>
      val result = splitFacet(facet)
      Plane.SplitResult(
        front = acc.front ++ result.front,
        back = acc.back ++ result.back,
        coFront = acc.coFront ++ result.coFront,
        coBack = acc.coBack ++ result.coBack
      )
    }
  }
}

object Plane {

  case class SplitResult(
    front: Seq[Facet] = Seq.empty,
    back: Seq[Facet] = Seq.empty,
    coFront: Seq[Facet] = Seq.empty,
    coBack: Seq[Facet] = Seq.empty
  )

  val Coplanar: Int = 0
  val Front: Int = 1
  val Back: Int = 2
  val Spanning: Int = 3

  def apply(facet: Facet): Plane = Plane(facet.normal, facet.normal.dot(facet.v1))
}
