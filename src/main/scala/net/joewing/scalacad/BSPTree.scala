package net.joewing.scalacad

final case class BSPTree(
  plane: Plane,
  polygons: Seq[Polygon],
  front: Option[BSPTree],
  back: Option[BSPTree]
) {

  def allPolygons: Seq[Polygon] = {
    val f: Seq[Polygon] = front.map(_.allPolygons).toSeq.flatten
    val b: Seq[Polygon] = back.map(_.allPolygons).toSeq.flatten
    polygons ++ f ++ b
  }

  override def toString: String = allPolygons.map(_.toString).mkString("\n")

  // Clip facets to this BSPTree.
  def clipPolygons(ps: Seq[Polygon]): Seq[Polygon] = {
    val result = plane.split(ps)
    val frontPolygons = result.front ++ result.coFront
    val filteredFront = front match {
      case Some(f) => f.clipPolygons(frontPolygons)
      case None    => frontPolygons
    }
    back match {
      case Some(b) => b.clipPolygons(result.back ++ result.coBack) ++ filteredFront
      case None    => filteredFront
    }
  }

  // Return this BSPTree clipped to the other BSPTree.
  def clip(other: BSPTree): BSPTree = BSPTree(
    plane = plane,
    polygons = other.clipPolygons(polygons),
    front = front.map(_.clip(other)),
    back = back.map(_.clip(other))
  )

  def inverted: BSPTree = BSPTree(
    plane = plane.flip,
    polygons = polygons.map(_.flip),
    front = back.map(_.inverted),
    back = front.map(_.inverted)
  )

  def insert(ps: Seq[Polygon]): BSPTree = {
    if (ps.isEmpty) {
      this
    } else {
      val result = plane.split(ps)
      val f: Option[BSPTree] = front match {
        case Some(x)                       => Some(x.insert(result.front))
        case None if result.front.nonEmpty => Some(BSPTree.apply(result.front))
        case None                          => None
      }
      val b: Option[BSPTree] = back match {
        case Some(x)                       => Some(x.insert(result.back))
        case None if result.back.nonEmpty  => Some(BSPTree.apply(result.back))
        case None                          => None
      }
      BSPTree(plane, polygons ++ result.coFront ++ result.coBack, f, b)
    }
  }

  def merge(other: BSPTree): BSPTree = insert(other.allPolygons)

  def depth: Int = {
    val frontDepth = front.map(_.depth).getOrElse(0)
    val backDepth = back.map(_.depth).getOrElse(0)
    math.max(frontDepth, backDepth) + 1
  }
}

object BSPTree {
  def apply(polygons: Seq[Polygon]): BSPTree = {
    val plane = Plane(polygons.head)
    val result = plane.split(polygons.tail)
    val f = if (result.front.nonEmpty) Some(apply(result.front)) else None
    val b = if (result.back.nonEmpty) Some(apply(result.back)) else None
    BSPTree(plane, polygons.head +: (result.coFront ++ result.coBack), f, b)
  }
}
