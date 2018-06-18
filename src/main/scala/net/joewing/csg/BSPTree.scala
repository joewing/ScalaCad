package net.joewing.csg

final case class BSPTree(
  plane: Plane,
  facets: Seq[Facet],
  front: Option[BSPTree],
  back: Option[BSPTree]
) {

  def allFacets: Seq[Facet] = {
    val f: Seq[Facet] = front.map(_.allFacets).toSeq.flatten
    val b: Seq[Facet] = back.map(_.allFacets).toSeq.flatten
    facets ++ f ++ b
  }

  override def toString: String = allFacets.map(_.toString).mkString("\n")

  // Clip facets to this BSPTree.
  def clipFacets(fs: Seq[Facet]): Seq[Facet] = {
    val result = plane.split(fs)
    val frontFacets = result.front ++ result.coFront
    val backFacets = result.back ++ result.coBack
    val filteredFront = front match {
      case Some(f) => f.clipFacets(frontFacets)
      case None    => frontFacets
    }
    val filteredBack = back match {
      case Some(b) => b.clipFacets(backFacets)
      case None    => Seq.empty
    }
    filteredFront ++ filteredBack
  }

  // Return this BSPTree clipped to the other BSPTree.
  def clip(other: BSPTree): BSPTree = BSPTree(
    plane = plane,
    facets = other.clipFacets(facets),
    front = front.map(_.clip(other)),
    back = back.map(_.clip(other))
  )

  def inverted: BSPTree = BSPTree(
    plane = plane.flip,
    facets = facets.map(_.flip),
    front = back.map(_.inverted),
    back = front.map(_.inverted)
  )

  def insert(fs: Seq[Facet]): BSPTree = {
    if (fs.isEmpty) {
      this
    } else {
      val result = plane.split(fs)
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
      BSPTree(plane, facets ++ result.coFront ++ result.coBack, f, b)
    }
  }

  def merge(other: BSPTree): BSPTree = insert(other.allFacets)

  def simplified: BSPTree = {
    val i = clip(this).inverted
    i.clip(i).inverted
  }
}

object BSPTree {
  def apply(facets: Seq[Facet]): BSPTree = {
    require(facets.nonEmpty)
    val plane = Plane(facets.head)
    val result = plane.split(facets)
    val f = if (result.front.nonEmpty) Some(apply(result.front)) else None
    val b = if (result.back.nonEmpty) Some(apply(result.back)) else None
    BSPTree(plane, result.coFront ++ result.coBack, f, b)
  }
}
