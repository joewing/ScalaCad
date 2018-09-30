package net.joewing.scalacad

sealed trait BSPTree {
  def allPolygons: Seq[PlanePolygon]
  def clipPolygons(ps: Seq[PlanePolygon]): Seq[PlanePolygon]
  def clip(other: BSPTree): BSPTree
  def inverted: BSPTree
  def insert(others: Seq[PlanePolygon]): BSPTree
  def reduce: BSPTree
  final def merge(other: BSPTree): BSPTree = insert(other.allPolygons)
  def paint(p: Vertex, backfaces: Boolean)(f: PlanePolygon => Unit): Unit
}

trait BSPTreeLeaf extends BSPTree {
  final def allPolygons: Seq[PlanePolygon] = Vector.empty
  final def clip(other: BSPTree): BSPTree = this
  final def insert(others: Seq[PlanePolygon]): BSPTree = BSPTree(others)
  final def reduce: BSPTree = this
  final def paint(p: Vertex, backfaces: Boolean)(f: PlanePolygon => Unit): Unit = ()
}

case object BSPTreeIn extends BSPTreeLeaf {
  def clipPolygons(ps: Seq[PlanePolygon]): Seq[PlanePolygon] = ps
  def inverted: BSPTree = BSPTreeOut
}

case object BSPTreeOut extends BSPTreeLeaf {
  def clipPolygons(ps: Seq[PlanePolygon]): Seq[PlanePolygon] = Vector.empty
  def inverted: BSPTree = BSPTreeIn
}

final case class BSPTreeNode(
  plane: Plane,
  polygons: Seq[PlanePolygon],
  front: BSPTree,
  back: BSPTree
) extends BSPTree {

  def allPolygons: Seq[PlanePolygon] = {
    polygons ++ front.allPolygons ++ back.allPolygons
  }

  // Clip facets to this BSPTree.
  def clipPolygons(ps: Seq[PlanePolygon]): Seq[PlanePolygon] = {
    val result = plane.split(ps)
    val frontPolygons = result.front ++ result.coFront
    val filteredFront = front.clipPolygons(frontPolygons)
    back.clipPolygons(result.back ++ result.coBack) ++ filteredFront
  }

  // Return this BSPTree clipped to the other BSPTree.
  def clip(other: BSPTree): BSPTree = {
    BSPTreeNode(
      plane = plane,
      polygons = other.clipPolygons(polygons),
      front = front.clip(other),
      back = back.clip(other)
    )
  }

  def inverted: BSPTree = BSPTreeNode(
    plane = plane.flip,
    polygons = polygons.map(_.flip),
    front = back.inverted,
    back = front.inverted
  )

  def insert(others: Seq[PlanePolygon]): BSPTree = {
    val result = plane.split(others)
    val newFront = if (result.front.nonEmpty) front.insert(result.front) else front
    val newBack = if (result.back.nonEmpty) back.insert(result.back) else back
    BSPTreeNode(plane, polygons ++ (result.coFront ++ result.coBack), newFront, newBack)
  }

  def reduce: BSPTree = {
    (front, back) match {
      case (BSPTreeIn, BSPTreeIn)   => BSPTreeIn
      case (BSPTreeOut, BSPTreeOut) => BSPTreeOut
      case _                        =>
        if (polygons.isEmpty) {
          if (front.isInstanceOf[BSPTreeLeaf]) {
            back.reduce
          } else if (back.isInstanceOf[BSPTreeLeaf]) {
            front.reduce
          } else {
            copy(front = front.reduce, back = back.reduce)
          }
        } else copy(front = front.reduce, back = back.reduce)
    }
  }

  def paint(p: Vertex, backfaces: Boolean)(f: PlanePolygon => Unit): Unit = {
    val dp = p.dot(plane.normal)
    if (dp > 0) {
      back.paint(p, backfaces)(f)
      polygons.foreach(f)
      front.paint(p, backfaces)(f)
    } else {
      front.paint(p, backfaces)(f)
      if (backfaces) {
        polygons.foreach(f)
      }
      back.paint(p, backfaces)(f)
    }
  }
}

object BSPTree {
  def helper(i: Int, polygons: Seq[PlanePolygon]): BSPTree = {
    val (before, after) = polygons.splitAt(i)
    val others = before ++ after.tail
    val current = after.head
    val plane = current.planes.head
    val result = plane.split(others)
    val f = if (result.front.nonEmpty) apply(result.front) else BSPTreeIn
    val b = if (result.back.nonEmpty) apply(result.back) else BSPTreeOut
    BSPTreeNode(plane, current +: (result.coFront ++ result.coBack), f, b)
  }

  def apply(polygons: Seq[PlanePolygon]): BSPTree = {
    if (polygons.isEmpty) {
      BSPTreeOut
    } else {
      helper(polygons.size / 2, polygons)
    }
  }
}
