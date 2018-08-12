package net.joewing.scalacad

sealed trait BSPTree {
  def allPolygons: Seq[Polygon]
  def clipPolygons(ps: Seq[Polygon]): Seq[Polygon]
  def clip(other: BSPTree): BSPTree
  def inverted: BSPTree
  def translated(v: Vertex): BSPTree
  def insert(others: Seq[Polygon]): BSPTree
  final def merge(other: BSPTree): BSPTree = insert(other.allPolygons)
  def paint(p: Vertex, backfaces: Boolean)(f: Polygon => Unit): Unit
}

trait BSPTreeLeaf extends BSPTree {
  final def allPolygons: Seq[Polygon] = Vector.empty
  final def clip(other: BSPTree): BSPTree = this
  final def translated(v: Vertex): BSPTree = this
  final def insert(others: Seq[Polygon]): BSPTree = BSPTree(others)
  final def paint(p: Vertex, backfaces: Boolean)(f: Polygon => Unit): Unit = ()
}

case object BSPTreeIn extends BSPTreeLeaf {
  def clipPolygons(ps: Seq[Polygon]): Seq[Polygon] = ps
  def inverted: BSPTree = BSPTreeOut
}

case object BSPTreeOut extends BSPTreeLeaf {
  def clipPolygons(ps: Seq[Polygon]): Seq[Polygon] = Vector.empty
  def inverted: BSPTree = BSPTreeIn
}

final case class BSPTreeNode(
  plane: Plane,
  polygons: Seq[Polygon],
  front: BSPTree,
  back: BSPTree
) extends BSPTree {

  def allPolygons: Seq[Polygon] = {
    polygons ++ front.allPolygons ++ back.allPolygons
  }

  // Clip facets to this BSPTree.
  def clipPolygons(ps: Seq[Polygon]): Seq[Polygon] = {
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

  def translated(v: Vertex): BSPTree = {
    copy(
      polygons = polygons.map(_.moved(v.x1, v.x2, v.x3)),
      front = front.translated(v),
      back = back.translated(v)
    )
  }

  def insert(others: Seq[Polygon]): BSPTree = {
    val result = plane.split(others)
    val newFront = if (result.front.nonEmpty) front.insert(result.front) else front
    val newBack = if (result.back.nonEmpty) back.insert(result.back) else back
    BSPTreeNode(plane, polygons ++ (result.coFront ++ result.coBack), newFront, newBack)
  }

  def paint(p: Vertex, backfaces: Boolean)(f: Polygon => Unit): Unit = {
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
  def helper(i: Int, polygons: Seq[Polygon]): BSPTree = {
    val (before, after) = polygons.splitAt(i)
    val others = before ++ after.tail
    val current = after.head
    val plane = Plane(current)
    val result = plane.split(others)
    val f = if (result.front.nonEmpty) apply(result.front) else BSPTreeIn
    val b = if (result.back.nonEmpty) apply(result.back) else BSPTreeOut
    BSPTreeNode(plane, current +: (result.coFront ++ result.coBack), f, b)
  }

  def apply(polygons: Seq[Polygon]): BSPTree = {
    if (polygons.isEmpty) {
      BSPTreeOut
    } else {
      helper(polygons.size / 2, polygons)
    }
  }
}
