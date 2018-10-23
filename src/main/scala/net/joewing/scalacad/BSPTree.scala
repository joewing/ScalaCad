package net.joewing.scalacad

import scala.concurrent.{ExecutionContext, Future}

sealed trait BSPTree {
  def allPolygons: IndexedSeq[Polygon3d]
  def clipPolygons(ps: IndexedSeq[Polygon3d])(implicit ec: ExecutionContext): Future[IndexedSeq[Polygon3d]]
  def clip(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree]
  def inverted: BSPTree
  final def merge(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree] = {
    val leftPolygonsFuture = Future(allPolygons)
    val rightPolygonsFuture = Future(other.allPolygons)
    for {
      left <- leftPolygonsFuture
      right <- rightPolygonsFuture
      tree <- BSPTree(left ++ right)
    } yield tree
  }
  def paint(p: Vertex, backfaces: Boolean)(f: Polygon3d => Unit): Unit
}

sealed trait BSPTreeLeaf extends BSPTree {
  final def allPolygons: IndexedSeq[Polygon3d] = Vector.empty
  final def clip(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree] = Future.successful(this)
  final def paint(p: Vertex, backfaces: Boolean)(f: Polygon3d => Unit): Unit = ()
}

case object BSPTreeIn extends BSPTreeLeaf {
  def clipPolygons(ps: IndexedSeq[Polygon3d])(implicit ec: ExecutionContext): Future[IndexedSeq[Polygon3d]] = {
    Future.successful(ps)
  }
  def inverted: BSPTree = BSPTreeOut
}

case object BSPTreeOut extends BSPTreeLeaf {
  def clipPolygons(ps: IndexedSeq[Polygon3d])(implicit ec: ExecutionContext): Future[IndexedSeq[Polygon3d]] = {
    Future.successful(Vector.empty)
  }
  def inverted: BSPTree = BSPTreeIn
}

final case class BSPTreeNode(
  plane: Plane,
  polygons: IndexedSeq[Polygon3d],
  front: BSPTree,
  back: BSPTree
) extends BSPTree {

  def allPolygons: IndexedSeq[Polygon3d] = front.allPolygons ++ polygons ++ back.allPolygons

  // Clip facets to this BSPTree.
  def clipPolygons(ps: IndexedSeq[Polygon3d])(implicit ec: ExecutionContext): Future[IndexedSeq[Polygon3d]] = {
    val result = plane.split(ps)
    val frontPolygons = result.front ++ result.coFront
    val filteredFrontFuture = front.clipPolygons(frontPolygons)
    val filteredBackFuture = back.clipPolygons(result.back ++ result.coBack)
    for {
      filteredFront <- filteredFrontFuture
      filteredBack <- filteredBackFuture
    } yield filteredBack ++ filteredFront
  }

  // Return this BSPTree clipped to the other BSPTree.
  def clip(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree] = {
    val frontFuture = front.clip(other)
    val backFuture = back.clip(other)
    val polygonsFuture = other.clipPolygons(polygons)
    for {
      newFront <- frontFuture
      newBack <- backFuture
      newPolygons <- polygonsFuture
    } yield BSPTreeNode(
      plane = plane,
      polygons = newPolygons,
      front = newFront,
      back = newBack
    )
  }

  def inverted: BSPTree = BSPTreeNode(
    plane = plane.flip,
    polygons = polygons.map(_.flip),
    front = back.inverted,
    back = front.inverted
  )

  def paint(p: Vertex, backfaces: Boolean)(f: Polygon3d => Unit): Unit = {
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

  private def helper(polygons: IndexedSeq[Polygon3d], prev: Vertex)(implicit ec: ExecutionContext): Future[BSPTree] = {
    if (polygons.isEmpty) {
      Future.successful(BSPTreeOut)
    } else {
      // Find the plane most orthogonal to prev.
      val current = polygons.minBy(p => math.abs(p.normal.dot(prev)))
      val plane = Plane(current)
      val result = plane.split(polygons.filter(_ != current))

      val frontFuture = if (result.front.nonEmpty) helper(result.front, plane.normal) else Future.successful(BSPTreeIn)
      val backFuture = if (result.back.nonEmpty) helper(result.back, plane.normal) else Future.successful(BSPTreeOut)
      for {
        f <- frontFuture
        b <- backFuture
      } yield BSPTreeNode(plane, current +: (result.coFront ++ result.coBack), f, b)
    }
  }

  def apply(polygons: IndexedSeq[Polygon3d])(implicit ec: ExecutionContext): Future[BSPTree] = {
    helper(polygons, Vertex(1, 0, 0))
  }

}
