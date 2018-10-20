package net.joewing.scalacad

import scala.concurrent.{ExecutionContext, Future}

sealed trait BSPTree {
  def allPolygons: Seq[Polygon3d]
  def clipPolygons(ps: Seq[Polygon3d])(implicit ec: ExecutionContext): Future[Seq[Polygon3d]]
  def clip(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree]
  def inverted: BSPTree
  def insert(others: Seq[Polygon3d])(implicit ec: ExecutionContext): Future[BSPTree]
  final def merge(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree] = insert(other.allPolygons)
  def paint(p: Vertex, backfaces: Boolean)(f: Polygon3d => Unit): Unit
}

sealed trait BSPTreeLeaf extends BSPTree {
  final def allPolygons: Seq[Polygon3d] = Vector.empty
  final def clip(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree] = Future.successful(this)
  final def insert(others: Seq[Polygon3d])(implicit ec: ExecutionContext): Future[BSPTree] = BSPTree(others)
  final def paint(p: Vertex, backfaces: Boolean)(f: Polygon3d => Unit): Unit = ()
}

case object BSPTreeIn extends BSPTreeLeaf {
  def clipPolygons(ps: Seq[Polygon3d])(implicit ec: ExecutionContext): Future[Seq[Polygon3d]] = {
    Future.successful(ps)
  }
  def inverted: BSPTree = BSPTreeOut
}

case object BSPTreeOut extends BSPTreeLeaf {
  def clipPolygons(ps: Seq[Polygon3d])(implicit ec: ExecutionContext): Future[Seq[Polygon3d]] = {
    Future.successful(Vector.empty)
  }
  def inverted: BSPTree = BSPTreeIn
}

final case class BSPTreeNode(
  plane: Plane,
  polygons: Seq[Polygon3d],
  front: BSPTree,
  back: BSPTree
) extends BSPTree {

  def allPolygons: Seq[Polygon3d] = front.allPolygons ++ polygons ++ back.allPolygons

  // Clip facets to this BSPTree.
  def clipPolygons(ps: Seq[Polygon3d])(implicit ec: ExecutionContext): Future[Seq[Polygon3d]] = {
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

  def insert(others: Seq[Polygon3d])(implicit ec: ExecutionContext): Future[BSPTree] = {
    val result = plane.split(others)
    val newFrontFuture = if (result.front.nonEmpty) front.insert(result.front) else Future.successful(front)
    val newBackFuture = if (result.back.nonEmpty) back.insert(result.back) else Future.successful(back)
    for {
      newFront <- newFrontFuture
      newBack <- newBackFuture
    } yield BSPTreeNode(plane, polygons ++ (result.coFront ++ result.coBack), newFront, newBack)
  }

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

  private val emptyPlaneResult: Plane.SplitResult = Plane.SplitResult()

  private def findPartition(prev: Vertex, polygons: Seq[Polygon3d]): (Polygon3d, Plane.SplitResult) = {
    val n = polygons.size
    if (n > 1) {
      // Find the plane most orthogonal to prev.
      val current = polygons.minBy(p => math.abs(p.normal.dot(prev)))
      val plane = Plane(current)
      val result = plane.split(polygons.filter(_ != current))
      current -> result
    } else {
      polygons.head -> emptyPlaneResult
    }
  }

  private def helper(polygons: Seq[Polygon3d], prev: Vertex)(implicit ec: ExecutionContext): Future[BSPTree] = {
    if (polygons.isEmpty) {
      Future.successful(BSPTreeOut)
    } else {
      val (current, result) = findPartition(prev, polygons)
      val plane = Plane(current)
      val frontFuture = if (result.front.nonEmpty) helper(result.front, plane.normal) else Future.successful(BSPTreeIn)
      val backFuture = if (result.back.nonEmpty) helper(result.back, plane.normal) else Future.successful(BSPTreeOut)
      for {
        f <- frontFuture
        b <- backFuture
      } yield BSPTreeNode(plane, current +: (result.coFront ++ result.coBack), f, b)
    }
  }

  def apply(polygons: Seq[Polygon3d])(implicit ec: ExecutionContext): Future[BSPTree] = {
    helper(polygons, Vertex(1, 0, 0))
  }

}
