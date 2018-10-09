package net.joewing.scalacad

import scala.concurrent.{ExecutionContext, Future}

sealed trait BSPTree {
  def allPolygons: Seq[PlanePolygon]
  def clipPolygons(ps: Seq[PlanePolygon])(implicit ec: ExecutionContext): Future[Seq[PlanePolygon]]
  def clip(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree]
  def inverted: BSPTree
  def insert(others: Seq[PlanePolygon])(implicit ec: ExecutionContext): Future[BSPTree]
  def reduce: BSPTree
  final def merge(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree] = insert(other.allPolygons)
  def paint(p: Vertex, backfaces: Boolean)(f: PlanePolygon => Unit): Unit
}

trait BSPTreeLeaf extends BSPTree {
  final def allPolygons: Seq[PlanePolygon] = Vector.empty
  final def clip(other: BSPTree)(implicit ec: ExecutionContext): Future[BSPTree] = Future.successful(this)
  final def insert(others: Seq[PlanePolygon])(implicit ec: ExecutionContext): Future[BSPTree] = BSPTree(others)
  final def reduce: BSPTree = this
  final def paint(p: Vertex, backfaces: Boolean)(f: PlanePolygon => Unit): Unit = ()
}

case object BSPTreeIn extends BSPTreeLeaf {
  def clipPolygons(ps: Seq[PlanePolygon])(implicit ec: ExecutionContext): Future[Seq[PlanePolygon]] = {
    Future.successful(ps)
  }
  def inverted: BSPTree = BSPTreeOut
}

case object BSPTreeOut extends BSPTreeLeaf {
  def clipPolygons(ps: Seq[PlanePolygon])(implicit ec: ExecutionContext): Future[Seq[PlanePolygon]] = {
    Future.successful(Vector.empty)
  }
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
  def clipPolygons(ps: Seq[PlanePolygon])(implicit ec: ExecutionContext): Future[Seq[PlanePolygon]] = {
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

  def insert(others: Seq[PlanePolygon])(implicit ec: ExecutionContext): Future[BSPTree] = {
    val result = plane.split(others)
    val newFrontFuture = if (result.front.nonEmpty) front.insert(result.front) else Future.successful(front)
    val newBackFuture = if (result.back.nonEmpty) back.insert(result.back) else Future.successful(back)
    for {
      newFront <- newFrontFuture
      newBack <- newBackFuture
    } yield BSPTreeNode(plane, polygons ++ (result.coFront ++ result.coBack), newFront, newBack)
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

  /** Log base 2 of an integer. */
  private def lg(n: Int): Int = Integer.SIZE - Integer.numberOfLeadingZeros(n)

  private val emptyPlaneResult: Plane.PlaneSplitResult = Plane.PlaneSplitResult()

  private def findPartition(polygons: Seq[PlanePolygon]): (PlanePolygon, Plane.PlaneSplitResult) = {
    val n = polygons.size
    if (n > 1) {
      val maxIter = lg(n)
      Vector.range(0, maxIter).par.map { i =>
        val part = i * maxIter / n
        val (before, after) = polygons.splitAt(part)
        val plane = polygons(part).planes.head
        val result = plane.split(before)
        plane.split(after.tail, result)
        after.head -> result
      }.minBy(x => x._2.back.size + x._2.front.size)
    } else {
      polygons.head -> emptyPlaneResult
    }
  }

  def apply(polygons: Seq[PlanePolygon])(implicit ec: ExecutionContext): Future[BSPTree] = {
    if (polygons.isEmpty) {
      Future.successful(BSPTreeOut)
    } else {
      val (current, result) = findPartition(polygons)
      val plane = current.planes.head
      val frontFuture = if (result.front.nonEmpty) apply(result.front) else Future.successful(BSPTreeIn)
      val backFuture = if (result.back.nonEmpty) apply(result.back) else Future.successful(BSPTreeOut)
      for {
        f <- frontFuture
        b <- backFuture
      } yield BSPTreeNode(plane, current +: (result.coFront ++ result.coBack), f, b)
    }
  }
}
