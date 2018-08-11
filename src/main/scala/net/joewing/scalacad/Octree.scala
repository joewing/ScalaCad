package net.joewing.scalacad

sealed trait Octree {
  def vertices: Set[Vertex]
  def lowerBound: Vertex
  def upperBound: Vertex
  def midPoint: Vertex

  final def vertexIndex(v: Vertex): Int = {
    val xoffset = if (v.x1 < midPoint.x1) 0 else 1
    val yoffset = if (v.x2 < midPoint.x2) 0 else 2
    val zoffset = if (v.x3 < midPoint.x3) 0 else 4
    xoffset | yoffset | zoffset
  }

  /** Insert a vertex into this octree. */
  def insert(v: Vertex): Octree

  /** Get all vertices within the specified bounding box. */
  def contained(lower: Vertex, upper: Vertex): Seq[Vertex]
}

final case class OctreeNode(
  children: Vector[Octree],
  lowerBound: Vertex,
  upperBound: Vertex,
  midPoint: Vertex
) extends Octree {
  def vertices: Set[Vertex] = children.flatMap(_.vertices).toSet

  def insert(v: Vertex): Octree = {
    val i = vertexIndex(v)
    OctreeNode(
      children.updated(i, children(i).insert(v)),
      lowerBound.min(v),
      upperBound.max(v),
      midPoint
    )
  }

  def contained(lower: Vertex, upper: Vertex): Seq[Vertex] = {
    val checkXLower = lower.x1 < midPoint.x1 && upper.x1 >= lowerBound.x1
    val checkXUpper = lower.x1 <= upperBound.x1 && upper.x1 >= midPoint.x1
    val checkYLower = lower.x2 < midPoint.x2 && upper.x2 >= lowerBound.x2
    val checkYUpper = lower.x2 <= upperBound.x2 && upper.x2 >= midPoint.x2
    val checkZLower = lower.x3 < midPoint.x3 && upper.x3 >= lowerBound.x3
    val checkZUpper = lower.x3 <= upperBound.x3 && upper.x3 >= midPoint.x3

    (0 until 8).flatMap { i =>
      val checkx = if ((i & 1) == 0) checkXLower else checkXUpper
      val checky = if ((i & 2) == 0) checkYLower else checkYUpper
      val checkz = if ((i & 4) == 0) checkZLower else checkZUpper
      if (checkx && checky && checkz) {
        children(i).contained(lower, upper)
      } else {
        Seq.empty[Vertex]
      }
    }
  }
}

final case class OctreeLeaf(vertices: Set[Vertex]) extends Octree {
  lazy val lowerBound: Vertex = {
    if (vertices.isEmpty) {
      Vertex(Double.MaxValue, Double.MaxValue, Double.MaxValue)
    } else {
      vertices.tail.foldLeft(vertices.head)(_ min _)
    }
  }
  lazy val upperBound: Vertex = {
    if (vertices.isEmpty) {
      Vertex(Double.MinValue, Double.MinValue, Double.MinValue)
    } else {
      vertices.tail.foldLeft(vertices.head)(_ max _)
    }
  }
  lazy val midPoint: Vertex = (lowerBound + upperBound) / 2

  def split: Octree = {
    if (vertices.size > Octree.maxLeafSize) {
      val partition = vertices.groupBy(vertexIndex)
      val newLeaves = Vector.range(0, 8).map { i =>
        OctreeLeaf(partition.getOrElse(i, Set.empty))
      }
      OctreeNode(newLeaves, lowerBound, upperBound, midPoint)
    } else {
      this
    }
  }

  def insert(v: Vertex): Octree = OctreeLeaf(vertices = vertices + v).split

  def contained(lower: Vertex, upper: Vertex): Seq[Vertex] = {
    vertices.filter { v =>
      v.x1 >= lower.x1 && v.x2 >= lower.x2 && v.x3 >= lower.x3 &&
      v.x1 <= upper.x1 && v.x2 <= upper.x2 && v.x3 <= upper.x3
    }.toSeq
  }
}

object Octree {

  def maxLeafSize: Int = 64

  val empty: Octree = OctreeLeaf(Set.empty)

  def apply(vertices: Seq[Vertex]): Octree = {
    vertices.foldLeft(empty) { (t, v) => t.insert(v) }
  }
}

