package net.joewing.scalacad

sealed trait Octree {
  def vertices: Set[Vertex]
  def lowerBound: Vertex
  def upperBound: Vertex
  def midPoint: Vertex

  final def vertexIndex(v: Vertex): Int = {
    val xoffset = if (v.x < midPoint.x) 0 else 1
    val yoffset = if (v.y < midPoint.y) 0 else 2
    val zoffset = if (v.z < midPoint.z) 0 else 4
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
    val checkXLower = lower.x < midPoint.x && upper.x >= lowerBound.x
    val checkXUpper = lower.x <= upperBound.x && upper.x >= midPoint.x
    val checkYLower = lower.y < midPoint.y && upper.y >= lowerBound.y
    val checkYUpper = lower.y <= upperBound.y && upper.y >= midPoint.y
    val checkZLower = lower.z < midPoint.z && upper.z >= lowerBound.z
    val checkZUpper = lower.z <= upperBound.z && upper.z >= midPoint.z

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
      Vertex.max
    } else {
      vertices.tail.foldLeft(vertices.head)(_ min _)
    }
  }
  lazy val upperBound: Vertex = {
    if (vertices.isEmpty) {
      Vertex.min
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
      v.x >= lower.x && v.y >= lower.y && v.z >= lower.z &&
      v.x <= upper.x && v.y <= upper.y && v.z <= upper.z
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

