package net.joewing.scalacad

final class Polygon3d(val vertices: Array[Vertex]) {

  def normal: Vertex = {
    val a = vertices.head
    val b = vertices(1)
    val c = vertices(2)
    (b - a).cross(c - a).unit
  }

  def minBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ min _)
  def maxBound: Vertex = vertices.tail.foldLeft(vertices.head)(_ max _)

  def edges: Seq[(Vertex, Vertex)] = vertices.zip(vertices.tail :+ vertices.head)

  def flip: Polygon3d = Polygon3d(vertices.reverse)

  def scaled(x: Double = 1, y: Double = 1, z: Double = 1): Polygon3d = {
    val newPolygon = Polygon3d(vertices.map(_.scaled(x, y, z)))
    if (x * y * z < 0) newPolygon.flip else newPolygon
  }

  def rotated(x: Double = 0, y: Double = 0, z: Double = 0): Polygon3d = Polygon3d(
    vertices.map(_.rotated(x, y, z))
  )

  def moved(x: Double = 0, y: Double = 0, z: Double = 0): Polygon3d = Polygon3d(
    vertices.map(_.moved(x, y, z))
  )

  override def equals(obj: Any): Boolean = obj match {
    case p: Polygon3d if vertices.length == p.vertices.length => vertices.sameElements(p.vertices)
    case _ => false
  }
}

object Polygon3d {
  def apply(vs: TraversableOnce[Vertex]): Polygon3d = new Polygon3d(vs.toArray)
  def apply(vs: Array[Vertex]): Polygon3d = new Polygon3d(vs)
}
