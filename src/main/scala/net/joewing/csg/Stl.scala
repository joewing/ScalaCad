package net.joewing.csg

case class Stl(name: String, facets: Seq[Facet]) {
  def rotated(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Stl = copy(
    facets = facets.map(_.rotated(x, y, z))
  )

  lazy val minBound: Vertex = {
    val bounds = facets.map(_.minBound)
    bounds.tail.foldLeft(bounds.head) { (mb, current) =>
      Vertex(
        math.min(mb.x1, current.x1),
        math.min(mb.x2, current.x2),
        math.min(mb.x3, current.x3),
      )
    }
  }

  lazy val maxBound: Vertex = {
    val bounds = facets.map(_.maxBound)
    bounds.tail.foldLeft(bounds.head) { (mb, current) =>
      Vertex(
        math.max(mb.x1, current.x1),
        math.max(mb.x2, current.x2),
        math.max(mb.x3, current.x3),
      )
    }
  }
}
