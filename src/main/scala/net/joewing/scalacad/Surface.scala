package net.joewing.scalacad

sealed trait Surface {
  def vertices: Seq[Vertex]

  def facetSurface: FacetSurface
  def bspSurface: BSPTreeSurface

  def facets: Seq[Facet]
  def tree: BSPTree

  def invert: Surface

  def translate(v: Vertex): Surface

  def scale(x: Double = 1, y: Double = 1, z: Double = 1): Surface

  def map(f: Facet => Facet): FacetSurface = Surface.fromFacets(facets.map(f))

  def filter(f: Facet => Boolean): FacetSurface = Surface.fromFacets(facets.filter(f))

  def filterNot(f: Facet => Boolean): FacetSurface = Surface.fromFacets(facets.filterNot(f))

  def insert(facet: Facet): Surface = facetSurface.copy(facets = facet +: facets)
}

final case class FacetSurface(facets: Seq[Facet]) extends Surface {

  lazy val vertices: Seq[Vertex] = facets.flatMap(_.vertices).distinct

  def tree: BSPTree = bspSurface.tree

  def invert: Surface = FacetSurface(facets.map(_.flip))

  def translate(v: Vertex): Surface = FacetSurface(facets.map(_.moved(v.x, v.y, v.z)))

  def scale(x: Double = 1, y: Double = 1, z: Double = 1): Surface = FacetSurface(
    facets.map(_.scaled(x, y, z))
  )

  def facetSurface: FacetSurface = this
  def bspSurface: BSPTreeSurface = BSPTreeSurface(BSPTree(Facet.toPolygons(facets)))
}

final case class BSPTreeSurface(tree: BSPTree) extends Surface {
  lazy val vertices: Seq[Vertex] = tree.allPolygons.flatMap(_.vertices)

  def facets: Seq[Facet] = facetSurface.facets

  def invert: Surface = BSPTreeSurface(tree.inverted)

  def translate(v: Vertex): Surface = BSPTreeSurface(tree.translated(v))

  def scale(x: Double = 1, y: Double = 1, z: Double = 1): Surface = facetSurface.scale(x, y, z)

  def facetSurface: FacetSurface = Surface.treeToFacets(tree)
  def bspSurface: BSPTreeSurface = this
}

object Surface {

  val empty: Surface = FacetSurface(Seq.empty)

  def fromFacets(facets: Seq[Facet]): FacetSurface = FacetSurface(facets)

  def fromVertices(vertices: Seq[Vertex]): FacetSurface = fromFacets(Facet.fromVertices(vertices))

  // Insert a point to the facet by splitting it.
  // Note that this assumes all inserted points are on an edge of the facet.
  def insertPoint(facet: Facet, p: Vertex): Seq[Facet] = {
    if (p.between(facet.v1, facet.v2) && p.collinear(facet.v1, facet.v2)) {
      Vector(Facet(facet.v1, p, facet.v3), Facet(p, facet.v2, facet.v3))
    } else if (p.between(facet.v2, facet.v3) && p.collinear(facet.v2, facet.v3)) {
      Vector(Facet(facet.v1, facet.v2, p), Facet(p, facet.v3, facet.v1))
    } else if (p.between(facet.v3, facet.v1) && p.collinear(facet.v3, facet.v1)) {
      Vector(Facet(facet.v1, facet.v2, p), Facet(facet.v2, facet.v3, p))
    } else {
      Vector(facet)
    }
  }

  // Insert points from the Octree into the facet by splitting it.
  def insertPoints(facet: Facet, octree: Octree): Seq[Facet] = {
    val d = Vertex(Vertex.epsilon, Vertex.epsilon, Vertex.epsilon)
    octree.contained(facet.minBound - d, facet.maxBound + d).foldLeft(Vector(facet)) { (oldFacets, point) =>
      oldFacets.flatMap { f => insertPoint(f, point) }
    }
  }

  // Check if a facet is valid (has non-zero area).
  def validFacet(f: Facet): Boolean = {
    !f.v1.approxEqual(f.v2) && !f.v1.approxEqual(f.v3) && !f.v2.approxEqual(f.v3)
  }

  def treeToFacets(root: BSPTree): FacetSurface = {
    val polygons = root.allPolygons
    val vertices = polygons.flatMap(_.vertices).distinct
    val octree = Octree(vertices)
    val facets = polygons.par.flatMap { p =>
      Facet.fromVertices(p.vertices).flatMap(f => insertPoints(f, octree).filter(validFacet))
    }.seq
    FacetSurface(facets)
  }
}
