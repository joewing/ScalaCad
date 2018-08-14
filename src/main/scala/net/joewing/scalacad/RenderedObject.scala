package net.joewing.scalacad

sealed trait RenderedObject {
  def vertices: Seq[Vertex]

  def facets: Seq[Facet]
  def tree: BSPTree

  def invert: RenderedObject

  def translate(v: Vertex): RenderedObject

  def map(f: Facet => Facet): FacetRenderedObject = RenderedObject.fromFacets(facets.map(f))

  def filter(f: Facet => Boolean): FacetRenderedObject = RenderedObject.fromFacets(facets.filter(f))

  def filterNot(f: Facet => Boolean): FacetRenderedObject = RenderedObject.fromFacets(facets.filterNot(f))
}

final case class FacetRenderedObject(facets: Seq[Facet]) extends RenderedObject {
  lazy val vertices: Seq[Vertex] = facets.flatMap(_.vertices).distinct

  def tree: BSPTree = BSPTree(Facet.toPolygons(facets))

  def invert: RenderedObject = FacetRenderedObject(facets.map(_.flip))

  def translate(v: Vertex): RenderedObject = FacetRenderedObject(facets.map(_.moved(v.x, v.y, v.z)))
}

final case class BSPTreeRenderedObject(tree: BSPTree) extends RenderedObject {
  lazy val vertices: Seq[Vertex] = tree.allPolygons.flatMap(_.vertices)

  def facets: Seq[Facet] = RenderedObject.treeToFacets(tree)

  def invert: RenderedObject = BSPTreeRenderedObject(tree.inverted)

  def translate(v: Vertex): RenderedObject = BSPTreeRenderedObject(tree.translated(v))
}

object RenderedObject {

  val empty: RenderedObject = FacetRenderedObject(Seq.empty)

  def fromFacets(facets: Seq[Facet]): FacetRenderedObject = FacetRenderedObject(facets)

  def fromVertices(vertices: Seq[Vertex]): FacetRenderedObject = fromFacets(Facet.fromVertices(vertices))

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

  def treeToFacets(root: BSPTree): Seq[Facet] = {
    val polygons = root.allPolygons
    val vertices = polygons.flatMap(_.vertices).distinct
    val octree = Octree(vertices)
    polygons.par.flatMap { p =>
      Facet.fromVertices(p.vertices).flatMap(f => insertPoints(f, octree).filter(validFacet))
    }.seq
  }
}
