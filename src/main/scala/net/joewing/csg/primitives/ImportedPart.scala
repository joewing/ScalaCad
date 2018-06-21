package net.joewing.csg.primitives

import net.joewing.csg.io.StlAsciiFileReader
import net.joewing.csg.{BSPTree, Facet, Stl}

case class ImportedPart(facets: Seq[Facet]) extends Primitive[ThreeDimensional] {
  def render: BSPTree = BSPTree(facets)
}

object ImportedPart {
  def fromStl(stl: Stl): ImportedPart = ImportedPart(stl.facets)
  def fromFile(fileName: String): ImportedPart = fromStl(StlAsciiFileReader.read(fileName))
}
