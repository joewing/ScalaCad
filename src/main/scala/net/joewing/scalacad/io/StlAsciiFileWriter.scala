package net.joewing.scalacad.io

import java.io.{FileOutputStream, OutputStream, PrintStream}

import net.joewing.scalacad._
import net.joewing.scalacad.primitives.{Primitive, ThreeDimensional}

class StlAsciiFileWriter(os: OutputStream) {

  private def fmt(d: Double): String = d.toString

  private def vertexString(v: Vertex): String = s"${fmt(v.x)} ${fmt(v.y)} ${fmt(v.z)}"

  private def writeFacet(ps: PrintStream, facet: Facet): Unit = {
    ps.println(s"  facet normal ${vertexString(facet.normal)}")
    ps.println(s"    outer loop")
    facet.vertices.foreach { v =>
      ps.println(s"      vertex ${vertexString(v)}")
    }
    ps.println(s"    endloop")
    ps.println(s"  endfacet")
  }

  def write(name: String, facets: Seq[Facet]): Unit = {
    val ps = new PrintStream(os)
    try {
      ps.println(s"solid $name")
      facets.foreach { facet =>
        writeFacet(ps, facet)
      }
      ps.println(s"endsolid $name")
    } finally {
      ps.close()
    }
  }

}

object StlAsciiFileWriter {
  def write(r: Primitive[ThreeDimensional], os: OutputStream): Unit = {
    new StlAsciiFileWriter(os).write("", r.rendered.facets)
  }
  def write(r: Primitive[ThreeDimensional], fileName: String): Unit = write(r, new FileOutputStream(fileName))
}