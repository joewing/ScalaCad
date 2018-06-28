package net.joewing.scalacad.io

import java.io.{FileOutputStream, OutputStream, PrintStream}

import net.joewing.scalacad._
import net.joewing.scalacad.primitives.{Primitive, ThreeDimensional}

class StlAsciiFileWriter(os: OutputStream) {

  private def fmt(d: Double): String = d.toString

  private def vertexString(v: Vertex): String = s"${fmt(v.x1)} ${fmt(v.x2)} ${fmt(v.x3)}"

  private def writeFacet(ps: PrintStream, facet: Facet): Unit = {
    ps.println(s"  facet normal ${vertexString(facet.normal)}")
    ps.println(s"    outer loop")
    facet.vertices.foreach { v =>
      ps.println(s"      vertex ${vertexString(v)}")
    }
    ps.println(s"    endloop")
    ps.println(s"  endfacet")
  }

  def write(name: String, polygons: Seq[Polygon]): Unit = {
    val ps = new PrintStream(os)
    try {
      ps.println(s"solid $name")
      Facet.fromPolygons(polygons).foreach { facet =>
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
    new StlAsciiFileWriter(os).write("", r.render.allPolygons)
  }
  def write(r: Primitive[ThreeDimensional], fileName: String): Unit = write(r, new FileOutputStream(fileName))
}