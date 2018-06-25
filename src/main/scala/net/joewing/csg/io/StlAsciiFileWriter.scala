package net.joewing.csg.io

import java.io.{FileOutputStream, OutputStream, PrintStream}

import net.joewing.csg._
import net.joewing.csg.primitives.{Primitive, ThreeDimensional}

class StlAsciiFileWriter(os: OutputStream) extends StlWriter {

  private def fmt(d: Double): String = f"$d%.6e"

  private def vertexString(v: Vertex): String = s"${fmt(v.x1)} ${fmt(v.x2)} ${fmt(v.x3)}"

  private def writeFacet(ps: PrintStream, facet: Facet): Unit = {
    ps.println(s"  facet normal 0 0 0")
    ps.println(s"    outer loop")
    facet.vertices.foreach { v =>
      ps.println(s"      vertex ${vertexString(v)}")
    }
    ps.println(s"    endloop")
    ps.println(s"  endfacet")
  }

  def write(stl: Stl): Unit = {
    val ps = new PrintStream(os)
    try {
      ps.println(s"solid ${stl.name}")
      stl.facets.foreach { facet =>
        writeFacet(ps, facet)
      }
      ps.println(s"endsolid ${stl.name}")
    } finally {
      ps.close()
      println(s"Wrote ${stl.facets.size} facets")
    }
  }

}

object StlAsciiFileWriter {
  def write(stl: Stl, os: OutputStream): Unit = new StlAsciiFileWriter(os).write(stl)
  def write(stl: Stl, fileName: String): Unit = write(stl, new FileOutputStream(fileName))
  def write(r: Primitive[ThreeDimensional], os: OutputStream): Unit = write(Stl("", r.render.toFacets), os)
  def write(r: Primitive[ThreeDimensional], fileName: String): Unit = write(Stl(fileName, r.render.toFacets), fileName)
}