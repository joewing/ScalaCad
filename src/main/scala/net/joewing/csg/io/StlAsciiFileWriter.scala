package net.joewing.csg.io

import java.io.{FileOutputStream, OutputStream, PrintStream}

import net.joewing.csg._
import net.joewing.csg.primitives.{Primitive, ThreeDimensional}

class StlAsciiFileWriter(os: OutputStream) extends StlWriter {

  private def vertexString(v: Vertex): String = s"${v.x1} ${v.x2} ${v.x3}"

  private def writeFacet(ps: PrintStream, facet: Facet): Unit = {
    ps.println(s"  facet normal ${vertexString(facet.normal)}")
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
    }
  }

}

object StlAsciiFileWriter {
  def write(stl: Stl, os: OutputStream): Unit = new StlAsciiFileWriter(os).write(stl)
  def write(stl: Stl, fileName: String): Unit = write(stl, new FileOutputStream(fileName))
  def write(r: Primitive[ThreeDimensional], os: OutputStream): Unit = write(Stl("", r.render.allFacets), os)
  def write(r: Primitive[ThreeDimensional], fileName: String): Unit = write(Stl(fileName, r.render.allFacets), fileName)
}