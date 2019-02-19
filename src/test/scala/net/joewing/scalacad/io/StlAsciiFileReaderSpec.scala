package net.joewing.scalacad.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import net.joewing.scalacad.Polygon3d
import net.joewing.scalacad.primitives.Cube
import org.scalatest.{FunSpec, Matchers}

class StlAsciiFileReaderSpec extends FunSpec with Matchers {
  describe("read") {
    it("can read an STL file") {
      val obj = Cube(10, 10, 10)

      val os = new ByteArrayOutputStream()
      StlAsciiFileWriter.write(obj, os)
      val is = new ByteArrayInputStream(os.toByteArray)

      StlAsciiFileReader.read(is).polygons shouldBe obj.rendered.facets.map(f => Polygon3d(f.vertices))
    }
  }
}
