package net.joewing.scalacad.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import net.joewing.scalacad.primitives.Cube
import org.scalatest.{FunSpec, Matchers}

class StlBinaryFileReaderSpec extends FunSpec with Matchers {
  describe("read") {
    it("should read an STL file") {
      val obj = Cube(5, 5, 5)

      val os = new ByteArrayOutputStream()
      StlBinaryFileWriter.write(obj, os)
      val is = new ByteArrayInputStream(os.toByteArray)

      StlBinaryFileReader.read(is).facets shouldBe obj.render.facets
    }
  }
}
