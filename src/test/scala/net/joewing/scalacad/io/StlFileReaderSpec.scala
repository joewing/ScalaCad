package net.joewing.scalacad.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import net.joewing.scalacad.primitives.Cube
import org.scalatest.{FunSpec, Matchers}

class StlFileReaderSpec extends FunSpec with Matchers {
  describe("read") {
    val obj = Cube(10, 10, 10)

    it("should read a binary STL file") {
      val os = new ByteArrayOutputStream()
      StlBinaryFileWriter.write(obj, os)

      val is = new ByteArrayInputStream(os.toByteArray)
      StlFileReader.read(is).facets shouldBe obj.rendered.facets
    }

    it("should read an ASCII STL file") {
      val os = new ByteArrayOutputStream()
      StlAsciiFileWriter.write(obj, os)

      val is = new ByteArrayInputStream(os.toByteArray)
      StlFileReader.read(is).facets shouldBe obj.rendered.facets
    }
  }
}
