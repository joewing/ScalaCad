package net.joewing.scalacad.io

import java.io.{FileOutputStream, OutputStream}

import net.joewing.scalacad.primitives.{Primitive, ThreeDimensional}

object StlFileWriter {
  def write(
    r: Primitive[ThreeDimensional],
    os: OutputStream,
    ascii: Boolean
  ): Unit = {
    if (ascii) {
      StlAsciiFileWriter.write(r, os)
    } else {
      StlBinaryFileWriter.write(r, os)
    }
  }

  def write(
    r: Primitive[ThreeDimensional],
    fileName: String,
    ascii: Boolean = false
  ): Unit = write(r, new FileOutputStream(fileName), ascii)
}
