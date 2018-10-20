package net.joewing.scalacad.examples

import net.joewing.scalacad.io.{AwtRenderer, StlFileReader, StlFileWriter}
import net.joewing.scalacad.parts.Threads
import net.joewing.scalacad.primitives._

object ScrewExample extends App {

  val tolerance = 0.4

  val screwTurns = 4.0
  val screwLength = 8.0
  val screwRadius = 4.0

  val nutLength = 4.0
  val nutRadius = 8.0
  val nutSides = 6

  val nutTurns = screwTurns * nutLength / screwLength

  def cap: Primitive[ThreeDimensional] = cylinder(nutLength, nutRadius, sides = nutSides)

  def screwThreads = Threads(screwRadius, screwLength, screwTurns)

  def screw: Primitive[ThreeDimensional] = screwThreads above cap

  def nutThreads = Threads(screwRadius + tolerance, nutLength, nutTurns)

  def nut: Primitive[ThreeDimensional] = cap.translate(x = nutRadius * 4) - nutThreads.translate(x = nutRadius * 4)

  val obj = screw.beside(nut, overlap = -nutRadius)

  val start = System.currentTimeMillis
  StlFileWriter.write(obj, "screw.stl")
  val end = System.currentTimeMillis
  println(s"Duration: ${end - start} ms")

  AwtRenderer.show(StlFileReader.read("screw.stl"))
}
