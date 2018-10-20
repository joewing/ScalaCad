package net.joewing.scalacad.examples

import net.joewing.scalacad.io.{AwtRenderer, StlFileWriter}
import net.joewing.scalacad.parts.{Platonic, Threads}
import net.joewing.scalacad.primitives._

object Lamp extends App {

  val bottomHeight = 10
  val bottomRadius = 200 / 2
  val stemRadius = 24.0 / 2
  val baseHeight = 200
  val cordRadius = 5
  val socketRadius = 20 / 2
  val socketHeight = 40
  val screwTurns = 4
  val screwHeight = 10
  val threadDepth = 1
  val tolerance = 0.4

  val topSize = 150
  val wallWidth = tolerance * 2

  val base = {
    union(
      cylinder(bottomHeight, bottomRadius, sides = 5),
      cylinder(baseHeight - bottomHeight - screwHeight, stemRadius).translate(z = bottomHeight),
      Threads(stemRadius, screwHeight, screwTurns, threadRatio = stemRadius / (stemRadius + threadDepth))
        .translate(z = baseHeight - screwHeight)
    ) - union(
      cylinder(bottomRadius, cordRadius)
        .rotate(x = math.Pi / 2, y = -math.Pi / 10)
        .translate(z = cordRadius * 5 / 6),
      cylinder(baseHeight, cordRadius),
      cylinder(bottomHeight, socketRadius, cordRadius),
      cylinder(socketHeight, socketRadius).translate(z = baseHeight - socketHeight)
    )
  }

  val shade = {
    union(
      (
        Platonic.dodecahedron(topSize) - union(
          Platonic.dodecahedron(topSize - wallWidth * 2),
          cylinder(topSize / 2, stemRadius)
        )
      ).translate(z = topSize / 2),
      cylinder(topSize, topSize / 4, stemRadius) - cylinder(topSize, topSize / 4 - wallWidth, stemRadius - wallWidth),
      (
        cylinder(screwHeight, stemRadius + 4 * threadDepth) -
        Threads(stemRadius + tolerance, screwHeight, screwTurns, threadRatio = stemRadius / (stemRadius + threadDepth))
      ).translate(z = topSize)
    )
  }

  StlFileWriter.write(base, "lamp-base.stl")
  StlFileWriter.write(shade, "lamp-shade.stl")

  AwtRenderer.show(shade)
}
