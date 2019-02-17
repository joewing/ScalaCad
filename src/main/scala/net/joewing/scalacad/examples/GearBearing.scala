package net.joewing.scalacad.examples

import net.joewing.scalacad._
import net.joewing.scalacad.primitives._

/** Gear bearing example.
  * Based off of the Gear Bearing by emmett:
  * https://www.thingiverse.com/thing:53451
  */
object GearBearing extends App {

  private def herringbone(
    numberOfTeeth: Int = 15,
    circularPitch: Double = math.toRadians(10),
    pressureAngle: Double = math.toRadians(28),
    depthRatio: Double = 1,
    clearance: Double = 0,
    helixAngle: Double = math.toRadians(1),
    gearThickness: Double = 5
  ): Primitive[ThreeDimensional] = {
    val g = gear(numberOfTeeth, circularPitch, pressureAngle, depthRatio, clearance, helixAngle, gearThickness / 2.0)
    g | g.scale(1, 1, -1)
  }

  private def gear(
    numberOfTeeth: Int = 15,
    circularPitch: Double = math.toRadians(10),
    pressureAngle: Double = math.toRadians(28),
    depthRatio: Double = 1,
    clearance: Double = 0,
    helixAngle: Double = math.toRadians(0),
    gearThickness: Double = 5
  ): Primitive[ThreeDimensional] = {
    val pitchRadius = numberOfTeeth * circularPitch / (2.0 * math.Pi)
    val twist = math.tan(helixAngle) * gearThickness / pitchRadius
    val slices = 3
    gear2d(numberOfTeeth, circularPitch, pressureAngle, depthRatio, clearance)
      .extrude(gearThickness, twist / slices, slices = slices)
  }

  private def gear2d(
    numberOfTeeth: Int,
    circularPitch: Double,
    pressureAngle: Double,
    depthRatio: Double,
    clearance: Double
  ): Primitive[TwoDimensional] = {
    val pitchRadius = numberOfTeeth * circularPitch / (2 * math.Pi)
    val baseRadius = pitchRadius * math.cos(pressureAngle)
    val depth = circularPitch / (2 * math.tan(pressureAngle))
    val outerRadius = if (clearance < 0) pitchRadius + depth / 2 - clearance else pitchRadius + depth / 2
    val rootRadius1 = pitchRadius - depth / 2 - clearance / 2
    val rootRadius = if (clearance < 0 && rootRadius1 < baseRadius) baseRadius else rootRadius1
    val backlashAngle = clearance / (pitchRadius * math.cos(pressureAngle))
    val halfThickAngle = (math.Pi / 2) / numberOfTeeth - backlashAngle / 2
    val pitchPoint = involute(baseRadius, involuteIntersectAngle(baseRadius, pitchRadius))
    val pitchAngle = math.atan2(pitchPoint._2, pitchPoint._1)
    val minRadius = math.max(baseRadius, rootRadius)

    val halfTeeth = Vector.range(0, numberOfTeeth).map { i =>
      val a = halftooth(pitchAngle, baseRadius, minRadius, outerRadius, halfThickAngle)
      val b = a.scale(1, -1)
      (a | b).rotate(i * math.Pi * 2.0 / numberOfTeeth)
    }

    union(
      circle(
        r = math.max(rootRadius, pitchRadius - depthRatio * circularPitch / 2 - clearance / 2),
        sides = numberOfTeeth * 2
      ).rotate((math.Pi / 2) / numberOfTeeth) +: halfTeeth: _*
    )
  }

  private def halftooth(
    pitchAngle: Double,
    baseRadius: Double,
    minRadius: Double,
    outerRadius: Double,
    halfThickAngle: Double
  ): Primitive[TwoDimensional] = {
    val index = Vector.range(0, 6)
    val startAngle = math.max(involuteIntersectAngle(baseRadius, minRadius) - math.toRadians(5), 0)
    val stopAngle = involuteIntersectAngle(baseRadius, outerRadius)
    val angle = index.map(i => startAngle + i * (stopAngle - startAngle) / index.last)
    polygon(
      (0.0 -> 0.0) +: angle.map(a => involute(baseRadius, a)): _*
    ).rotate(-pitchAngle - halfThickAngle) - square(2 * outerRadius)
  }

  private def involuteIntersectAngle(baseRadius: Double, radius: Double): Double = {
    math.sqrt(math.pow(radius / baseRadius, 2) - 1)
  }

  private def involute(
    baseRadius: Double,
    involuteAngle: Double
  ): (Double, Double) = {
    val a = baseRadius * (math.cos(involuteAngle) + involuteAngle * math.sin(involuteAngle))
    val b = baseRadius * (math.sin(involuteAngle) - involuteAngle * math.cos(involuteAngle))
    a -> b
  }

  val diameter = 51.7
  val thickness = 15.0
  val tolerance = 0.15
  val nPlanets: Int = 6
  val planetTeeth: Int = 6
  val sunTeeth: Int = 9
  val pressureAngle = math.toRadians(45)
  val nTwist = 1
  val depthRatio = 0.5

  val k1 = math.round(2.0 / nPlanets * (sunTeeth + planetTeeth)).toInt
  val k = if ((k1 * nPlanets) % 2 != 0) k1 + 1 else k1
  val ns = k * nPlanets / 2 - planetTeeth
  val nr = ns + 2 * planetTeeth
  val pitchD = 0.9 * diameter / (1 + math.min(math.Pi / (2 * nr * math.tan(pressureAngle)), math.Pi * depthRatio / nr))
  val pitch = pitchD * math.Pi / nr
  val helixAngle = math.atan2(2 * nTwist * pitch, thickness)

  val ring = cylinder(thickness, diameter / 2).centered -
      herringbone(nr, pitch, pressureAngle, depthRatio, -tolerance, helixAngle, thickness + 0.2)

  val sun = herringbone(ns, pitch, pressureAngle, depthRatio, tolerance, helixAngle, thickness)
    .rotate(z = math.Pi / ns)
    .scale(y = -1)

  val planets = disjointUnion(
    Vector.range(1, nPlanets + 1).map { i =>
      herringbone(planetTeeth, pitch, pressureAngle, depthRatio, tolerance, helixAngle, thickness)
        .rotate(0, 0, 2.0 * math.Pi * i * ns / nPlanets / planetTeeth)
        .translate(x = pitchD / 2 * (ns + planetTeeth) / nr)
        .rotate(0, 0, i * math.Pi * 2 / nPlanets)
    }
    : _*
  )

  val obj = disjointUnion(sun, planets, ring)
  io.AwtRenderer.show(obj)

}
