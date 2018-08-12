# ScalaCad

![Build Status](https://travis-ci.org/joewing/ScalaCad.svg?branch=master)
[![Download](https://api.bintray.com/packages/joewing/maven/scalacad/images/download.svg)](https://bintray.com/joewing/maven/scalacad/_latestVersion)

## Overview

ScalaCad is a Constructive Solid Geometry library/DSL for Scala
with support for creating, visualizing, and exporting 3D models.
Models created with ScalaCad can be exported to the STL file format
for 3D printing.

## ScalaCad Dependency
To use ScalaCad, add the following to your build:
```scala
resolvers += Resolver.bintrayRepo("joewing", "maven")
libraryDependencies += "net.joewing" %% "scalacad" % "<version>"
```

## Creating a 3D Model.

A simple embedded domain-specific language (DSL) for creating models
is available in the `net.joewing.scalacad.primitives` package.

### 3D Objects

The following 3D objects are available:

 - `Cube`
 - `Cylinder`
 - `Sphere`

### 2D Objects

2D objects can be extruded to create 3-dimensional objects using the
`extrude` function.  This permits one to perform rotations, etc., which
enables one to create objects such as screw threads.
The following 3D objects are available:

 - `Circle`
 - `Raster`
 - `Rectangle`
 - `Triangle`

### Transformations

Both 2D and 3D objects can be transformed via the following transformations:

 - `Rotate`
 - `Scale`
 - `Translate`

### Operations

Both 2D and 3D objects can be combined using the following operations:

 - `Difference`
 - `Intersection`
 - `Union`

## Visualizing a 3D Model

Models can be visualized using `AwtRenderer.show` from the`net.joewing.scalacad.io` package.  This will open a window to display the object.

## Saving a 3D Model

Models can be saved to STL files using the `StlFileWriter.write` method
from the `net.joewing.scalacad.io` package.

## Reading a 3D Model

Models can be loaded from STL files using the `StlFileReader.read` method
from the `net.joewing.scalacad.io` package.

## An example

Here's a simple example that can run from the command line as a script.
See the [SBT Documentation](https://www.scala-sbt.org/1.0/docs/Scripts.html)
for instructions on installing the `scalas` script required to run this.
Once `scalas` is available in your path, simply save this file as
`Hole.scala`, make it executable (`chmod +x Hole.scala`), and run it
(`./Hole.scala`).

```scala
#!/usr/bin/env scalas

// A simple ScalaCad example as a Scala script.
// See https://www.scala-sbt.org/1.0/docs/Scripts.html

/***
resolvers += Resolver.bintrayRepo("joewing", "maven")
libraryDependencies += "net.joewing" %% "scalacad" % "0.3"
*/

import net.joewing.scalacad._
import net.joewing.scalacad.primitives._

val obj = cube(10, 10, 10).centered - cylinder(10, 3, 3, 16).centered
io.StlFileWriter.write(obj, "hole.stl")
```
