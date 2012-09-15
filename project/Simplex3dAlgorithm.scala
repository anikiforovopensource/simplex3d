/*
 * Simplex3d Build Script
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import sbt._
import Keys._


object Simplex3dAlgorithm extends Build {
  
  val buildSettings = Common.buildSettings ++ Seq(
    version := Simplex3d.AlgorithmVersion,
    startYear := Some(2010),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html")))
  )
  
  lazy val root = Project(
    id = "algorithm",
    base = file("."),
    settings = buildSettings ++ Seq(
      target := new File("target/algorithm"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(intersection, mesh, noise)
  
  lazy val intersection = Project(
    id = "algorithm-intersection",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-algorithm-intersection",
      description := "Intersection and Collision Algorithms.",
      target := new File("target/algorithm/intersection"),
      scalaSource in Compile <<= baseDirectory(_ / "src/intersection")
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double)
  
  lazy val mesh = Project(
    id = "algorithm-mesh",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-algorithm-mesh",
      description := "Algorithms to generate and work with mesh data.",
      target := new File("target/algorithm/mesh"),
      scalaSource in Compile <<= baseDirectory(_ / "src/mesh")
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double, Simplex3dData.core, Simplex3dData.double)
  
  lazy val noise = Project(
    id = "algorithm-noise",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-algorithm-noise",
      description := "Noise Algorithms.",
      target := new File("target/algorithm/noise"),
      scalaSource in Compile <<= baseDirectory(_ / "src/noise")
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double)
  
  
  lazy val doc = Project(
    id = "algorithm-doc",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq(
      target := new File("target/algorithm/doc"),
      scalaSource in Compile <<= baseDirectory(_ / "src"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double, Simplex3dData.core, Simplex3dData.double)
  
  lazy val test = Project(
    id = "algorithm-test",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-algorithm-test",
      description := "Algorithm Tests.",
      licenses := Seq(("GPLv3+", new URL("http://www.gnu.org/licenses/gpl.html"))),
      target := new File("target/algorithm/test"),
      libraryDependencies += "org.scalatest" %% "scalatest" % Simplex3d.ScalatestVersion % "test",
      scalaSource in Compile <<= baseDirectory(_ / "/test/bench"),
      scalaSource in Test <<= baseDirectory(_ / "test/unit"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(intersection, mesh, noise)
  
  lazy val example = Project(
    id = "algorithm-example",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Common.exampleSettings ++ Seq(
      target := new File("target/algorithm/example")
    )
  ) dependsOn(intersection, mesh, noise, Simplex3dScript.core)
}
