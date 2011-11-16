/*
 * Simplex3d build script.
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


object Simplex3dData extends Build {
  
  val buildSettings = Simplex3d.buildSettings ++ Seq (
    version := "2.0-SNAPSHOT",
    startYear := Some(2010),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html")))
  )

  val coreFilter = new WorkingFilter("simplex3d/data/.*")
  val floatFilter = new WorkingFilter("simplex3d/data/float/.*")
  val doubleFilter = new WorkingFilter("simplex3d/data/double/.*")
  
  lazy val root = Project(
    id = "data",
    base = file("."),
    settings = buildSettings ++ Seq (
      target := new File("target/data"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(core, float, double) dependsOn(Simplex3d.dummyProjectToFixSbt)
  
  lazy val core = Project(
    id = "data-core",
    base = file("Simplex3dData"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-data-core",
      description := "Data Binding API, Core Module.",
      target := new File("target/data/core"),
      includeFilter := coreFilter && Simplex3d.codeFilter,
      excludeFilter := floatFilter || doubleFilter
    )
  ) dependsOn(Simplex3dMath.core)
  
  lazy val float = Project(
    id = "data-float",
    base = file("Simplex3dData"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-data-float",
      description := "Data Binding API, Float Module.",
      target := new File("target/data/float"),
      includeFilter := floatFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core, Simplex3dMath.float)
  
  lazy val double = Project(
    id = "data-double",
    base = file("Simplex3dData"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-data-double",
      description := "Data Binding API, Double Module.",
      target := new File("target/data/double"),
      includeFilter := doubleFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core, Simplex3dMath.double)
  
  
  lazy val doc = Project(
    id = "doc-data",
    base = file("Simplex3dData"),
    settings = buildSettings ++ Seq (
      target := new File("target/data/doc"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.float, Simplex3dMath.double)
  
  lazy val test = Project(
    id = "test-data",
    base = file("Simplex3dDataTest"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-data-test",
      description := "Data Binding API, Tests.",
      licenses := Seq(("GPLv3+", new URL("http://www.gnu.org/licenses/gpl.html"))),
      target := new File("target/data/test"),
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test",
      scalaSource in Compile <<= baseDirectory(_ / "none"),
      scalaSource in Test <<= baseDirectory(_ / "src"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(core, float, double)
}
