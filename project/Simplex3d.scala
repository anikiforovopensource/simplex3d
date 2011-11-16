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

import java.util.regex.Pattern
import sbt._
import Keys._


object Simplex3d extends Build {
  
  val codeFilter = "*.scala" || "*.java"
  
  val buildSettings = Defaults.defaultSettings ++ Seq (
    scalaVersion := "2.9.1",
    organization := "org.simplex3d",
    homepage := Some(new URL("http://www.simplex3d.org/")),
    scalaSource in Compile <<= baseDirectory(_ / "src"),
    scalacOptions += "-deprecation",
    maxErrors := 20
  )
  
  
  // SBT bug: projects have to depend on something to generate docs properly.
  lazy val dummyProjectToFixSbt = Project(
    id = "sbt-fix",
    base = file("."),
    settings = buildSettings ++ Seq (
      name := "dummy-project-to-fix-sbt",
      target := new File("target/sbt-fix"),
      excludeFilter := "*",
      publish := {},
      publishLocal := {}
    )
  )
  
  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings ++ Seq (
      target := new File("target/root"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(
    Simplex3dMath.core, Simplex3dMath.float, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.float, Simplex3dData.double,
    Simplex3dAlgorithm.dataFormat, Simplex3dAlgorithm.noise, Simplex3dAlgorithm.intersection, Simplex3dAlgorithm.mesh,
    Simplex3dEngine.core, Simplex3dEngine.sceneGraph, Simplex3dEngine.renderer,
    Simplex3dEngine.backendOpengl, Simplex3dEngine.backendLwjgl, Simplex3dEngine.default
  ) dependsOn(dummyProjectToFixSbt)
}


class WorkingFilter(regexString: String) extends FileFilter {
  private val pattern = Pattern.compile(".*/" + regexString)
  
  def accept(file: java.io.File) = pattern.matcher(file.getAbsolutePath).matches
}
