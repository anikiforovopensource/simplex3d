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

import java.util.regex.Pattern
import sbt._
import Keys._


object Simplex3d extends Build {
  
  val codeFilter = "*.scala" || "*.java"
  
  val buildSettings = Defaults.defaultSettings ++ Seq (
    scalaVersion := "2.9.1",
    organization := "org.simplex3d",
    homepage := Some(new URL("http://www.simplex3d.org/")),
    unmanagedClasspath in Compile += Attributed.blank(new File("dummy-dir-to-fix-doc-task")),
    scalacOptions += "-deprecation",
    maxErrors := 20
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
    Simplex3dMath.root, Simplex3dData.root, Simplex3dAlgorithm.root, Simplex3dEngine.root, Simplex3dScript.root, Simplex3dConsole.root
  )
}
