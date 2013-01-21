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
  
  val ScalaVersion = "2.10.0"
  val LwjglVersion = "2.8.4"
  
  val MathVersion = "2.4.5-SNAPSHOT"
  val DataVersion = "2.4.6-SNAPSHOT"
  val AlgorithmNoiseVersion = "0.4.6-SNAPSHOT"
  val AlgorithmIntersectionVersion = "0.4.6-SNAPSHOT"
  val AlgorithmMeshVersion = "0.4.6-SNAPSHOT"
  val EngineVersion = "0.3.4-SNAPSHOT"
  val ConsoleVersion = "0.3.4-SNAPSHOT"
  val ScriptVersion = "0.3.4-SNAPSHOT"
  
  val ScalatestVersion = "1.9.1"
  
  
  lazy val allCode = Project(
    id = "all-code",
    base = file("."),
    settings = Common.buildSettings ++ Seq(
      target := new File("target/root"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(root, test, example)
  
  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Common.buildSettings ++ Seq(
      target := new File("target/root"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(
    Simplex3dMath.root, Simplex3dData.root, Simplex3dAlgorithm.root, Simplex3dEngine.root, Simplex3dScript.root, Simplex3dConsole.root
  )
  
  lazy val doc = Project(
    id = "root-doc",
    base = file("."),
    settings = Common.buildSettings ++ Seq(
      target := new File("target/root"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(
    Simplex3dMath.doc, Simplex3dData.doc, Simplex3dAlgorithm.doc, Simplex3dEngine.doc, Simplex3dScript.core
  )
  
  lazy val test = Project(
    id = "root-test",
    base = file("."),
    settings = Common.buildSettings ++ Seq(
      target := new File("target/root"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(
    Simplex3dMath.test, Simplex3dData.test, Simplex3dAlgorithm.test, Simplex3dEngine.test
  )
  
  lazy val example = Project(
    id = "root-example",
    base = file("."),
    settings = Common.buildSettings ++ Seq(
      target := new File("target/root"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(
    Simplex3dMath.example, Simplex3dData.example, Simplex3dAlgorithm.example, Simplex3dEngine.example, Simplex3dScript.example
  )
}
