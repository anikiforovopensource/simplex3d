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


object Simplex3dScript extends Build {
  
  val buildSettings = Common.buildSettings ++ Seq(
    version := Simplex3d.ScriptVersion,
    startYear := Some(2010),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html")))
  )

  lazy val root = core
  
  lazy val core = Project(
    id = "script",
    base = file("Simplex3dScript"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-script",
      description := "Scripting API.",
      target := new File("target/script"),
      scalaSource in Compile <<= baseDirectory(_ / "src")
    )
  ) dependsOn(
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double,
    Simplex3dEngine.core, Simplex3dEngine.sceneGraph, Simplex3dEngine.renderer,
    Simplex3dEngine.backendOpengl, Simplex3dEngine.backendLwjgl, Simplex3dEngine.vanilla
  )
  
  lazy val example = Project(
    id = "script-example",
    base = file("Simplex3dScript"),
    settings = buildSettings ++ Common.exampleSettings ++ Seq(
      target := new File("target/script/example"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(core)
}
