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

import scala.collection.mutable.ArrayBuffer
import sbt._
import Keys._


object Simplex3dEngine extends Build {
  
  val buildSettings = Common.buildSettings ++ Seq(
    version := Simplex3d.EngineVersion,
    startYear := Some(2011),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html")))
  )

  lazy val root = Project(
    id = "engine",
    base = file("."),
    settings = buildSettings ++ Seq(
      target := new File("target/engine"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(core, sceneGraph, renderer, backendOpengl, backendLwjgl, default)
  
  lazy val core = Project(
    id = "engine-core",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-engine-core",
      description := "Simplex3D Engine, Core Module.",
      target := new File("target/engine/core"),
      scalaSource in Compile <<= baseDirectory(_ / "src/core")
    )
  ) dependsOn(
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double, Simplex3dData.format,
    Simplex3dAlgorithm.intersection
  )
  
  lazy val sceneGraph = Project(
    id = "engine-scenegraph",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-engine-scenegraph",
      description := "Simplex3D Engine, Scenegraph Module.",
      target := new File("target/engine/scenegraph"),
      scalaSource in Compile <<= baseDirectory(_ / "src/scenegraph")
    )
  ) dependsOn(core)
  
  lazy val renderer = Project(
    id = "engine-renderer",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-engine-renderer",
      description := "Simplex3D Engine, Renderer Module.",
      target := new File("target/engine/renderer"),
      scalaSource in Compile <<= baseDirectory(_ / "src/renderer")
    )
  ) dependsOn(core)
  
  lazy val backendOpengl = Project(
    id = "engine-backend-opengl",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-engine-backend-opengl",
      description := "Simplex3D Engine, Common OpenGL Backend.",
      target := new File("target/engine/backend/opengl"),
      scalaSource in Compile <<= baseDirectory(_ / "src/backend-opengl")
    )
  ) dependsOn(core)
  
  lazy val backendLwjgl = Project(
    id = "engine-backend-lwjgl",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Common.lwjglSettings ++ Seq(
      name := "simplex3d-engine-backend-lwjgl",
      description := "Simplex3D Engine, LWJGL Backend.",
      target := new File("target/engine/backend/lwjgl"),
      scalaSource in Compile <<= baseDirectory(_ / "src/backend-lwjgl")
    )
  ) dependsOn(core, backendOpengl)
  
  lazy val default = Project(
    id = "engine-default",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq(
      name := "simplex3d-engine-default",
      description := "Simplex3D Engine, Default Implementation.",
      target := new File("target/engine/default"),
      scalaSource in Compile <<= baseDirectory(_ / "src/default")
    )
  ) dependsOn(core, sceneGraph, renderer, backendOpengl, backendLwjgl)
  
  
  lazy val doc = Project(
    id = "engine-doc",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq(
      target := new File("target/engine/doc"),
      sourceGenerators in Compile <+= baseDirectory map { base =>
        val files = new ArrayBuffer[File]
        def index(dir: File) = {
          new FileSet(dir).foreach((path, _) => files += new File(dir, path))
          files: Seq[File]
        }
        index(base / "src/core") ++
        index(base / "src/scenegraph") ++
        index(base / "src/renderer") ++
        index(base / "src/default")
      },
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double, Simplex3dData.format,
    Simplex3dAlgorithm.intersection
  )
  
  lazy val test = Project(
    id = "engine-test",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Common.lwjglSettings ++ Seq(
      target := new File("target/engine/test"),
      scalaSource in Compile <<= baseDirectory(_ / "test/visual"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(
    core, sceneGraph, renderer, backendOpengl, backendLwjgl, default,
    Simplex3dAlgorithm.mesh, Simplex3dAlgorithm.noise
  )
  
  lazy val example = Project(
    id = "engine-example",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Common.exampleSettings ++ Seq(
      target := new File("target/engine/example")
    )
  ) dependsOn(
    core, sceneGraph, renderer, backendOpengl, backendLwjgl, default,
    Simplex3dAlgorithm.mesh, Simplex3dAlgorithm.noise
  )
}
