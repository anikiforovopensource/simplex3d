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


object Simplex3dEngine extends Build {
  
  val buildSettings = Simplex3d.buildSettings ++ Seq (
    version := "0.5-SNAPSHOT",
    startYear := Some(2011),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html")))
  )

  val coreFilter = new WorkingFilter("simplex3d/engine/.*")
  val sceneGraphFilter = new WorkingFilter("simplex3d/engine/scenegraph/.*")
  val rendererFilter = new WorkingFilter("simplex3d/engine/renderer/.*")
  
  val backendFilter = new WorkingFilter("simplex3d/engine/backend/.*")
  val backendOpenglFilter = new WorkingFilter("simplex3d/engine/backend/opengl/.*")
  val backendLwjglFilter = new WorkingFilter("simplex3d/engine/backend/lwjgl/.*")
  
  val defaultFilter = new WorkingFilter("simplex3d/engine/default/.*")
  val testFilter = new WorkingFilter("test/.*") //TODO rework test layout
  
  
  lazy val root = Project(
    id = "engine",
    base = file("."),
    settings = buildSettings ++ Seq (
      target := new File("target/engine"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(core, sceneGraph, renderer, backendOpengl, backendLwjgl, default) dependsOn(Simplex3d.dummyProjectToFixSbt)
  
  lazy val core = Project(
    id = "engine-core",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-engine-core",
      description := "Simplex3D Engine, Core Module.",
      target := new File("target/engine/core"),
      includeFilter := coreFilter && Simplex3d.codeFilter,
      excludeFilter := sceneGraphFilter || rendererFilter || defaultFilter || backendFilter || testFilter
    )
  ) dependsOn(
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double, Simplex3dData.format,
    Simplex3dAlgorithm.intersection
  )
  
  lazy val sceneGraph = Project(
    id = "engine-scenegraph",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-engine-scenegraph",
      description := "Simplex3D Engine, Scenegraph Module.",
      target := new File("target/engine/scenegraph"),
      includeFilter := sceneGraphFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core)
  
  lazy val renderer = Project(
    id = "engine-renderer",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-engine-renderer",
      description := "Simplex3D Engine, Renderer Module.",
      target := new File("target/engine/renderer"),
      includeFilter := rendererFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core)
  
  lazy val backendOpengl = Project(
    id = "engine-backend-opengl",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-engine-backend-opengl",
      description := "Simplex3D Engine, Common OpenGL Backend.",
      target := new File("target/engine/backend/opengl"),
      includeFilter := backendOpenglFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core)
  
  lazy val backendLwjgl = Project(
    id = "engine-backend-lwjgl",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-engine-backend-lwjgl",
      description := "Simplex3D Engine, LWJGL Backend.",
      target := new File("target/engine/backend/lwjgl"),
      libraryDependencies += "org.lwjgl.lwjgl" % "lwjgl" % "2.8.1",
      libraryDependencies += "org.lwjgl.lwjgl" % "lwjgl_util" % "2.8.1",
      includeFilter := backendLwjglFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core, backendOpengl)
  
  lazy val default = Project(
    id = "engine-default",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-engine-default",
      description := "Simplex3D Engine, Default Implementation.",
      target := new File("target/engine/default"),
      includeFilter := defaultFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core, sceneGraph, renderer, backendOpengl, backendLwjgl)
  
  
  lazy val doc = Project(
    id = "doc-engine",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq (
      target := new File("target/engine/doc"),
      excludeFilter := testFilter,
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double, Simplex3dData.format,
    Simplex3dAlgorithm.intersection
  )
  
  lazy val test = Project(
    id = "test-engine",
    base = file("."),
    settings = buildSettings ++ Seq (
      name := "simplex3d-engine-test",
      description := "Simplex3D Engine Interactive Tests.",
      target := new File("target/engine/test"),
      scalaSource in Compile <<= baseDirectory(_ / "Simplex3dEngine/src"),
      includeFilter := testFilter && Simplex3d.codeFilter,
      fork := true,
      javaOptions <<= ivyPaths { ivyPath => //TODO change to "map" for "sbt.version=0.11.2-20111110-052207" or higher
        val nativeJarDir = ivyPath.ivyHome.get / "/cache/org.lwjgl.lwjgl/lwjgl-platform/jars/"
        val nativeJars = nativeJarDir.listFiles.filter(_.getName.endsWith(".jar"))
        val targetDir = new File("target/engine/natives")
        for (jar <- nativeJars) {
          IO.unzip(jar, targetDir, new SimpleFilter(!_.toUpperCase.startsWith("META-INF")))
        }
        Seq("-Djava.library.path=" + targetDir)
      },
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(core, sceneGraph, renderer, backendOpengl, backendLwjgl, default, Simplex3dAlgorithm.noise, Simplex3dAlgorithm.mesh)
}
