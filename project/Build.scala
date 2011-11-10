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
import Project.Initialize


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
}


object Simplex3dMath extends Build {
  
  val buildSettings = Simplex3d.buildSettings ++ Seq (
    version := "2.0-SNAPSHOT",
    startYear := Some(2009),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html")))
  )
  
  val coreFilter = new WorkingFilter("simplex3d/math/.*")
  val floatFilter = new WorkingFilter("simplex3d/math/floatx/.*") || new WorkingFilter("simplex3d/math/float/.*")
  val doubleFilter = new WorkingFilter("simplex3d/math/doublex/.*") || new WorkingFilter("simplex3d/math/double/.*")
  
  lazy val root = Project(
    id = "math",
    base = file("."),
    settings = buildSettings ++ Seq (
      target := new File("target/math"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(core, float, double) dependsOn(Simplex3d.dummyProjectToFixSbt)
  
  lazy val core = Project(
    id = "math-core",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-math-core",
      description := "Vector Math DSL, Core Module.",
      target := new File("target/math/core"),
      includeFilter := coreFilter && Simplex3d.codeFilter,
      excludeFilter := floatFilter || doubleFilter
    )
  ) dependsOn(Simplex3d.dummyProjectToFixSbt)
  
  lazy val float = Project(
    id = "math-float",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-math-float",
      description := "Vector Math DSL, Float Module.",
      target := new File("target/math/float"),
      includeFilter := floatFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core)
  
  lazy val double = Project(
    id = "math-double",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-math-double",
      description := "Vector Math DSL, Double Module.",
      target := new File("target/math/double"),
      includeFilter := doubleFilter && Simplex3d.codeFilter
    )
  ) dependsOn(core)
  
  
  lazy val doc = Project(
    id = "doc-math",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Seq (
      target := new File("target/math"),
      excludeFilter := "*",
      sourceGenerators in Compile <+= scalaSource in Compile map { src => {
        StripSwizzling.stripCopy(src, new File("target/math/scaladoc-src"))
      }},
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(Simplex3d.dummyProjectToFixSbt)
  
  lazy val test = Project(
    id = "test-math",
    base = file("Simplex3dMathTest"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-math-test",
      description := "Vector Math DSL, Tests.",
      licenses := Seq(("GPLv3+", new URL("http://www.gnu.org/licenses/gpl.html"))),
      target := new File("target/math"),
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test",
      scalaSource in Compile <<= baseDirectory(_ / "none"),
      scalaSource in Test <<= baseDirectory(_ / "src"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(core, float, double)
}


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
      target := new File("target/data"),
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
      target := new File("target/data"),
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test",
      scalaSource in Compile <<= baseDirectory(_ / "none"),
      scalaSource in Test <<= baseDirectory(_ / "src"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(core, float, double)
}


object Simplex3dAlgorithm extends Build {
  
  val buildSettings = Simplex3d.buildSettings ++ Seq (
    version := "0.5-SNAPSHOT",
    startYear := Some(2010),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html")))
  )
  
  val dataFormatFilter = new WorkingFilter("simplex3d/data/format/.*")
  val noiseFilter = new WorkingFilter("simplex3d/algorithm/noise/.*")
  val intersectionFilter = new WorkingFilter("simplex3d/algorithm/intersection/.*")
  val meshFilter = new WorkingFilter("simplex3d/algorithm/mesh/.*")
  
  
  lazy val root = Project(
    id = "algorithm",
    base = file("."),
    settings = buildSettings ++ Seq (
      target := new File("target/algorithm"),
      publish := {},
      publishLocal := {}
    )
  ) aggregate(dataFormat, noise, intersection, mesh) dependsOn(Simplex3d.dummyProjectToFixSbt)
  
  lazy val dataFormat = Project(
    id = "data-format",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-data-format",
      description := "Additional data formats for Data Binding API.",
      target := new File("target/algorithm/data-format"),
      includeFilter := dataFormatFilter && Simplex3d.codeFilter
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double, Simplex3dData.core, Simplex3dData.double)
  
  lazy val noise = Project(
    id = "algorithm-noise",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-algorithm-noise",
      description := "Noise Algorithms.",
      target := new File("target/algorithm/noise"),
      includeFilter := noiseFilter && Simplex3d.codeFilter
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double)
  
  lazy val intersection = Project(
    id = "algorithm-intersection",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-algorithm-intersection",
      description := "Intersection and Collision Algorithms.",
      target := new File("target/algorithm/intersection"),
      includeFilter := intersectionFilter && Simplex3d.codeFilter
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double)
  
  lazy val mesh = Project(
    id = "algorithm-mesh",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-algorithm-mesh",
      description := "Algorithms to generate and work with mesh data.",
      target := new File("target/algorithm/mesh"),
      includeFilter := meshFilter && Simplex3d.codeFilter
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double, Simplex3dData.core, Simplex3dData.double)
  
  
  lazy val doc = Project(
    id = "doc-algorithm",
    base = file("Simplex3dAlgorithm"),
    settings = buildSettings ++ Seq (
      target := new File("target/algorithm"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(Simplex3dMath.core, Simplex3dMath.double, Simplex3dData.core, Simplex3dData.double)
}


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
    Simplex3dData.core, Simplex3dData.double,
    Simplex3dAlgorithm.dataFormat, Simplex3dAlgorithm.intersection
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
      target := new File("target/engine"),
      excludeFilter := testFilter,
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double,
    Simplex3dAlgorithm.dataFormat, Simplex3dAlgorithm.intersection
  )
  
  lazy val test = Project(
    id = "test-engine",
    base = file("Simplex3dEngine"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-engine-test",
      description := "Simplex3D Engine Interactive Tests.",
      target := new File("target/engine"),
      includeFilter := testFilter && Simplex3d.codeFilter,
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(core, sceneGraph, renderer, backendOpengl, backendLwjgl, default, Simplex3dAlgorithm.noise, Simplex3dAlgorithm.mesh)
}


class WorkingFilter(regexString: String) extends FileFilter {
  private val pattern = Pattern.compile(".*/" + regexString)
  
  def accept(file: java.io.File) = pattern.matcher(file.getAbsolutePath).matches
}


object StripSwizzling {
  import java.io._
  import scala.collection.mutable.ArrayBuffer
  
  def stripCopy(src: File, dest: File) :Seq[File] = {
    val res = new ArrayBuffer[File]
    rec(src, dest, res)
    res
  }
  
  def rec(dir: File, out: File, res: ArrayBuffer[File]) {
    if (!out.exists) out.mkdirs()
    
    for (entry <- dir.listFiles) {
      if (entry.isDirectory) rec(entry, new File(out, entry.getName()), res)
      else if (entry.getName.endsWith(".java")) { res += copy(entry, out) }
      else if (entry.getName.endsWith(".scala")) { res += filter(entry, out) }
    }
  }
  
  def copy(file: File, todir: File) :File = {
    val dest = new File(todir, file.getName)
    
    val in = new BufferedInputStream(new FileInputStream(file))
    val out = new BufferedOutputStream(new FileOutputStream(dest))
    
    val buff = new Array[Byte](1024)
    var read = 0; while (read >= 0) {
      out.write(buff, 0, read)
      read = in.read(buff)
    }
    
    in.close()
    out.close()
    
    dest
  }
  
  def filter(file: File, todir: File) :File = {
    val lines = scala.io.Source.fromFile(file).getLines.toArray
    if (lines.find(_.contains("@SwizzlingStart")).isEmpty) copy(file, todir)
    else stripSwizzling(lines, new File(todir, file.getName))
  }
  
  def stripSwizzling(lines: Array[String], dest: File) :File = {
    val out = new BufferedWriter(new FileWriter(dest))
    
    var excludeEnd = 0
    var excludeStart = lines.indexWhere(_.contains("@SwizzlingStart"))
    
    var i = 0; while (i < lines.length) {
      i = excludeEnd; while (i < excludeStart) {
        out.write(lines(i))
        out.write("\n")
      
        i += 1
      }
      excludeEnd = lines.indexWhere(_.contains("@SwizzlingEnd"), excludeStart) + 1
      excludeStart = lines.indexWhere(_.contains("@SwizzlingStart"), excludeEnd)
      if (excludeStart == -1) excludeStart = lines.length
    }
    
    out.close()
    
    dest
  }
}
