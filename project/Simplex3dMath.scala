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
  ) aggregate(core, float, double)
  
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
  )
  
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
      target := new File("target/math/doc"),
      excludeFilter := "*",
      sourceGenerators in Compile <+= scalaSource in Compile map { src =>
        StripSwizzling.stripCopy(src, new File("target/math/doc/modified-src"))
      },
      publish := {},
      publishLocal := {}
    )
  )
  
  lazy val test = Project(
    id = "test-math",
    base = file("Simplex3dMathTest"),
    settings = buildSettings ++ Seq (
      name := "simplex3d-math-test",
      description := "Vector Math DSL, Tests.",
      licenses := Seq(("GPLv3+", new URL("http://www.gnu.org/licenses/gpl.html"))),
      target := new File("target/math/test"),
      libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test",
      scalaSource in Compile <<= baseDirectory(_ / "none"),
      scalaSource in Test <<= baseDirectory(_ / "src"),
      publish := {},
      publishLocal := {}
    )
  ) dependsOn(core, float, double)
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
