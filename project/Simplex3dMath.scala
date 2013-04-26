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
  
  val buildSettings = Common.buildSettings ++ Seq(
    version := Simplex3d.MathVersion,
    startYear := Some(2009),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html")))
  )
  
  lazy val root = Project(
    id = "math",
    base = file("."),
    settings = buildSettings ++ Seq(
      target := new File("target/math")
    )
  ) aggregate(core, double, float)
  
  lazy val core = Project(
    id = "math-core",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Common.publishSettings ++ Seq(
      name := "simplex3d-math-core",
      description := "Vector Math DSL, Core Module.",
      target := new File("target/math/core"),
      scalaSource in Compile <<= baseDirectory(_ / "src/core")
    )
  )
  
  lazy val double = Project(
    id = "math-double",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Common.publishSettings ++ Seq(
      name := "simplex3d-math-double",
      description := "Vector Math DSL, Double Module.",
      target := new File("target/math/double"),
      scalaSource in Compile <<= baseDirectory(_ / "src/double")
    )
  ) dependsOn(core)
  
  lazy val float = Project(
    id = "math-float",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Common.publishSettings ++ Seq(
      name := "simplex3d-math-float",
      description := "Vector Math DSL, Float Module.",
      target := new File("target/math/float"),
      scalaSource in Compile <<= baseDirectory(_ / "src/float")
    )
  ) dependsOn(core)
  
  lazy val doc = Project(
    id = "math-doc",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Seq(
      target := new File("target/math/doc"),
      excludeFilter := "*",
      sourceGenerators in Compile <+= baseDirectory map { base =>
        StripSwizzling.stripCopy(base / "src/core", new File("target/math/doc/modified-src")) ++
        StripSwizzling.stripCopy(base / "src/float", new File("target/math/doc/modified-src")) ++
        StripSwizzling.stripCopy(base / "src/double", new File("target/math/doc/modified-src"))
      }
    )
  )
  
  lazy val test = Project(
    id = "math-test",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Common.testSettings ++ Seq(
      name := "simplex3d-math-test",
      description := "Vector Math DSL, Tests.",
      licenses := Seq(("GPLv3+", new URL("http://www.gnu.org/licenses/gpl.html"))),
      target := new File("target/math/test")
    )
  ) dependsOn(core, double, float)
  
  lazy val example = Project(
    id = "math-example",
    base = file("Simplex3dMath"),
    settings = buildSettings ++ Common.exampleSettings ++ Seq(
      target := new File("target/math/example")
    )
  ) dependsOn(core, double, Simplex3dScript.core)
}


object StripSwizzling {
  import java.io._
  import scala.collection.mutable.ArrayBuffer
  
  def stripCopy(src: File, dest: File) :Seq[File] = {
    val res = new ArrayBuffer[File]
    recursive(src, dest, res)
    res
  }
  
  private def recursive(dir: File, out: File, res: ArrayBuffer[File]) {
    if (!out.exists) out.mkdirs()
    
    for (entry <- dir.listFiles) {
      if (entry.isDirectory) recursive(entry, new File(out, entry.getName()), res)
      else if (entry.getName.endsWith(".java")) { res += copy(entry, out) }
      else if (entry.getName.endsWith(".scala")) { res += filter(entry, out) }
    }
  }
  
  private def copy(file: File, todir: File) :File = {
    val dest = new File(todir, file.getName)
    Util.copy(file, dest)
    dest
  }
  
  private def filter(file: File, todir: File) :File = {
    var source: scala.io.Source = null
    try {
      source = scala.io.Source.fromFile(file)
      val lines = source.getLines.toArray
      
      if (lines.find(_.contains("@SwizzlingStart")).isEmpty) copy(file, todir)
      else stripSwizzling(lines, new File(todir, file.getName))
    }
    finally {
      if (source != null) source.close()
    }
  }
  
  private def stripSwizzling(lines: Array[String], dest: File) :File = {
    var out: BufferedWriter = null
    try {
      out = new BufferedWriter(new FileWriter(dest))
      
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
    }
    finally {
      if (out != null) out.close()
    }
    
    dest
  }
}
