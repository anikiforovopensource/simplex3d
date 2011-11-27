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
  
  val exampleSettings = buildSettings ++ Seq(
    scalaSource in Compile <<= baseDirectory(_ / "example"),
    
    compile in Compile <<= (scalaSource in Compile, classDirectory in Compile, compile in Compile) map {
    (src, classes, compileRes) =>
      val exampleFiles = new FileSet(src, List(""".*\.scala"""))
      exampleFiles foreach { (path, openStream) =>
        Util.copy(openStream(), classes / path)
      }
      compileRes
    },
    
    packageBin in Compile <<= (scalaSource in Compile, packageBin in Compile) map { (src, dest) =>
      Jar.create(dest, new FileSet(src, List(""".*\.scala""")) :: Nil)
      dest
    },
    
    publish := {},
    publishLocal := {}
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


import java.io.{ File => IoFile, _ }
import java.util.zip._
import java.util.regex._


class FileSet(val src: File, val includePatterns: List[String] = List(".*"), val excludePatterns: List[String] = Nil) {
  private val included = includePatterns.map(r => Pattern.compile(r))
  private val excluded = {
    val patterns = excludePatterns.map(r => Pattern.compile(r))
    if (src.getName.toLowerCase.endsWith(".jar"))
      Pattern.compile("""META-INF/.*""", Pattern.CASE_INSENSITIVE) :: patterns 
    else
      patterns
  }
  
  private def keep(path: String) :Boolean = {
    val include = included.find(_.matcher(path).matches).isDefined
    val exclude = if (include) excluded.find(_.matcher(path).matches).isDefined else true
    
    include && !exclude
  }
  
  
  def foreach(function: (String, () => InputStream) => Unit) {
    if (!src.exists) return
      
    if (src.isDirectory) dirForeach(function)
    else zipForeach(function)
  }
  
  private def dirForeach(function: (String, () => InputStream) => Unit) {
    val base = src.toURI
    
    def recurse(dir: File) {
      for (entry <- dir.listFiles) {
        entry match {
          
          case dir if dir.isDirectory =>
            recurse(dir)
            
          case file =>
            val path = base.relativize(file.toURI).getPath
            if (keep(path)) {
               var in: InputStream = null
               try {
                 val openStream = () => { if (in == null) in = new BufferedInputStream(new FileInputStream(file)); in }
                 function(path, openStream)
               }
               finally {
                 if (in != null) in.close()
               }
            }
        }
      }
    }
    
    recurse(src)
  }
  
  private def zipForeach(function: (String, () => InputStream) => Unit) {
    var zipIn: ZipInputStream = null
    try {
      zipIn = new ZipInputStream(new BufferedInputStream(new FileInputStream(src)))
      val managedIn = new FilterInputStream(zipIn) { override def close() {} }
      val openStream = () => managedIn
      
      var entry = zipIn.getNextEntry(); while (entry != null) {
        if (!entry.isDirectory && keep(entry.getName)) function(entry.getName, openStream)
        
        entry = zipIn.getNextEntry()
      }
    }
    finally {
      if (zipIn != null) zipIn.close()
    }
  }
}


object Util {
  private final val buff = new Array[Byte](1024*8)
  
  def copy(in: InputStream, dest: File) {
    this.synchronized {
      var out: FileOutputStream = null
      try {
        dest.getParentFile.mkdirs()
        out = new FileOutputStream(dest)
        var read = 0; while (read >= 0) {
          out.write(buff, 0, read)
          read = in.read(buff)
        }
      }
      finally {
        if (out != null) out.close()
      }
    }
  }
  
  def copy(in: InputStream, out: OutputStream) {
    this.synchronized {
      var read = 0; while (read >= 0) {
        out.write(buff, 0, read)
        read = in.read(buff)
      }
    }
  }
  
  def writeFile(file: File, contents: String) {
    this.synchronized {
      var out: OutputStreamWriter = null
      try {
        file.getParentFile.mkdirs()
        out = new OutputStreamWriter(new FileOutputStream(file))
        out.write(contents)
      }
      finally {
        if (out != null) out.close()
      }
    }
  }
}


object Jar {
  val DefaultManifest = "Manifest-Version: 1.0\n\n"
  
  private def loadFile(file: File) :String = {
    val source = scala.io.Source.fromFile(file)
    val res = source.mkString
    source.close ()
    res
  }
  
  def create(dest: File, fileSets: Seq[FileSet], manifest: File = null) {
    val manifestString = if (manifest == null) DefaultManifest else loadFile(manifest)
    
    var zip: ZipOutputStream = null
    try {
      zip = new ZipOutputStream(new FileOutputStream(dest))
      
      zip.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF"))
      zip.write(manifestString.getBytes())
      zip.closeEntry()
      
      for (fileSet <- fileSets) {
        fileSet.foreach { (path, openStream) =>
          zip.putNextEntry(new ZipEntry(path))
          Util.copy(openStream(), zip)
        }
      }
    }
    finally {
      if (zip != null) zip.close()
    }
  }
}
