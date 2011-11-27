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
import Process._


object Simplex3dConsole extends Build {
  
  val buildSettings = Simplex3d.buildSettings ++ Seq (
    version := "0.5-SNAPSHOT",
    startYear := Some(2010),
    licenses := Seq(("LGPLv3+", new URL("http://www.gnu.org/licenses/lgpl.html"))),
    publish := {},
    publishLocal := {}
  )

  // Use this command to generate keys: keytool -genkeypair -keystore keystore.local -alias SOMEALIAS
  // Then move keystore.local to Simplex3dConsole/keystore.local
  val keystoreCredentials = InputKey[Unit]("keystore-credentials")
  var keystoreAlias = ""
  var keystorePass = ""
  
  
  def extractFileSets(classes: File, cp: Seq[Attributed[File]]) = {
    val fileSets = cp.map(a => new FileSet(a.data))

    val (scalaFiles, rest1) = fileSets.partition(_.src.getName.startsWith("scala-"))
    val (simplexFiles, rest2) = rest1.partition { f =>
      val ap = f.src.getAbsolutePath
      ap.contains("/target/math/") ||
      ap.contains("/target/data/") ||
      ap.contains("/target/algorithm/") ||
      ap.contains("/target/engine/") ||
      ap.contains("/target/script")
    }
    
    val (rest3, mainFiles) = rest2.partition{ f =>
      val ap = f.src.getAbsolutePath
      ap.contains("/target/console/example/")
    }
    
    val exampleFiles = rest3.map(fs => new FileSet(fs.src, List(""".*\.scala""")))
    
    val extensionFileset = new FileSet(classes, List("""simplex3d/console/extension/.*"""))
    (scalaFiles, simplexFiles ++ Seq(extensionFileset), mainFiles, exampleFiles)
  }
  
  
  lazy val root = Project(
    id = "console",
    base = file("."),
    settings = buildSettings ++ Seq (
      target := new File("target/console")
    )
  ) aggregate(core, example)
  
  
  lazy val core = Project(
    id = "console-core",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Seq (
      target := new File("target/console/core"),
      mainClass := Some("simplex3d.console.ConsoleFrame"),
      fork in run := true,
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
      unmanagedJars in Compile <<= baseDirectory map { base => (base / "lib" ** "*.jar").classpath },
      scalaSource in Compile <<= baseDirectory(_ / "src"),
      
      compile in Compile <<= (scalaSource in Compile, classDirectory in Compile, dependencyClasspath in Compile, compile in Compile) map { (src, classes, cp, compileRes) =>
        val (scalaFiles, simplexFiles, mainFiles, exampleFiles) = extractFileSets(classes, cp)
        Indexer.index(
          classes / "simplex3d/console/deps.version",
          classes / "simplex3d/console/deps.index", scalaFiles ++ simplexFiles,
          classes / "simplex3d/console/examples.index", exampleFiles
        )
        
        // XXX: port back to ant...
        IO.copyFile(src / "simplex3d/console/simplex3d-logo.png", classes / "simplex3d/console/simplex3d-logo.png")
        
        compileRes
      }
    )
  ) dependsOn(example, Simplex3dScript.core)
  
  
  lazy val example = Project(
    id = "console-example",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Seq (
      target := new File("target/console/example"),
      scalaSource in Compile <<= baseDirectory(_ / "example"),
      
      compile in Compile <<= (baseDirectory, scalaSource in Compile, classDirectory in Compile, compile in Compile) map {
      (base, src, classes, compileRes) =>
        val exampleFiles = new FileSet(new File(base, "example"), List(""".*\.scala"""))
        exampleFiles foreach { (path, openStream) =>
          Util.copy(openStream(), classes / path)
        }
        compileRes
      },
      
      packageBin in Compile <<= (scalaSource in Compile, packageBin in Compile) map { (src, dest) =>
        Jar.create(dest, new FileSet(src, List("""example/.*\.scala""")) :: Nil)
        dest
      }
    )
  ) dependsOn(
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double, Simplex3dData.format,
    Simplex3dAlgorithm.intersection, Simplex3dAlgorithm.mesh, Simplex3dAlgorithm.noise,
    Simplex3dScript.core
  )

  
  lazy val webstart = Project(
    id = "console-webstart",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Seq (
      target := new File("target/console/webstart"),
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-library" % _),
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
      unmanagedJars in Compile <<= baseDirectory map { base => (base / "lib" ** "*.jar").classpath },
      excludeFilter := "*",
      
      keystoreCredentials <<= inputTask { argTask =>
        argTask map { args =>
          require(args.length == 2, "Must enter alias and pass separated by space.")
          keystoreAlias = args(0)
          keystorePass = args(1)
        }
      },
      
      packageBin in Compile <<= (baseDirectory, target, classDirectory in Compile, dependencyClasspath in Compile, packageBin in Compile) map {
        (base, target, classes, cp, jar) =>
        
        require(!keystoreAlias.isEmpty && !keystorePass.isEmpty, "Run keystore-credentials first.")
      
      
        println("Building web-start jars...")
        
        val (scalaFiles, simplexFiles, mainFiles, exampleFiles) = extractFileSets(classes, cp)
        
        
        val scalaDeps = new File(target, "console-ws-scala-deps.jar")
        val simplexDeps = new File(target, "console-ws-simplex3d-deps.jar")
        val main = new File(target, "console-ws-main.jar")
        
        Jar.create(scalaDeps, scalaFiles)
        Jar.create(simplexDeps, simplexFiles)
        Jar.create(main, mainFiles ++ exampleFiles)
        val jars = List(scalaDeps.getAbsolutePath, simplexDeps.getAbsolutePath, main.getAbsolutePath)
        
        val tempLog = java.io.File.createTempFile("pack200", ".log")
        tempLog.deleteOnExit()
        
        
        println("Repacking web-start jars...")
        
        for (jar <- jars) {
          val cmd = "pack200 --repack --log-file=" + tempLog.getAbsolutePath + " " + jar
          require((cmd !)== 0, "Failed to repack jars.")
        }
        
        
        println("Signing web-start jars...")
        
        val keystore = new File(base, "keystore.local")
        
        for (jar <- jars) {
          val cmd =
            "jarsigner" +
            " -keystore " + keystore +
            " -storepass " + keystorePass +
            " -keypass " + keystorePass +
            " " + jar +
            " " + keystoreAlias
          require((cmd !) == 0, "Failed to sign jars.")
        }
        
        
        println("Compressing web-start jars...")
        
        for (jar <- jars) {
          val cmd =
            "pack200" +
            " --log-file=" + tempLog.getAbsolutePath +
            " --deflate-hint=true" +
            " --effort=9" +
            " " + jar + ".pack.gz" +
            " " + jar
          require((cmd !) == 0, "Failed to compress jars.")
        }
        
        
        println("Copying extra files...")
        
        IO.copyFile(base / "simplex3d-console.jnlp", target / "simplex3d-console.jnlp")
        IO.copyFile(base / "simplex3d-icon.png", target / "simplex3d-icon.png")
        
        
        println("Done.")
        
        jar.delete()
        jar
      }
    )
  ) dependsOn(core)
}


import java.io.{ File => IoFile, _ }
import java.math.BigInteger
import java.security.MessageDigest
import java.util.zip._
import java.util.regex._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap


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


object Indexer {
  def index(
    timeStamp: File,
    depsIndex: File, deps: Seq[FileSet],
    examplesIndex: File, examples: Seq[FileSet]
  ) {
    println("Indexing files...")

    Util.writeFile(timeStamp, (System.currentTimeMillis / 1000).toString)
    Util.writeFile(depsIndex, makeIndex(deps))
    Util.writeFile(examplesIndex, makeIndex(examples))

    println("Indexing complete.")
  }

  private def makeIndex(files: Seq[FileSet]) :String = {
    val index = ListBuffer[String]()
    
    for (fileSet <- files) {
      fileSet.foreach { (path, openStream) =>
        index += path
      }
    }
    
    index.toList.sorted.mkString("\n")
  }
}
