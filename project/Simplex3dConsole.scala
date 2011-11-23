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

  val packageWebstart = TaskKey[Unit]("package-webstart", "Generates all the files necessary for webstart deployment.")
  
  
  val coreFilter = new WorkingFilter("src/simplex3d/console/.*")
  val extensionFilter = new WorkingFilter("src/simplex3d/console/extension/.*")
  val exampleFilter = new WorkingFilter("src/example/.*")
  
  lazy val root = Project(
    id = "console",
    base = file("."),
    settings = buildSettings ++ Seq (
      target := new File("target/console")
    )
  ) aggregate(core, extension, example)
  
  lazy val core = Project(
    id = "console-core",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Seq (
      target := new File("target/console/core"),
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
      unmanagedJars in Compile <<= baseDirectory map { base => (base / "lib" ** "*.jar").classpath },
      includeFilter := coreFilter && Simplex3d.codeFilter,
      excludeFilter := extensionFilter
    )
  )
  
  lazy val extension: Project = Project(
    id = "console-extnsion",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Seq (
      target := new File("target/console/extension"),
      includeFilter := extensionFilter && Simplex3d.codeFilter
    )
  ) dependsOn(
    core,
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double, Simplex3dData.format
  )
  
  lazy val example = Project(
    id = "console-example",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Seq (
      target := new File("target/console/example"),
      includeFilter := exampleFilter && Simplex3d.codeFilter,
      packageBin in Compile <<= (scalaSource in Compile, packageBin in Compile) map { (src, dest) => //XXX also copy sources int /classes dir
        Jar.create(dest, new FileSet(src, List("""example/.*\.scala""")) :: Nil)
        dest
      }
    )
  ) dependsOn(
    extension,
    Simplex3dMath.core, Simplex3dMath.double,
    Simplex3dData.core, Simplex3dData.double, Simplex3dData.format,
    Simplex3dAlgorithm.intersection, Simplex3dAlgorithm.mesh, Simplex3dAlgorithm.noise
  )
  
  
  val keystore = InputKey[Unit]("keystore")
  var keystoreAlias = ""
  var keystorePass = ""
  
  lazy val webstart = Project(
    id = "console-webstart",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Seq (
      target := new File("target/console/webstart"),
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-library" % _),
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
      unmanagedJars in Compile <<= baseDirectory map { base => (base / "lib" ** "*.jar").classpath },
      excludeFilter := "*",
      keystore <<= inputTask { argTask =>
        argTask map { args =>
          require(args.length == 2, "Must enter alias and pass separated by space.")
          keystoreAlias = args(0)
          keystorePass = args(1)
        }
      },
      packageBin in Compile <<= (
        baseDirectory, target,
        dependencyClasspath in Compile,
        packageBin in Compile
      ) map {
        (base, target, cp, jar) =>
      
      
        println("Building web-start jars...")
        
        val fileSets = cp.map(a => new FileSet(a.data))

        val (scalaSet, rest1) = fileSets.partition(_.src.getName.startsWith("scala-"))
        val (simplexSet, rest2) = rest1.partition { f =>
          val ap = f.src.getAbsolutePath
          ap.contains("/target/math/") ||
          ap.contains("/target/data/") ||
          ap.contains("/target/algorithm/") ||
          ap.contains("/target/console/extension")
        }
        
        val mainSet =
          new FileSet(new File(base, "src"), List("""example/.*\.scala""")) ::
          rest2.filter(!_.src.getAbsolutePath.contains("/target/console/example/")).toList
        
        val scalaDeps = new File(target, "console-ws-scala-deps.jar")
        val simplexDeps = new File(target, "console-ws-simplex3d-deps.jar")
        val main = new File(target, "console-ws-main.jar")
        
        Jar.create(scalaDeps, scalaSet)
        Jar.create(simplexDeps, simplexSet)
        Jar.create(main, mainSet)
        val jars = scalaDeps.getAbsolutePath :: simplexDeps.getAbsolutePath :: main.getAbsolutePath :: Nil
        
        val keystore = new File(base, "keystore.local")
        
        val tempLog = java.io.File.createTempFile("pack200", ".log")
        tempLog.deleteOnExit()
        
        
        println("Repacking web-start jars...")
        
        for (jar <- jars) { ("pack200 --repack --log-file=" + tempLog.getAbsolutePath + " " + jar) ! }
        
        
        println("Signing web-start jars...")
        
        for (jar <- jars) {
          val cmd =
            "jarsigner" +
            " -keystore " + keystore +
            " -storepass " + keystorePass +
            " -keypass " + keystorePass +
            " " + jar +
            " " + keystoreAlias
          cmd !
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
          cmd !
        }
        
        
        println("Copying extra files...")
        
        IO.copyFile(base / "simplex3d-console.jnlp", target / "simplex3d-console.jnlp")
        IO.copyFile(base / "simplex3d-console.jnlp", target / "simplex3d-console.jnlp")
        IO.copyFile(base / "simplex3d-icon.png", target / "simplex3d-icon.png")
        
        
        println("Done.")
        
        jar.delete()
        jar
      }
    )
  ) dependsOn(core, extension, example)
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


object Jar {
  val DefaultManifest = "Manifest-Version: 1.0\n\n"
  
  private def loadFile(file: File) :String = {
    val source = scala.io.Source.fromFile(file)
    val res = source.mkString
    source.close ()
    res
  }
  
  private final val buff = new Array[Byte](1024)
  private def copy(src: InputStream, dest: OutputStream) {
    var read = 0; while (read >= 0) {
      dest.write(buff, 0, read)
      read = src.read(buff)
    }
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
          copy(openStream(), zip)
        }
      }
    }
    finally {
      if (zip != null) zip.close()
    }
  }
}


object Indexer {
  val DepsJars = List("build/scala-deps.jar", "build/simplex3d-deps.jar")

  def main(args: Array[String]) {
    println("Indexing files...")

    val depsIndices = for (jar <- DepsJars) yield { makeJarIndex(jar) }
    val depsIndex = combineIndices(depsIndices)
    val depsSum = makeSum(DepsJars, depsIndices)
    val exampleIndex = makeExampleIndex().mkString("\n")

    writeFile("build/classes/simplex3d/console/deps.index", depsIndex)
    writeFile("build/classes/simplex3d/console/deps.sum", depsSum)
    writeFile("build/classes/simplex3d/console/examples.index", exampleIndex)

    println("Indexing complete.")
  }

  def writeFile(file: String, contents: String) {
    val out = new OutputStreamWriter(new FileOutputStream(file))
    out.write(contents)
    out.close()
  }

  private def makeExampleIndex() :List[String] = {
    val baseDir = "src/example/"
    val res = ListBuffer[String]()

    def makeExampleIndexRec(dir: File, res: ListBuffer[String]) {
      val entries = dir.listFiles
      for (entry <- entries) {
        if (entry.isDirectory) makeExampleIndexRec(entry, res)
        else if (entry.getName.endsWith(".scala")) res += entry.getPath.replace(baseDir, "")
      }
    }

    makeExampleIndexRec(new File(baseDir), res)
    res.toList.sorted
  }

  private def combineIndices(indices: Seq[List[String]]) :String = {
    indices.flatten.sorted.mkString("\n")
  }

  private def makeJarIndex(jar: String) :List[String] = {
    val zipStream = new ZipInputStream(new FileInputStream(jar))

    var names = List[String]();
    var entry = zipStream.getNextEntry()

    while (entry != null) {
      if (!entry.isDirectory && !entry.getName.startsWith("META-INF")) names = entry.getName :: names
      entry = zipStream.getNextEntry()
    }

    zipStream.close()
    names.sorted
  }

  private def makeSum(jars: List[String], jarIndices: Seq[List[String]]) :String = {
    val digest = MessageDigest.getInstance("MD5")

    for ((jar, index) <- jars.zip(jarIndices)) {
      jarSum(digest, jar, index)
    }

    val sum = digest.digest()
    new BigInteger(sum).abs.toString(16).toUpperCase
  }

  private def jarSum(digest: MessageDigest, jar: String, index: List[String]) {
    val fileMap = new HashMap[String, Array[Byte]]()

    val zipStream = new ZipInputStream(new FileInputStream(jar))
    val buff = new Array[Byte](1024*8)

    var entry = zipStream.getNextEntry(); while (entry != null) {
      if (!entry.isDirectory) {
        val out = new ByteArrayOutputStream()

        var len = 0; while (len >= 0) {
          len = zipStream.read(buff)
          if (len > 0) out.write(buff, 0, len)
        }

        fileMap.put(entry.getName, out.toByteArray)
      }
      entry = zipStream.getNextEntry()
    }
    zipStream.close()

    var count = 0
    for (path <- index) {
      val data = fileMap.get(path).get
      count += data.length
      digest.update(data)
    }
    println(jar + " size: " + count)
  }
}
