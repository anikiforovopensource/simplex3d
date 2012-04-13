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
  
  val buildSettings = Common.buildSettings ++ Seq(
    version := Simplex3d.ConsoleVersion,
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
      val ap = f.src.getAbsolutePath.replace('\\', '/')
      if (ap.contains("/example/")) false
      else (
        ap.contains("/target/math/") ||
        ap.contains("/target/data/") ||
        ap.contains("/target/algorithm/") ||
        ap.contains("/target/engine/") ||
        ap.contains("/target/script")
      )
    }
    
    val (unfilteredExamples, rest3) = rest2.partition{ f =>
      val ap = f.src.getAbsolutePath.replace('\\', '/')
      ap.contains("/example/")
    }
    
    val (mainFiles, otherDeps) = rest3.partition{ f =>
      val ap = f.src.getAbsolutePath.replace('\\', '/')
      ap.contains("/target/console/") ||
      ap.contains("/Simplex3dConsole/")
    }
    
    val exampleFiles = unfilteredExamples.map(fs => new FileSet(fs.src, List(""".*\.scala""")))
    (scalaFiles, simplexFiles, exampleFiles, mainFiles, otherDeps)
  }
  
  
  lazy val root = core
  
  lazy val core = Project(
    id = "console",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Common.lwjglSettings ++ Seq(
      target := new File("target/console"),
      mainClass := Some("simplex3d.console.ConsoleFrame"),
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
      unmanagedJars in Compile <<= baseDirectory map { base => (base / "lib" ** "*.jar").classpath },
      scalaSource in Compile <<= baseDirectory(_ / "src"),
      
      compile in Compile <<= (scalaSource in Compile, classDirectory in Compile, dependencyClasspath in Compile, compile in Compile) map { (src, classes, cp, compileRes) =>
        val (scalaFiles, simplexFiles, exampleFiles, mainFiles, otherDeps) = extractFileSets(classes, cp)
        Indexer.index(
          classes / "simplex3d/console/deps.version",
          classes / "simplex3d/console/deps.index", scalaFiles ++ simplexFiles,
          classes / "simplex3d/console/examples.index", exampleFiles
        )
        
        IO.copyFile(src / "simplex3d/console/simplex3d-logo.png", classes / "simplex3d/console/simplex3d-logo.png")
        
        compileRes
      }
    )
  ) dependsOn(
    Simplex3dScript.core, Simplex3dScript.example,
    Simplex3dMath.example, Simplex3dData.example, Simplex3dAlgorithm.example, Simplex3dEngine.example
  )
  
  
  lazy val webstart = Project(
    id = "console-webstart",
    base = file("Simplex3dConsole"),
    settings = buildSettings ++ Seq(
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
      
      packageBin in Compile <<= (
        baseDirectory, target,
        classDirectory in Compile, dependencyClasspath in Compile, packageBin in Compile,
        ivyPaths
      ) map {
        (base, target, classes, cp, jar, ivyPaths) =>
        
        require(!keystoreAlias.isEmpty && !keystorePass.isEmpty, "Run keystore-credentials first.")
        
        val (scalaFiles, simplexFiles, exampleFiles, mainFiles, otherDeps) = extractFileSets(classes, cp)
        
        val tempLog = java.io.File.createTempFile("pack200", ".log")
        tempLog.deleteOnExit()
      
      
        println("Copying lwjgl jars...")
        
        //val lwjglNativeJars = Common.getLwjglNativeJars(ivyPaths.ivyHome.get)
        //for (jar <- lwjglNativeJars) { IO.copyFile(jar, target / jar.getName) }
        
        val otherJars = otherDeps.map(_.src).filter(file => !file.isDirectory && file.getName.endsWith(".jar"))
        val lwjglJars = otherJars.filter(_.getAbsolutePath.contains("lwjgl"))
        val copiedLwjglJars = for (jar <- lwjglJars) yield {
          val copy = target / jar.getName.replace("-" + Common.lwjglVersion, "")
          IO.copyFile(jar, copy)
          copy
        }
        
        
        println("Building web-start jars...")
        
        val scalaDeps = new File(target, "console-ws-scala-deps.jar")
        val simplexDeps = new File(target, "console-ws-simplex3d-deps.jar")
        val main = new File(target, "console-ws-main.jar")
        
        Jar.create(scalaDeps, scalaFiles)
        Jar.create(simplexDeps, simplexFiles)
        Jar.create(main, mainFiles ++ exampleFiles)
        
        val jars = copiedLwjglJars ++ List(scalaDeps.getAbsolutePath, simplexDeps.getAbsolutePath, main.getAbsolutePath)
        
        
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


import scala.collection.mutable.ListBuffer


object Indexer {
  def index(
    timeStamp: File,
    depsIndex: File, deps: Seq[FileSet],
    examplesIndex: File, examples: Seq[FileSet]
  ) {
    println("console: indexing jars...")

    Util.writeFile(timeStamp, (System.currentTimeMillis / 1000).toString)
    Util.writeFile(depsIndex, makeIndex(deps))
    Util.writeFile(examplesIndex, makeIndex(examples))

    println("console: indexing complete.")
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
