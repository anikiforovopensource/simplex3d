#!/bin/sh
exec scala "$0" "$@"
!#

// Temporary release script, until scalac can run with less ram.

import java.io._
import scala.collection.mutable.ArrayBuffer

def exec(args: Seq[String], env: Seq[String]) {
  val proc =
    if (env == null || env.size == 0) Runtime.getRuntime.exec(args.toArray)
    else Runtime.getRuntime.exec(args.toArray, env.toArray)

  val is = new BufferedReader(new InputStreamReader(proc.getInputStream))
  val es = new BufferedReader(new InputStreamReader(proc.getErrorStream))

  var line = is.readLine(); while (line != null) {
    println(line)
    line = is.readLine()
  }
  line = es.readLine(); while (line != null) {
    System.err.println(line)
    line = es.readLine()
  }

  val exitCode = proc.waitFor
  if (exitCode != 0) System.exit(exitCode)
}
def silent(args: String*) {
  val proc = Runtime.getRuntime.exec(args.toArray)
  val is = new BufferedReader(new InputStreamReader(proc.getInputStream))
  val es = new BufferedReader(new InputStreamReader(proc.getErrorStream))

  val exitCode = proc.waitFor
  if (exitCode != 0) System.exit(exitCode)
}

def listFilesHelper(
  dir: String, exclude: Seq[String], result: ArrayBuffer[String]
) {
  val files = new File(dir).listFiles()
  if (files == null) return

  for (file <- files) {
    if (file.isDirectory()) {
      listFilesHelper(file.getPath, exclude, result)
    }
    else if (file.getName.endsWith(".scala") || file.getName.endsWith(".java")){
      if (!exclude.contains(file.getPath)) result += file.getPath
    }
  }
}
def listFiles(dir: String, exclude: Seq[String]) :Seq[String] = {
  val buff = new ArrayBuffer[String](10)
  listFilesHelper(dir, exclude, buff)
  buff
}
def filterJava(files: Seq[String]) :Seq[String] = {
  for (file <- files; if (file.endsWith(".java"))) yield file
}

val outputdir = "build/classes"
val classpath = "../Simplex3dMath/dist/Simplex3dMath.jar:lib/scalatest-1.4.1.jar:" + outputdir + "/"
def compile(srcfiles: Seq[String]) {
  val scalaEnv =
    "JAVACMD=" + System.getenv("JAVA_HOME") + "/bin/java" ::
    "JAVA_OPTS=-Xss2048k -Xmx1200m" ::
    Nil

  val scalaCmd =
    "scalac" ::
    "-classpath" :: classpath ::
    "-d" :: outputdir ::
    srcfiles.toList

  val javaFiles = filterJava(srcfiles)
  val javaCmd =
    "javac" ::
    "-classpath" :: System.getenv("SCALA_HOME") + "/lib/scala-library.jar:" + classpath ::
    "-d" :: outputdir ::
    javaFiles.toList

  exec(scalaCmd, scalaEnv)
  if (javaFiles.size > 0) exec(javaCmd, Nil)
}

val start = System.currentTimeMillis()

// Prep dirs.
silent("ant", "clean")
(new File(outputdir)).mkdirs()


// Compile dependencies.
val dependencies =
  "src/test/math/BooleanCombinations.scala" +:
  listFiles("src/visual/math/lines", Nil)

println("Compiling dependencies...")
//println(dependencies.mkString("\n"))
compile(dependencies)
println("Done.\n")


// Compile tests.
val compilationSize = 10
val classes = listFiles("src", dependencies)
val total = classes.size
val groups = classes.grouped(compilationSize)
println("Compiling tests...")
var count = 0
for (group <- groups) {
  compile(group)
  count += group.size
  println("Compiled " + count + " out of " + total + ".")
}
println("Done.")
println("\nCompilation successful.")
val time = (System.currentTimeMillis() - start)/1000
val minutesStr = if (time >= 60) (time / 60) + " minutes, " else ""
println("Total time: " + minutesStr + (time % 60) + " seconds.")
