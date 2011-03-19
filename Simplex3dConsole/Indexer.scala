/*
 * Simplex3dConsole
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dConsole.
 *
 * Simplex3dConsole is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dConsole is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


import java.io._
import java.math.BigInteger
import java.security.MessageDigest
import java.util.zip._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap


/**
 * @author Aleksey Nikiforov (lex)
 */
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
    val baseDir = "src/simplex3d/console/example/"
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

Indexer.main(null)
