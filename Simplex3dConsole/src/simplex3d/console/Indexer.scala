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

package simplex3d.console

import java.io._
import java.math.BigInteger
import java.security.MessageDigest
import java.util.zip._
import scala.collection.mutable.ListBuffer


/**
 * @author Aleksey Nikiforov (lex)
 */
object Indexer {

  // This stuff belongs in the build script.
  def main(args: Array[String]) {
    val index = makeDepsIndex()
    writeFile("src/simplex3d/console/deps.index", index.mkString("\n"))
    writeFile("src/simplex3d/console/deps.sum", makeSum(index))
    writeFile("src/simplex3d/console/examples.index", makeExampleIndex().mkString("\n"))
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

  private def makeDepsIndex() :List[String] = {
    val zipStream = new ZipInputStream(new FileInputStream("release/web/simplex3d-console-deps.jar"))

    var names = List[String]();
    var entry = zipStream.getNextEntry()

    while (entry != null) {
      if (!entry.isDirectory && !entry.getName.startsWith("META-INF")) names = entry.getName :: names
      entry = zipStream.getNextEntry()
    }

    names.sorted
  }

  private def makeSum(index: List[String]) :String = {
    val buff = new Array[Byte](1024*8)
    val digest = MessageDigest.getInstance("MD5")

    for (path <- index) {
      val fileIn = this.getClass.getClassLoader.getResourceAsStream(path)

      var len = 0; while (len >= 0) {
        len = fileIn.read(buff)
        if (len > 0) digest.update(buff, 0, len)
      }
      fileIn.close()
    }

    val sum = digest.digest()
    new BigInteger(sum).abs.toString(16).toUpperCase
  }
}
