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
import java.security.Policy
import java.util.prefs.Preferences
import java.util.zip._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Utils {

  def resolveDeps() :String = {
    val prefs = Preferences.userNodeForPackage(this.getClass)
    val depsJar = prefs.get("deps.jar", null)

    if (depsJar != null && new File(depsJar).exists) {
      val depsSum = prefs.get("deps.sum", "undefined")
      val curSum = {
        val is = this.getClass.getClassLoader.getResourceAsStream("simplex3d/console/deps.sum")
        scala.io.Source.fromInputStream(is).getLines().next
      }
      if (depsSum == curSum) return depsJar
      else prefs.put("deps.sum", curSum)
    }

    val newJar = rebuildDeps()
    prefs.put("deps.jar", newJar)
    prefs.flush()
    newJar
  }

  private[this] def rebuildDeps() :String = {
    val is = this.getClass.getClassLoader.getResourceAsStream("simplex3d/console/deps.index")
    val index = scala.io.Source.fromInputStream(is).getLines().toList
    is.close()

    val depsFile = File.createTempFile("simplex3d-console-deps", ".jar")

    val buff = new Array[Byte](1024*8)
    val zipOut = new java.util.zip.ZipOutputStream(new FileOutputStream(depsFile))
    zipOut.setLevel(1)

    for (path <- index) {
      zipOut.putNextEntry(new ZipEntry(path))
      val fileIn = this.getClass.getClassLoader.getResourceAsStream(path)

      var len = 0; while (len >= 0) {
        len = fileIn.read(buff)
        if (len > 0) zipOut.write(buff, 0, len)
      }
      fileIn.close()
      zipOut.closeEntry()
    }
    zipOut.close()

    depsFile.getAbsolutePath
  }

  def redirectSystemOut() :AccumPrintStream = {
    if (!System.out.isInstanceOf[AccumPrintStream]) {
      System.setOut(new AccumPrintStream())
    }
    System.out.asInstanceOf[AccumPrintStream]
  }

  def setSandboxEnabled(enabled: Boolean) {
    if (enabled) {
      Policy.setPolicy(new InterpretedPolicy)
      System.setSecurityManager(new SecurityManager)
    }
    else {
      System.setSecurityManager(null)
      Policy.setPolicy(null)
    }
  }
}
