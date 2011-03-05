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
import java.security._
import java.util.zip._
import scala.tools.nsc._


/**
 * @author Aleksey Nikiforov (lex)
 */
class SimplexInterpreter extends SimpleInterpreter {
  interpreter.interpret(
    """
    import simplex3d.math._
    import simplex3d.math.double._
    import simplex3d.math.double.functions._
    import simplex3d.data._
    import simplex3d.data.double._
    """
  )
  flusher.flush()
  out.clear()
}

class SimpleInterpreter {
  protected val out = new AccumPrintStream()
  protected val flusher = new PrintWriter(out)
  System.setOut(out)

  protected val interpreter = {
    val settings = new GenericRunnerSettings(out.println(_))
    settings.usejavacp.value = false
    settings.classpath.value = Util.mkDeps("simplex3d/console/deps.index")
    new Interpreter(settings, flusher)
  }

  Policy.setPolicy(new InterpretedPolicy)
  System.setSecurityManager(new SecurityManager())

  def interpret(code: String) :String = {
    interpreter.interpret(code)
    flusher.flush()
    val res = out.text
    out.clear()
    res
  }
}

object Util {
  def mkDeps(indexPath: String) :String = {
    val is = this.getClass.getClassLoader.getResourceAsStream(indexPath)
    val index = scala.io.Source.fromInputStream(is).getLines()

    val tmpFile = File.createTempFile("simplex3d-console-deps", ".jar")
    tmpFile.deleteOnExit()

    val buff = new Array[Byte](1024*8)
    val zipOut = new java.util.zip.ZipOutputStream(new FileOutputStream(tmpFile))
    zipOut.setLevel(1)

    for (path <- index) {
      zipOut.putNextEntry(new ZipEntry(path))
      val jarIn = this.getClass.getClassLoader.getResourceAsStream(path)

      var len = 0; while (len >= 0) {
        len = jarIn.read(buff)
        if (len > 0) zipOut.write(buff, 0, len)
      }
      zipOut.closeEntry()
    }
    zipOut.close()

    tmpFile.getAbsolutePath
  }
}

class InterpretedPolicy extends Policy {
  private val allPermissions = new Permissions
  allPermissions.add(new AllPermission)
  allPermissions.setReadOnly

  private val noPermissions = new Permissions
  noPermissions.setReadOnly

  override def getPermissions(codesource: CodeSource) = {
    if (codesource.getLocation == null) noPermissions
    else allPermissions
  }

  override def getPermissions(domain: ProtectionDomain) = {
    if (domain.getCodeSource.getLocation == null) noPermissions
    else allPermissions
  }
}

class AccumPrintStream(private[this] val bstream: ByteArrayOutputStream) extends PrintStream(bstream) {
  def this() { this(new ByteArrayOutputStream) }
  def text :String = bstream.toString()
  def clear() { bstream.reset() }
}
