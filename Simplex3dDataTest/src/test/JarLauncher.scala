/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test


/**
 * @author Aleksey Nikiforov (lex)
 */
object JarLauncher {
  def findJar(clazz: Class[_]) = {
    val resource = clazz.getName.replace('.', '/') + ".class"
    val path = this.getClass.getClassLoader.getResource(resource).getPath
    val pathEnd = path.lastIndexOf('!')

    if (!path.startsWith("file:") || pathEnd < 0)
      throw new IllegalArgumentException("The specified class is not in a jar.")

    path.substring(5, pathEnd)
  }

  def main(args: Array[String]) {
    
    org.scalatest.tools.Runner.main(Array[String](
      "-p",
      findJar(this.getClass),
      "-gNHL"
    ))
  }
}
