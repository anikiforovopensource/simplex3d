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
import java.util.zip._


/**
 * @author Aleksey Nikiforov (lex)
 */
object MakeIndex {
  def main(args: Array[String]) {
    val zipStream = new ZipInputStream(new FileInputStream("release/web/simplex3d-console-deps.jar"))

    var names = List[String]();
    var entry = zipStream.getNextEntry()

    while (entry != null) {
      if (!entry.isDirectory && !entry.getName.startsWith("META-INF")) names = entry.getName :: names
      entry = zipStream.getNextEntry()
    }

    val out = new OutputStreamWriter(new FileOutputStream("src/simplex3d/console/deps.index"))
    out.write(names.mkString("\n"))
    out.close()
  }
}
