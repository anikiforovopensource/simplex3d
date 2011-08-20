/*
 * Simplex3dMath
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import java.io._


object StripSwizzling {
  
  def main(args: Array[String]) {
    stripCopy("src", "build/sdoc")
  }
  
  def stripCopy(srcPath: String, destPath: String) {
    val src = new File(srcPath)
    val dest = new File(destPath)
    
    rec(src, dest)
  }
  
  def rec(dir: File, out: File) {
    if (!out.exists) out.mkdirs()
    
    for (entry <- dir.listFiles) {
      if (entry.isDirectory) rec(entry, new File(out, entry.getName()))
      else if (entry.getName.endsWith(".java")) copy(entry, out)
      else if (entry.getName.endsWith(".scala")) filter(entry, out)
    }
  }
  
  def copy(file: File, todir: File) {
    val dest = new File(todir, file.getName)
    
    val in = new BufferedInputStream(new FileInputStream(file))
    val out = new BufferedOutputStream(new FileOutputStream(dest))
    
    val buff = new Array[Byte](1024)
    var read = 0; while (read >= 0) {
      out.write(buff, 0, read)
      read = in.read(buff)
    }
    
    in.close()
    out.close()
  }
  
  def filter(file: File, todir: File) {
    val lines = scala.io.Source.fromFile(file).getLines.toArray
    if (lines.find(_.contains("@SwizzlingStart")).isEmpty) copy(file, todir)
    else stripSwizzling(lines, new File(todir, file.getName))
  }
  
  def stripSwizzling(lines: Array[String], dest: File) {
    val out = new BufferedWriter(new FileWriter(dest))
    
    var excludeEnd = 0
    var excludeStart = lines.indexWhere(_.contains("@SwizzlingStart"))
    
    var i = 0; while (i < lines.length) {
      i = excludeEnd; while (i < excludeStart) {
        out.write(lines(i))
        out.write("\n")
      
        i += 1
      }
      excludeEnd = lines.indexWhere(_.contains("@SwizzlingEnd"), excludeStart) + 1
      excludeStart = lines.indexWhere(_.contains("@SwizzlingStart"), excludeEnd)
      if (excludeStart == -1) excludeStart = lines.length
    }
    
    out.close()
  }
}
