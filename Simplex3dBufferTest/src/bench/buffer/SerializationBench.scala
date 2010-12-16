/*
 * Simplex3d, BufferTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBufferTest.
 *
 * Simplex3dBufferTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBufferTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package bench.buffer

import java.io._
import java.nio._
import simplex3d.math._
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._
import simplex3d.buffer._
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SerializationBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val size = 10000
  val loops = 10000

  val random = new java.util.Random()

  val dataArray = DataArray[RFloat, RFloat](size)
  var i = 0; while (i < size) {
    dataArray(i) = random.nextFloat()
    i += 1
  }

  val dataBuffer = dataArray.copyAsDataBuffer()

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testWriteArray(dataArray, loops)
    System.gc()
    val writeArrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testReadArray(dataArray, loops)
    System.gc()
    val readArrayTime = System.currentTimeMillis - start


    println("\nResults:")
    println("Write array time: " + writeArrayTime + ".")
    println("Read array time: " + readArrayTime + ".")
  }

  final def testWriteArray(data: DataArray[RFloat, RFloat], loops: Int) {
    var a = 0

    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)

    var l = 0; while (l < loops) {

      out.writeObject(data)
      out.close()

      a += bytes.size
      bytes.reset

      l += 1
    }

    println(a)
  }

  final def testReadArray(data: DataArray[RFloat, RFloat], loops: Int) {
    var a = 0

    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)
    out.writeObject(data)
    out.close()
    val src = bytes.toByteArray

    var l = 0; while (l < loops) {
      val in = new ObjectInputStream(new ByteArrayInputStream(src))
      val data = in.readObject().asInstanceOf[DataArray[RFloat, RFloat]]
      in.close()

      a += data.size

      l += 1
    }

    println(a)
  }
}
