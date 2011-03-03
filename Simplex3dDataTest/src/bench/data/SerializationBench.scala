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

package bench.data

import java.io._
import java.nio._
import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.floatx.functions._
import simplex3d.data._
import simplex3d.data.float._


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

  val primitive = DataArray[RFloat, RFloat](size)
  var i = 0; while (i < size) {
    primitive(i) = random.nextFloat()
    i += 1
  }

  val composite = DataArray[Vec4, RFloat](primitive)

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testWritePrimitive(primitive, loops)
    System.gc()
    val timeWritePrimitive = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testReadPrimitive(primitive, loops)
    System.gc()
    val timeReadPrimitive = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testWriteComposite(composite, loops)
    System.gc()
    val timeWriteComposite = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testReadComposite(composite, loops)
    System.gc()
    val timeReadComposite = System.currentTimeMillis - start


    println("\nResults:")
    println("Write Primitive time: " + timeWritePrimitive + ".")
    println("Read Primitive time: " + timeReadPrimitive + ".")
    println("Write Composite time: " + timeWriteComposite + ".")
    println("Read Composite time: " + timeReadComposite + ".")
  }

  final def testWritePrimitive(data: DataArray[RFloat, Raw], loops: Int) {
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

  final def testReadPrimitive(data: DataArray[RFloat, Raw], loops: Int) {
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

  final def testWriteComposite(data: DataArray[Vec4, Raw], loops: Int) {
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

  final def testReadComposite(data: DataArray[Vec4, Raw], loops: Int) {
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
