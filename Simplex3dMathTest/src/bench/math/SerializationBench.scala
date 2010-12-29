/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010, Simplex3d Team
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package bench.math

import java.io._
import simplex3d.math.floatx._
import simplex3d.math.doublex._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SerializationBench {

  def main(args: Array[String]) {
    serializationInfo("Vec3f", () => Vec3f(1))
    serializationInfo("Vec4f", () => Vec4f(2))
    serializationInfo("Vec4d", () => Vec4d(3))

    test()
    test()
    test()
    test()
  }

  def serializationInfo(name: String, objectFactory: () => AnyRef) {
    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)

    out.writeObject(objectFactory())
    val singleSize = bytes.size
    out.writeObject(objectFactory())
    val increment = bytes.size - singleSize
    out.writeObject(objectFactory())
    assert(bytes.size - increment*2 == singleSize)

    println("Size of one '" + name + "' is " + singleSize + " bytes.")
    println("Size of each additional '" + name + "' is " + increment + " bytes.")
  }

  val objectCount = 10000
  val repeats = 100

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testVec3f(objectCount, repeats)
    System.gc()
    val vec3fTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVec4f(objectCount, repeats)
    System.gc()
    val vec4fTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVec4d(objectCount, repeats)
    System.gc()
    val vec4dTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testC4d(objectCount, repeats)
    System.gc()
    val c4dTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Vec3f time: " + vec3fTime + ".")
    println("Vec4f time: " + vec4fTime + ".")
    println("Vec4d time: " + vec4dTime + ".")
    println("4 doubles time: " + c4dTime + ".")
  }

  final def testVec3f(objectCount: Int, repeats: Int) {
    var a = 0

    var r = 0; while (r < repeats) {
      val bytes = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytes)

      var i = 0; while (i < objectCount) {
        out.writeObject(Vec3f(i, i + 1, i + 2))

        i += 1
      }

      out.close()
      a += bytes.size

      r += 1
    }

    println(a)
  }

  final def testVec4f(objectCount: Int, repeats: Int) {
    var a = 0

    var r = 0; while (r < repeats) {
      val bytes = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytes)

      var i = 0; while (i < objectCount) {
        out.writeObject(Vec4f(i, i + 1, i + 2, i + 3))

        i += 1
      }

      out.close()
      a += bytes.size

      r += 1
    }

    println(a)
  }

  final def testVec4d(objectCount: Int, repeats: Int) {
    var a = 0

    var r = 0; while (r < repeats) {
      val bytes = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytes)

      var i = 0; while (i < objectCount) {
        out.writeObject(Vec4d(i, i + 1, i + 2, i + 3))

        i += 1
      }

      out.close()
      a += bytes.size

      r += 1
    }

    println(a)
  }

  final def testC4d(objectCount: Int, repeats: Int) {
    var a = 0

    var r = 0; while (r < repeats) {
      val bytes = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytes)

      var i = 0; while (i < objectCount) {
        out.writeDouble(i)
        out.writeDouble(i + 1)
        out.writeDouble(i + 2)
        out.writeDouble(i + 3)

        i += 1
      }

      out.close()
      a += bytes.size

      r += 1
    }

    println(a)
  }
}
