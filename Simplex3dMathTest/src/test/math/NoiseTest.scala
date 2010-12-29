/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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

package test.math

import org.scalatest._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class NoiseTest extends FunSuite {

  test("1d noise") {
    val maxDerivative = 8

    val from = 0.0
    val to = 10.0
    val delta = 1e-3
    val invdelta = 1/delta

    var x = from; while(x < to) {

      val n = noise1(x)
      val d = (n - noise1(x - delta))*invdelta

      assert(1 - abs(n) >= 0)
      assert(length(d) < maxDerivative)

      x += delta
    }
  }

  test("2d noise") {
    val maxDerivLenSquare = 8*8

    val from = 0.0
    val to = 10.0
    val delta = 1e-2
    val invdelta = 1/delta

    var x = from; while(x < to) {
      var y = from; while(y < to) {

        val n = noise1(Vec2(x, y))
        val d = Vec2(
          (n - noise1(Vec2(x - delta, y)))*invdelta,
          (n - noise1(Vec2(x, y - delta)))*invdelta
        )


        assert(1 - abs(n) >= 0)
        assert(dot(d, d) < maxDerivLenSquare)

        y += delta
      }

      x += delta
    }
  }

  test("3d noise") {
    val maxDerivLenSquare = 8*8

    val from = 0.0
    val to = 2.0
    val delta = 1e-2
    val invdelta = 1/delta

    val loopCount = toInt((to - from)*invdelta)

    var x = from; while(x < to) {
      var y = from; while(y < to) {
        var z = from; while(z < to) {

          val n = noise1(Vec3(x, y, z))
          val d = Vec3(
            (n - noise1(Vec3(x - delta, y, z)))*invdelta,
            (n - noise1(Vec3(x, y - delta, z)))*invdelta,
            (n - noise1(Vec3(x, y, z - delta)))*invdelta
          )


          assert(1 - abs(n) >= 0)
          assert(dot(d, d) < maxDerivLenSquare)

          z += delta
        }

        y += delta
      }

      x += delta
    }
  }

  test("4d noise") {
    val maxDerivLenSquare = 8*8

    val from = 0.0
    val to = 0.5
    val delta = 1e-2
    val invdelta = 1/delta

    var x = from; while(x < to) {
      var y = from; while(y < to) {
        var z = from; while(z < to) {
          var w = from; while(w < to) {

            val n = noise1(Vec4(x, y, z, w))
            val d = Vec4(
              (n - noise1(Vec4(x - delta, y, z, w)))*invdelta,
              (n - noise1(Vec4(x, y - delta, z, w)))*invdelta,
              (n - noise1(Vec4(x, y, z - delta, w)))*invdelta,
              (n - noise1(Vec4(x, y, z, w - delta)))*invdelta
            )

            assert(1 - abs(n) >= 0)
            assert(dot(d, d) < maxDerivLenSquare)

            w += delta
          }

          z += delta
        }

        y += delta
      }

      x += delta
    }
  }
}
