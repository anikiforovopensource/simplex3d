/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
import simplex3d.math.doublex._
import simplex3d.math.floatx._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseTestUtil extends FunSuite {

  private[this] val random = new java.util.Random(0)
  private def randomFloat = random.nextFloat
  private def randomVec2f = Vec2f(random.nextFloat, random.nextFloat)
  private def randomVec3f = Vec3f(random.nextFloat, random.nextFloat, random.nextFloat)
  private def randomVec4f = Vec3f(random.nextFloat, random.nextFloat, random.nextFloat, random.nextFloat)
  private def randomDouble = random.nextDouble
  private def randomVec2d = Vec2f(random.nextDouble, random.nextDouble)
  private def randomVec3d = Vec3f(random.nextDouble, random.nextDouble, random.nextDouble)
  private def randomVec4d = Vec3f(random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble)


  // Double.
  def test1dNoise(min: Double, max: Double, noise: (Double) => Double) {
    import simplex3d.math.double.functions._

    val maxDerivative = 8

    val from = 0.0
    val to = 10.0
    val delta = 1e-3
    val invdelta = 1/delta

    var x = from; while(x < to) {

      val n = noise(x)
      val d = (n - noise(x - delta))*invdelta

      if (min < max) assert(min <= n && n <= max)
      assert(length(d) < maxDerivative)

      x += delta
    }
  }

  def test2dNoise(min: Double, max: Double, noise: (inVec2d) => Double) {
    import simplex3d.math.double.functions._

    val maxDerivLenSquare = 8*8

    val from = 0.0
    val to = 10.0
    val delta = 1e-2
    val invdelta = 1/delta

    var x = from; while(x < to) {
      var y = from; while(y < to) {

        val n = noise1(Vec2d(x, y))
        val d = Vec2d(
          (n - noise1(Vec2d(x - delta, y)))*invdelta,
          (n - noise1(Vec2d(x, y - delta)))*invdelta
        )


        if (min < max) assert(min <= n && n <= max)
        assert(dot(d, d) < maxDerivLenSquare)

        y += delta
      }

      x += delta
    }
  }

  def test3dNoise(min: Double, max: Double, noise: (inVec3d) => Double) {
    import simplex3d.math.double.functions._

    val maxDerivLenSquare = 8*8

    val from = 0.0
    val to = 2.0
    val delta = 1e-2
    val invdelta = 1/delta

    val loopCount = Int((to - from)*invdelta)

    var x = from; while(x < to) {
      var y = from; while(y < to) {
        var z = from; while(z < to) {

          val n = noise1(Vec3d(x, y, z))
          val d = Vec3d(
            (n - noise1(Vec3d(x - delta, y, z)))*invdelta,
            (n - noise1(Vec3d(x, y - delta, z)))*invdelta,
            (n - noise1(Vec3d(x, y, z - delta)))*invdelta
          )


          if (min < max) assert(min <= n && n <= max)
          assert(dot(d, d) < maxDerivLenSquare)

          z += delta
        }

        y += delta
      }

      x += delta
    }
  }

  def test4dNoise(min: Double, max: Double, noise: (inVec4d) => Double) {
    import simplex3d.math.double.functions._

    val maxDerivLenSquare = 8*8

    val from = 0.0
    val to = 0.5
    val delta = 1e-2
    val invdelta = 1/delta

    var x = from; while(x < to) {
      var y = from; while(y < to) {
        var z = from; while(z < to) {
          var w = from; while(w < to) {

            val n = noise1(Vec4d(x, y, z, w))
            val d = Vec4d(
              (n - noise1(Vec4d(x - delta, y, z, w)))*invdelta,
              (n - noise1(Vec4d(x, y - delta, z, w)))*invdelta,
              (n - noise1(Vec4d(x, y, z - delta, w)))*invdelta,
              (n - noise1(Vec4d(x, y, z, w - delta)))*invdelta
            )

            if (min < max) assert(min <= n && n <= max)
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

  // Tiled double.
  def test1dTiles(tile: Double, noise: (Double) => Double) {
    import simplex3d.math.double.functions._
    
    
  }

  def test2dTiles(tile: inVec2d, noise: (inVec2d) => Double) {
    import simplex3d.math.double.functions._

    
  }

  def test3dTiles(tile: inVec3d, noise: (inVec3d) => Double) {
    import simplex3d.math.double.functions._

    
  }

  def test4dTiles(tile: inVec4d, noise: (inVec4d) => Double) {
    import simplex3d.math.double.functions._

    
  }

  // Float.
  def test1fNoise(min: Float, max: Float, noise: (Float) => Float) {
    import simplex3d.math.float.functions._

    val maxDerivative = 8

    val from = 0.0f
    val to = 10.0f
    val delta = 1e-3f
    val invdelta = 1/delta

    var x = from; while(x < to) {

      val n = noise(x)
      val d = (n - noise(x - delta))*invdelta

      if (min < max) assert(min <= n && n <= max)
      assert(length(d) < maxDerivative)

      x += delta
    }
  }

  def test2fNoise(min: Float, max: Float, noise: (inVec2f) => Float) {
    import simplex3d.math.float.functions._

    val maxDerivLenSquare = 8*8

    val from = 0.0f
    val to = 10.0f
    val delta = 1e-2f
    val invdelta = 1/delta

    var x = from; while(x < to) {
      var y = from; while(y < to) {

        val n = noise1(Vec2f(x, y))
        val d = Vec2f(
          (n - noise1(Vec2f(x - delta, y)))*invdelta,
          (n - noise1(Vec2f(x, y - delta)))*invdelta
        )


        if (min < max) assert(min <= n && n <= max)
        assert(dot(d, d) < maxDerivLenSquare)

        y += delta
      }

      x += delta
    }
  }

  def test3fNoise(min: Float, max: Float, noise: (inVec3f) => Float) {
    import simplex3d.math.float.functions._

    val maxDerivLenSquare = 8*8

    val from = 0.0f
    val to = 2.0f
    val delta = 1e-2f
    val invdelta = 1/delta

    val loopCount = Int((to - from)*invdelta)

    var x = from; while(x < to) {
      var y = from; while(y < to) {
        var z = from; while(z < to) {

          val n = noise1(Vec3f(x, y, z))
          val d = Vec3f(
            (n - noise1(Vec3f(x - delta, y, z)))*invdelta,
            (n - noise1(Vec3f(x, y - delta, z)))*invdelta,
            (n - noise1(Vec3f(x, y, z - delta)))*invdelta
          )


          if (min < max) assert(min <= n && n <= max)
          assert(dot(d, d) < maxDerivLenSquare)

          z += delta
        }

        y += delta
      }

      x += delta
    }
  }

  def test4fNoise(min: Float, max: Float, noise: (inVec4f) => Float) {
    import simplex3d.math.float.functions._
    
    val maxDerivLenSquare = 8*8

    val from = 0.0f
    val to = 0.5f
    val delta = 1e-2f
    val invdelta = 1/delta

    var x = from; while(x < to) {
      var y = from; while(y < to) {
        var z = from; while(z < to) {
          var w = from; while(w < to) {

            val n = noise1(Vec4f(x, y, z, w))
            val d = Vec4f(
              (n - noise1(Vec4f(x - delta, y, z, w)))*invdelta,
              (n - noise1(Vec4f(x, y - delta, z, w)))*invdelta,
              (n - noise1(Vec4f(x, y, z - delta, w)))*invdelta,
              (n - noise1(Vec4f(x, y, z, w - delta)))*invdelta
            )

            if (min < max) assert(min <= n && n <= max)
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
