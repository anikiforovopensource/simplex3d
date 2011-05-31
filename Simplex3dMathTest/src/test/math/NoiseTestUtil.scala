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
  def randomFloat = random.nextFloat
  def randomVec2f = Vec2f(random.nextFloat, random.nextFloat)
  def randomVec3f = Vec3f(random.nextFloat, random.nextFloat, random.nextFloat)
  def randomVec4f = Vec4f(random.nextFloat, random.nextFloat, random.nextFloat, random.nextFloat)
  def randomDouble = random.nextDouble
  def randomVec2d = Vec2d(random.nextDouble, random.nextDouble)
  def randomVec3d = Vec3d(random.nextDouble, random.nextDouble, random.nextDouble)
  def randomVec4d = Vec4d(random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble)


  // Double.
  def test1dNoise(rangeMin: Double, rangeMax: Double, noise: (Double) => Double) {
    import simplex3d.math.double.functions._

    val rangeMaxDerivative = 8

    val from = 0.0
    val to = 10.0
    val delta = 1e-3
    val invdelta = 1/delta

    var x = from; while(x < to) {

      val n = noise(x)
      val d = (n - noise(x - delta))*invdelta

      if (rangeMin < rangeMax) assert(rangeMin <= n && n <= rangeMax)
      assert(length(d) < rangeMaxDerivative)

      x += delta
    }
  }

  def test2dNoise(rangeMin: Double, rangeMax: Double, noise: (inVec2d) => Double) {
    import simplex3d.math.double.functions._

    val rangeMaxDerivLenSquare = 8*8

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


        if (rangeMin < rangeMax) assert(rangeMin <= n && n <= rangeMax)
        assert(dot(d, d) < rangeMaxDerivLenSquare)

        y += delta
      }

      x += delta
    }
  }

  def test3dNoise(rangeMin: Double, rangeMax: Double, noise: (inVec3d) => Double) {
    import simplex3d.math.double.functions._

    val rangeMaxDerivLenSquare = 8*8

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


          if (rangeMin < rangeMax) assert(rangeMin <= n && n <= rangeMax)
          assert(dot(d, d) < rangeMaxDerivLenSquare)

          z += delta
        }

        y += delta
      }

      x += delta
    }
  }

  def test4dNoise(rangeMin: Double, rangeMax: Double, noise: (inVec4d) => Double) {
    import simplex3d.math.double.functions._

    val rangeMaxDerivLenSquare = 8*8

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

            if (rangeMin < rangeMax) assert(rangeMin <= n && n <= rangeMax)
            assert(dot(d, d) < rangeMaxDerivLenSquare)

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
    
    var i = 0; while (i < 100) {
      val p = randomDouble
      val np = noise(p)

      for (x <- -1 to 1) {
        val t = x
        val nt = noise(t*tile + p)
        assert(approxEqual(np, nt, 1e-13))
      }
      
      i += 1
    }
  }

  def test2dTiles(tile: inVec2d, noise: (inVec2d) => Double) {
    import simplex3d.math.double.functions._
    
    var i = 0; while (i < 100) {
      val p = randomVec2d
      val np = noise(p)

      for (x <- -1 to 1; y <- -1 to 1) {
        val t = Vec2d(x, y)
        val nt = noise(t*tile + p)
        assert(approxEqual(np, nt, 1e-13))
      }
      
      i += 1
    }
  }

  def test3dTiles(tile: inVec3d, noise: (inVec3d) => Double) {
    import simplex3d.math.double.functions._

    var i = 0; while (i < 100) {
      val p = randomVec3d
      val np = noise(p)

      for (x <- -1 to 1; y <- -1 to 1; z <- -1 to 1) {
        val t = Vec3d(x, y, z)
        val nt = noise(t*tile + p)
        assert(approxEqual(np, nt, 1e-13))
      }
      
      i += 1
    }
  }

  def test4dTiles(tile: inVec4d, noise: (inVec4d) => Double) {
    import simplex3d.math.double.functions._

    var i = 0; while (i < 100) {
      val p = randomVec4d
      val np = noise(p)

      for (x <- -1 to 1; y <- -1 to 1; z <- -1 to 1; w <- -1 to 1) {
        val t = Vec4d(x, y, z, w)
        val nt = noise(t*tile + p)
        assert(approxEqual(np, nt, 1e-13))
      }
      
      i += 1
    }
  }

  // Float.
  def test1fNoise(rangeMin: Float, rangeMax: Float, noise: (Float) => Float) {
    import simplex3d.math.float.functions._

    val rangeMaxDerivative = 8

    val from = 0.0f
    val to = 10.0f
    val delta = 1e-3f
    val invdelta = 1/delta

    var x = from; while(x < to) {

      val n = noise(x)
      val d = (n - noise(x - delta))*invdelta

      if (rangeMin < rangeMax) assert(rangeMin <= n && n <= rangeMax)
      assert(length(d) < rangeMaxDerivative)

      x += delta
    }
  }

  def test2fNoise(rangeMin: Float, rangeMax: Float, noise: (inVec2f) => Float) {
    import simplex3d.math.float.functions._

    val rangeMaxDerivLenSquare = 8*8

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


        if (rangeMin < rangeMax) assert(rangeMin <= n && n <= rangeMax)
        assert(dot(d, d) < rangeMaxDerivLenSquare)

        y += delta
      }

      x += delta
    }
  }

  def test3fNoise(rangeMin: Float, rangeMax: Float, noise: (inVec3f) => Float) {
    import simplex3d.math.float.functions._

    val rangeMaxDerivLenSquare = 8*8

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


          if (rangeMin < rangeMax) assert(rangeMin <= n && n <= rangeMax)
          assert(dot(d, d) < rangeMaxDerivLenSquare)

          z += delta
        }

        y += delta
      }

      x += delta
    }
  }

  def test4fNoise(rangeMin: Float, rangeMax: Float, noise: (inVec4f) => Float) {
    import simplex3d.math.float.functions._
    
    val rangeMaxDerivLenSquare = 8*8

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

            if (rangeMin < rangeMax) assert(rangeMin <= n && n <= rangeMax)
            assert(dot(d, d) < rangeMaxDerivLenSquare)

            w += delta
          }

          z += delta
        }

        y += delta
      }

      x += delta
    }
  }
  
  // Tiled float.
  def test1fTiles(tile: Float, noise: (Float) => Float) {
    import simplex3d.math.float.functions._
    
    var i = 0; while (i < 100) {
      val p = randomFloat
      val np = noise(p)

      for (x <- -1 to 1) {
        val t = x
        val nt = noise(t*tile + p)
        assert(approxEqual(np, nt, 1e-6f))
      }
      
      i += 1
    }
  }

  def test2fTiles(tile: inVec2f, noise: (inVec2f) => Float) {
    import simplex3d.math.float.functions._
    
    var i = 0; while (i < 100) {
      val p = randomVec2f
      val np = noise(p)

      for (x <- -1 to 1; y <- -1 to 1) {
        val t = Vec2f(x, y)
        val nt = noise(t*tile + p)
        assert(approxEqual(np, nt, 1e-6f))
      }
      
      i += 1
    }
  }

  def test3fTiles(tile: inVec3f, noise: (inVec3f) => Float) {
    import simplex3d.math.float.functions._

    var i = 0; while (i < 100) {
      val p = randomVec3f
      val np = noise(p)

      for (x <- -1 to 1; y <- -1 to 1; z <- -1 to 1) {
        val t = Vec3f(x, y, z)
        val nt = noise(t*tile + p)
        assert(approxEqual(np, nt, 1e-6f))
      }
      
      i += 1
    }
  }

  def test4fTiles(tile: inVec4f, noise: (inVec4f) => Float) {
    import simplex3d.math.float.functions._

    var i = 0; while (i < 100) {
      val p = randomVec4f
      val np = noise(p)

      for (x <- -1 to 1; y <- -1 to 1; z <- -1 to 1; w <- -1 to 1) {
        val t = Vec4f(x, y, z, w)
        val nt = noise(t*tile + p)
        assert(approxEqual(np, nt, 1e-6f))
      }
      
      i += 1
    }
  }
}
