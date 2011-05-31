/*
 * Simplex3d, MathTest package
 * Copyright (C) 2011, Aleksey Nikiforov
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

package test.math.floatx

import java.io._
import org.scalatest._
import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._
import test.math.NoiseTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class FloatNoiseTest extends FunSuite {

  private val source = new ClassicalGradientNoise(0)


  test("Noise") {
    val n = new Noise(0, ClassicalGradientNoise)

    var count = 0
    var i = 0; while(i < 1000) {
      val u = randomFloat

      val p2 = n.noise2(u)
      if (approxEqual(p2.x, p2.y, 1e-3f)) count += 1

      val p3 = n.noise3(u)
      if (approxEqual(p3.x, p3.y, 1e-3f)) count += 1
      if (approxEqual(p3.y, p3.z, 1e-3f)) count += 1

      val p4 = n.noise4(u)
      if (approxEqual(p4.x, p4.y, 1e-3f)) count += 1
      if (approxEqual(p4.y, p4.z, 1e-3f)) count += 1
      if (approxEqual(p4.z, p4.w, 1e-3f)) count += 1

      i += 1
    }

    assert(count < 20)


    count = 0
    i = 0; while(i < 1000) {
      val u = randomVec2f

      val p2 = n.noise2(u)
      if (approxEqual(p2.x, p2.y, 1e-3f)) count += 1

      val p3 = n.noise3(u)
      if (approxEqual(p3.x, p3.y, 1e-3f)) count += 1
      if (approxEqual(p3.y, p3.z, 1e-3f)) count += 1

      val p4 = n.noise4(u)
      if (approxEqual(p4.x, p4.y, 1e-3f)) count += 1
      if (approxEqual(p4.y, p4.z, 1e-3f)) count += 1
      if (approxEqual(p4.z, p4.w, 1e-3f)) count += 1

      i += 1
    }

    assert(count < 20)


    count = 0
    i = 0; while(i < 1000) {
      val u = randomVec3f

      val p2 = n.noise2(u)
      if (approxEqual(p2.x, p2.y, 1e-3f)) count += 1

      val p3 = n.noise3(u)
      if (approxEqual(p3.x, p3.y, 1e-3f)) count += 1
      if (approxEqual(p3.y, p3.z, 1e-3f)) count += 1

      val p4 = n.noise4(u)
      if (approxEqual(p4.x, p4.y, 1e-3f)) count += 1
      if (approxEqual(p4.y, p4.z, 1e-3f)) count += 1
      if (approxEqual(p4.z, p4.w, 1e-3f)) count += 1

      i += 1
    }

    assert(count < 20)


    count = 0
    i = 0; while(i < 1000) {
      val u = randomVec4f

      val p2 = n.noise2(u)
      if (approxEqual(p2.x, p2.y, 1e-3f)) count += 1

      val p3 = n.noise3(u)
      if (approxEqual(p3.x, p3.y, 1e-3f)) count += 1
      if (approxEqual(p3.y, p3.z, 1e-3f)) count += 1

      val p4 = n.noise4(u)
      if (approxEqual(p4.x, p4.y, 1e-3f)) count += 1
      if (approxEqual(p4.y, p4.z, 1e-3f)) count += 1
      if (approxEqual(p4.z, p4.w, 1e-3f)) count += 1

      i += 1
    }

    assert(count < 20)
  }

  test("Noise Sum") {
    val noise = new NoiseSum(frequency = 1, octaves = 3, source = source)

    test1fNoise(-1, 1, noise(_))
    test2fNoise(0, 0, noise(_))
    test3fNoise(0, 0, noise(_))
    test4fNoise(0, 0, noise(_))
  }

  test("Turbulence") {
    val noise = new Turbulence(frequency = 1, octaves = 3, source = source)

    // Nothing to test.
  }

  test("Tiled Noise sum") {
    val noise = new TiledNoiseSum(tile = Vec4(2), frequency = 1, octaves = 3, source = source)

    test1fNoise(-1, 1, noise(_))
    test2fNoise(0, 0, noise(_))
    test3fNoise(0, 0, noise(_))
    test4fNoise(0, 0, noise(_))

    test1fTiles(2, noise(_))
    test2fTiles(Vec2(2), noise(_))
    test3fTiles(Vec3(2), noise(_))
    test4fTiles(Vec4(2), noise(_))
  }

  test("Tiled Turbulence") {
    val noise = new TiledTurbulence(tile = Vec4(2), frequency = 1, octaves = 3, source = source)

    test1fTiles(2, noise(_))
    test2fTiles(Vec2(2), noise(_))
    test3fTiles(Vec3(2), noise(_))
    test4fTiles(Vec4(2), noise(_))
  }

  test("Noise serialization") {
    val bytes = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bytes)

    val noise0 = new Noise(10, ClassicalGradientNoise)
    val noiseSum0 = new NoiseSum(2f, 5, 2.1f, 0.4f, new ClassicalGradientNoise(11))
    val turbulence0 = new Turbulence(2f, 5, 2.1f, 0.4f, 0.2f, new ClassicalGradientNoise(11))
    val tiledNoiseSum0 = new TiledNoiseSum(Vec4(3), 2f, 5, 2.1f, 0.4f, new ClassicalGradientNoise(11))
    val tiledTurbulence0 = new TiledTurbulence(Vec4(3), 2f, 5, 2.1f, 0.4f, 0.2f, new ClassicalGradientNoise(11))

    out.writeObject(noise0)
    out.writeObject(noiseSum0)
    out.writeObject(turbulence0)
    out.writeObject(tiledNoiseSum0)
    out.writeObject(tiledTurbulence0)

    out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray))

    val noise1 = in.readObject().asInstanceOf[Noise]
    val noiseSum1 = in.readObject().asInstanceOf[NoiseSum]
    val turbulence1 = in.readObject().asInstanceOf[Turbulence]
    val tiledNoiseSum1 = in.readObject().asInstanceOf[TiledNoiseSum]
    val tiledTurbulence1 = in.readObject().asInstanceOf[TiledTurbulence]

    var i = 0; while (i < 100) {
      val u1 = randomFloat
      val u2 = randomVec2f
      val u3 = randomVec3f
      val u4 = randomVec4f

      assert(noise0.noise4(u1) == noise1.noise4(u1))
      assert(noise0.noise4(u2) == noise1.noise4(u2))
      assert(noise0.noise4(u3) == noise1.noise4(u3))
      assert(noise0.noise4(u4) == noise1.noise4(u4))

      assert(noiseSum0(u1) == noiseSum1(u1))
      assert(noiseSum0(u2) == noiseSum1(u2))
      assert(noiseSum0(u3) == noiseSum1(u3))
      assert(noiseSum0(u4) == noiseSum1(u4))

      assert(turbulence0(u1) == turbulence1(u1))
      assert(turbulence0(u2) == turbulence1(u2))
      assert(turbulence0(u3) == turbulence1(u3))
      assert(turbulence0(u4) == turbulence1(u4))

      assert(tiledNoiseSum0(u1) == tiledNoiseSum1(u1))
      assert(tiledNoiseSum0(u2) == tiledNoiseSum1(u2))
      assert(tiledNoiseSum0(u3) == tiledNoiseSum1(u3))
      assert(tiledNoiseSum0(u4) == tiledNoiseSum1(u4))

      assert(tiledTurbulence0(u1) == tiledTurbulence1(u1))
      assert(tiledTurbulence0(u2) == tiledTurbulence1(u2))
      assert(tiledTurbulence0(u3) == tiledTurbulence1(u3))
      assert(tiledTurbulence0(u4) == tiledTurbulence1(u4))

      i += 1
    }
  }
}
