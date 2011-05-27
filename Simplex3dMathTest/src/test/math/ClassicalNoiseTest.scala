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
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import NoiseTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class ClassicalNoiseTest extends FunSuite {

  val noise = new ClassicalGradientNoise(0)
  
  test("1d noise") {
    test1dNoise(-1, 1, noise(_))
    test1fNoise(-1, 1, noise(_))
  }

  test("2d noise") {
    test2dNoise(0, 0, (u) => noise(u.x, u.y))
    test2fNoise(0, 0, (u) => noise(u.x, u.y))
  }

  test("3d noise") {
    test3dNoise(0, 0, (u) => noise(u.x, u.y, u.z))
    test3fNoise(0, 0, (u) => noise(u.x, u.y, u.z))
  }

  test("4d noise") {
    test4dNoise(0, 0, (u) => noise(u.x, u.y, u.z, u.w))
    test4fNoise(0, 0, (u) => noise(u.x, u.y, u.z, u.w))
  }
}
