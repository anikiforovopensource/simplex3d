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
    
    test1dTiles(1, noise(1, _))
    test1fTiles(1, noise(1, _))
    test1dTiles(2, noise(2, _))
    test1fTiles(2, noise(2, _))
  }

  test("2d noise") {
    test2dNoise(0, 0, (u) => noise(u.x, u.y))
    test2fNoise(0, 0, (u) => noise(u.x, u.y))
    
    test2dTiles(Vec2d(1), (u) => noise(1, 1, u.x, u.y))
    test2fTiles(Vec2f(1), (u) => noise(1, 1, u.x, u.y))
    test2dTiles(Vec2d(2), (u) => noise(2, 2, u.x, u.y))
    test2fTiles(Vec2f(2), (u) => noise(2, 2, u.x, u.y))
  }

  test("3d noise") {
    test3dNoise(0, 0, (u) => noise(u.x, u.y, u.z))
    test3fNoise(0, 0, (u) => noise(u.x, u.y, u.z))
    
    test3dTiles(Vec3d(1), (u) => noise(1, 1, 1, u.x, u.y, u.z))
    test3fTiles(Vec3f(1), (u) => noise(1, 1, 1, u.x, u.y, u.z))
    test3dTiles(Vec3d(2), (u) => noise(2, 2, 2, u.x, u.y, u.z))
    test3fTiles(Vec3f(2), (u) => noise(2, 2, 2, u.x, u.y, u.z))
  }

  test("4d noise") {
    test4dNoise(0, 0, (u) => noise(u.x, u.y, u.z, u.w))
    test4fNoise(0, 0, (u) => noise(u.x, u.y, u.z, u.w))
    
    test4dTiles(Vec4d(1), (u) => noise(1, 1, 1, 1, u.x, u.y, u.z, u.w))
    test4fTiles(Vec4f(1), (u) => noise(1, 1, 1, 1, u.x, u.y, u.z, u.w))
    test4dTiles(Vec4d(2), (u) => noise(2, 2, 2, 2, u.x, u.y, u.z, u.w))
    test4fTiles(Vec4f(2), (u) => noise(2, 2, 2, 2, u.x, u.y, u.z, u.w))
  }
}
