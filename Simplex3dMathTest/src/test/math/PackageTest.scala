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


/**
 * @author Aleksey Nikiforov (lex)
 */
class PackageTest extends FunSuite {

  test("Package") {
    import simplex3d.math._

    // implicits
    val i2 = 2*Vec2i(1)
    assert(i2.isInstanceOf[Vec2i])

    val i3 = 2*Vec3i(1)
    assert(i3.isInstanceOf[Vec3i])

    val i4 = 2*Vec4i(1)
    assert(i4.isInstanceOf[Vec4i])


    type m2 = AnyMat2x2[_]
    type m3 = AnyMat3x3[_]
    type m4 = AnyMat4x4[_]
  }
}
