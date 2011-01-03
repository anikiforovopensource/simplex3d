/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Simplex3d Team
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

package test.math.doublex

import org.scalatest._

import simplex3d.math._
import simplex3d.math.floatx._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PackageTest extends FunSuite {

  test("Regular package") {
    import simplex3d.math.doublex._

    // implicits
    val i2 = 2.0d*Vec2i(1)
    assert(i2.isInstanceOf[Vec2d])

    val f2 = 2.0d*Vec2f(1)
    assert(i2.isInstanceOf[Vec2d])

    val i3 = 2*Vec3d(1)
    assert(i3.isInstanceOf[Vec3d])

    val f3 = 2.0f*Vec3d(1)
    assert(f3.isInstanceOf[Vec3d])

    val d3 = 2.0d*Vec3d(1)
    assert(d3.isInstanceOf[Vec3d])

    val pi2: Vec2d = Vec2i(1)
    assert(pi2.isInstanceOf[Vec2d])

    val pi3: Vec3d = Vec3i(1)
    assert(pi3.isInstanceOf[Vec3d])

    val pi4: Vec4d = Vec4i(1)
    assert(pi4.isInstanceOf[Vec4d])

    val pf2: Vec2d = Vec2f(1)
    assert(pf2.isInstanceOf[Vec2d])

    val pf3: Vec3d = Vec3f(1)
    assert(pf3.isInstanceOf[Vec3d])

    val pf4: Vec4d = Vec4f(1)
    assert(pf4.isInstanceOf[Vec4d])


    // the following passes if it compiles
    val pi = functions.Pi

    type av2 = ReadVec2d
    type cv2 = ConstVec2d
    val cv2 = ConstVec2d
    type v2 = Vec2d
    val v2 = Vec2d

    type av3 = ReadVec3d
    type cv3 = ConstVec3d
    val cv3 = ConstVec3d
    type v3 = Vec3d
    val v3 = Vec3d

    type av4 = ReadVec4d
    type cv4 = ConstVec4d
    val cv4 = ConstVec4d
    type v4 = Vec4d
    val v4 = Vec4d

    type am2 = ReadMat2d
    type cm2 = ConstMat2d
    val cm2 = ConstMat2d
    type m2 = Mat2d
    val m2 = Mat2d

    type am2x3 = ReadMat2x3d
    type cm2x3 = ConstMat2x3d
    val cm2x3 = ConstMat2x3d
    type m2x3 = Mat2x3d
    val m2x3 = Mat2x3d

    type am2x4 = ReadMat2x4d
    type cm2x4 = ConstMat2x4d
    val cm2x4 = ConstMat2x4d
    type m2x4 = Mat2x4d
    val m2x4 = Mat2x4d

    type am3x2 = ReadMat3x2d
    type cm3x2 = ConstMat3x2d
    val cm3x2 = ConstMat3x2d
    type m3x2 = Mat3x2d
    val m3x2 = Mat3x2d

    type am3 = ReadMat3d
    type cm3 = ConstMat3d
    val cm3 = ConstMat3d
    type m3 = Mat3d
    val m3 = Mat3d

    type am3x4 = ReadMat3x4d
    type cm3x4 = ConstMat3x4d
    val cm3x4 = ConstMat3x4d
    type m3x4 = Mat3x4d
    val m3x4 = Mat3x4d

    type am4x2 = ReadMat4x2d
    type cm4x2 = ConstMat4x2d
    val cm4x2 = ConstMat4x2d
    type m4x2 = Mat4x2d
    val m4x2 = Mat4x2d

    type am4x3 = ReadMat4x3d
    type cm4x3 = ConstMat4x3d
    val cm4x3 = ConstMat4x3d
    type m4x3 = Mat4x3d
    val m4x3 = Mat4x3d

    type am4 = ReadMat4d
    type cm4 = ConstMat4d
    val cm4 = ConstMat4d
    type m4 = Mat4d
    val m4 = Mat4d

    type aq4 = ReadQuat4d
    type cq4 = ConstQuat4d
    val cq4 = ConstQuat4d
    type q4 = Quat4d
    val q4 = Quat4d

    // Aliases
    type am2x2 = ReadMat2x2d
    type cm2x2 = ConstMat2x2d
    val cm2x2 = ConstMat2x2d
    type m2x2 = Mat2x2d
    val m2x2 = Mat2x2d

    type am3x3 = ReadMat3x3d
    type cm3x3 = ConstMat3x3d
    val cm3x3 = ConstMat3x3d
    type m3x3 = Mat3x3d
    val m3x3 = Mat3x3d

    type am4x4 = ReadMat4x4d
    type cm4x4 = ConstMat4x4d
    val cm4x4 = ConstMat4x4d
    type m4x4 = Mat4x4d
    val m4x4 = Mat4x4d
  }

  test("Renamed package") {
    import simplex3d.math.double._

    // implicits
    val i2 = 2.0d*Vec2i(1)
    assert(i2.isInstanceOf[Vec2])

    val f2 = 2.0d*Vec2f(1)
    assert(i2.isInstanceOf[Vec2])

    val i3 = 2*Vec3(1)
    assert(i3.isInstanceOf[Vec3])

    val f3 = 2.0f*Vec3(1)
    assert(f3.isInstanceOf[Vec3])

    val d3 = 2.0d*Vec3(1)
    assert(d3.isInstanceOf[Vec3])

    val pi2: Vec2 = Vec2i(1)
    assert(pi2.isInstanceOf[Vec2])

    val pi3: Vec3 = Vec3i(1)
    assert(pi3.isInstanceOf[Vec3])

    val pi4: Vec4 = Vec4i(1)
    assert(pi4.isInstanceOf[Vec4])

    val pf2: Vec2 = Vec2f(1)
    assert(pf2.isInstanceOf[Vec2])

    val pf3: Vec3 = Vec3f(1)
    assert(pf3.isInstanceOf[Vec3])

    val pf4: Vec4 = Vec4f(1)
    assert(pf4.isInstanceOf[Vec4])


    assert(functions == simplex3d.math.doublex.functions)
    
    // the following passes if it compiles
    type av2 = ReadVec2
    type cv2 = ConstVec2
    val cv2 = ConstVec2
    type v2 = Vec2
    val v2 = Vec2

    type av3 = ReadVec3
    type cv3 = ConstVec3
    val cv3 = ConstVec3
    type v3 = Vec3
    val v3 = Vec3

    type av4 = ReadVec4
    type cv4 = ConstVec4
    val cv4 = ConstVec4
    type v4 = Vec4
    val v4 = Vec4

    type am2 = ReadMat2
    type cm2 = ConstMat2
    val cm2 = ConstMat2
    type m2 = Mat2
    val m2 = Mat2

    type am2x3 = ReadMat2x3
    type cm2x3 = ConstMat2x3
    val cm2x3 = ConstMat2x3
    type m2x3 = Mat2x3
    val m2x3 = Mat2x3

    type am2x4 = ReadMat2x4
    type cm2x4 = ConstMat2x4
    val cm2x4 = ConstMat2x4
    type m2x4 = Mat2x4
    val m2x4 = Mat2x4

    type am3x2 = ReadMat3x2
    type cm3x2 = ConstMat3x2
    val cm3x2 = ConstMat3x2
    type m3x2 = Mat3x2
    val m3x2 = Mat3x2

    type am3 = ReadMat3
    type cm3 = ConstMat3
    val cm3 = ConstMat3
    type m3 = Mat3
    val m3 = Mat3

    type am3x4 = ReadMat3x4
    type cm3x4 = ConstMat3x4
    val cm3x4 = ConstMat3x4
    type m3x4 = Mat3x4
    val m3x4 = Mat3x4

    type am4x2 = ReadMat4x2
    type cm4x2 = ConstMat4x2
    val cm4x2 = ConstMat4x2
    type m4x2 = Mat4x2
    val m4x2 = Mat4x2

    type am4x3 = ReadMat4x3
    type cm4x3 = ConstMat4x3
    val cm4x3 = ConstMat4x3
    type m4x3 = Mat4x3
    val m4x3 = Mat4x3

    type am4 = ReadMat4
    type cm4 = ConstMat4
    val cm4 = ConstMat4
    type m4 = Mat4
    val m4 = Mat4

    type aq4 = ReadQuat4
    type cq4 = ConstQuat4
    val cq4 = ConstQuat4
    type q4 = Quat4
    val q4 = Quat4

    // Aliases
    type am2x2 = ReadMat2x2
    type cm2x2 = ConstMat2x2
    val cm2x2 = ConstMat2x2
    type m2x2 = Mat2x2
    val m2x2 = Mat2x2

    type am3x3 = ReadMat3x3
    type cm3x3 = ConstMat3x3
    val cm3x3 = ConstMat3x3
    type m3x3 = Mat3x3
    val m3x3 = Mat3x3

    type am4x4 = ReadMat4x4
    type cm4x4 = ConstMat4x4
    val cm4x4 = ConstMat4x4
    type m4x4 = Mat4x4
    val m4x4 = Mat4x4
  }
}
