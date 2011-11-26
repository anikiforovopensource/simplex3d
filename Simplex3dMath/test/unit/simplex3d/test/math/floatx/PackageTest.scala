/*
 * Simplex3dMath - Test Package
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

package simplex3d.test.math.floatx

import org.scalatest._

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PackageTest extends FunSuite {

  test("Regular package") {
    import simplex3d.math.floatx._

    // implicits
    val i2 = 2.0f*Vec2i(1)
    assert(i2.isInstanceOf[Vec2f])

    val i3 = 2*Vec3f(1)
    assert(i3.isInstanceOf[Vec3f])

    val f3 = 2.0f*Vec3f(1)
    assert(f3.isInstanceOf[Vec3f])

    val pi2: ReadVec2f = Vec2i(1)
    assert(pi2.isInstanceOf[ConstVec2f])

    val pi3: ReadVec3f = Vec3i(1)
    assert(pi3.isInstanceOf[ConstVec3f])

    val pi4: ReadVec4f = Vec4i(1)
    assert(pi4.isInstanceOf[ConstVec4f])


    // the following passes if it compiles
    val pi = functions.Pi

    type fref = FloatRef

    type av2 = ReadVec2f
    type cv2 = ConstVec2f
    val cv2 = ConstVec2f
    type v2 = Vec2f
    val v2 = Vec2f

    type av3 = ReadVec3f
    type cv3 = ConstVec3f
    val cv3 = ConstVec3f
    type v3 = Vec3f
    val v3 = Vec3f

    type av4 = ReadVec4f
    type cv4 = ConstVec4f
    val cv4 = ConstVec4f
    type v4 = Vec4f
    val v4 = Vec4f

    type am2 = ReadMat2f
    type cm2 = ConstMat2f
    val cm2 = ConstMat2f
    type m2 = Mat2f
    val m2 = Mat2f

    type am2x3 = ReadMat2x3f
    type cm2x3 = ConstMat2x3f
    val cm2x3 = ConstMat2x3f
    type m2x3 = Mat2x3f
    val m2x3 = Mat2x3f

    type am2x4 = ReadMat2x4f
    type cm2x4 = ConstMat2x4f
    val cm2x4 = ConstMat2x4f
    type m2x4 = Mat2x4f
    val m2x4 = Mat2x4f

    type am3x2 = ReadMat3x2f
    type cm3x2 = ConstMat3x2f
    val cm3x2 = ConstMat3x2f
    type m3x2 = Mat3x2f
    val m3x2 = Mat3x2f

    type am3 = ReadMat3f
    type cm3 = ConstMat3f
    val cm3 = ConstMat3f
    type m3 = Mat3f
    val m3 = Mat3f

    type am3x4 = ReadMat3x4f
    type cm3x4 = ConstMat3x4f
    val cm3x4 = ConstMat3x4f
    type m3x4 = Mat3x4f
    val m3x4 = Mat3x4f

    type am4x2 = ReadMat4x2f
    type cm4x2 = ConstMat4x2f
    val cm4x2 = ConstMat4x2f
    type m4x2 = Mat4x2f
    val m4x2 = Mat4x2f

    type am4x3 = ReadMat4x3f
    type cm4x3 = ConstMat4x3f
    val cm4x3 = ConstMat4x3f
    type m4x3 = Mat4x3f
    val m4x3 = Mat4x3f

    type am4 = ReadMat4f
    type cm4 = ConstMat4f
    val cm4 = ConstMat4f
    type m4 = Mat4f
    val m4 = Mat4f

    type aq4 = ReadQuat4f
    type cq4 = ConstQuat4f
    val cq4 = ConstQuat4f
    type q4 = Quat4f
    val q4 = Quat4f

    // Aliases
    type am2x2 = ReadMat2x2f
    type cm2x2 = ConstMat2x2f
    val cm2x2 = ConstMat2x2f
    type m2x2 = Mat2x2f
    val m2x2 = Mat2x2f

    type am3x3 = ReadMat3x3f
    type cm3x3 = ConstMat3x3f
    val cm3x3 = ConstMat3x3f
    type m3x3 = Mat3x3f
    val m3x3 = Mat3x3f

    type am4x4 = ReadMat4x4f
    type cm4x4 = ConstMat4x4f
    val cm4x4 = ConstMat4x4f
    type m4x4 = Mat4x4f
    val m4x4 = Mat4x4f
  }

  test("Renamed package") {
    import simplex3d.math.float._

    // implicits
    val i2 = 2.0f*Vec2i(1)
    assert(i2.isInstanceOf[Vec2])

    val i3 = 2*Vec3(1)
    assert(i3.isInstanceOf[Vec3])

    val f3 = 2.0f*Vec3(1)
    assert(f3.isInstanceOf[Vec3])

    val pi2: ReadVec2 = Vec2i(1)
    assert(pi2.isInstanceOf[ConstVec2])

    val pi3: ReadVec3 = Vec3i(1)
    assert(pi3.isInstanceOf[ConstVec3])

    val pi4: ReadVec4 = Vec4i(1)
    assert(pi4.isInstanceOf[ConstVec4])


    assert(functions == simplex3d.math.floatx.functions)

    // the following passes if it compiles
    type fref = FloatRef

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
