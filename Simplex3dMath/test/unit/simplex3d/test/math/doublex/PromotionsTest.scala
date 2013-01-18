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

package simplex3d.test.math.doublex

import org.scalatest._

import simplex3d.math._
import simplex3d.math.floatx._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PromotionsTest extends FunSuite {

  val M = Mat4x4f(
    1, 2, 3, 4, 5, 6, 7, 8,
    9, 10, 11, 12, 13, 14, 15, 16
  )

  test("Promotions") {
    import simplex3d.math.doublex._

    val ci2: ConstVec2d = Vec2i(1, 2)
    expectResult(classOf[ConstVec2d]) { ci2.getClass }
    expectResult((1, 2)) { (ci2.x, ci2.y) }

    val ci3: ConstVec3d = Vec3i(1, 2, 3)
    expectResult(classOf[ConstVec3d]) { ci3.getClass }
    expectResult((1, 2, 3)) { (ci3.x, ci3.y, ci3.z) }

    val ci4: ConstVec4d = Vec4i(1, 2, 3, 4)
    expectResult(classOf[ConstVec4d]) { ci4.getClass }
    expectResult((1, 2, 3, 4)) { (ci4.x, ci4.y, ci4.z, ci4.w) }

    val cf2: ConstVec2d = Vec2f(1, 2)
    expectResult(classOf[ConstVec2d]) { cf2.getClass }
    expectResult((1, 2)) { (cf2.x, cf2.y) }

    val cf3: ConstVec3d = Vec3f(1, 2, 3)
    expectResult(classOf[ConstVec3d]) { cf3.getClass }
    expectResult((1, 2, 3)) { (cf3.x, cf3.y, cf3.z) }

    val cf4: ConstVec4d = Vec4f(1, 2, 3, 4)
    expectResult(classOf[ConstVec4d]) { cf4.getClass }
    expectResult((1, 2, 3, 4)) { (cf4.x, cf4.y, cf4.z, cf4.w) }


    val cq4: ConstQuat4d = Quat4f(1, 2, 3, 4)
    expectResult(classOf[ConstQuat4d]) { cq4.getClass }
    expectResult((1, 2, 3, 4)) { (cq4.a, cq4.b, cq4.c, cq4.d) }

    val cm2x2: ConstMat2x2d = Mat2x2f(M)
    expectResult(classOf[ConstMat2x2d]) { cm2x2.getClass }
    assert(cm2x2 == Mat2x2d(M))
    
    val cm3x2: ConstMat3x2d = Mat3x2f(M)
    expectResult(classOf[ConstMat3x2d]) { cm3x2.getClass }
    assert(cm3x2 == Mat3x2d(M))

    val cm4x2: ConstMat4x2d = Mat4x2f(M)
    expectResult(classOf[ConstMat4x2d]) { cm4x2.getClass }
    assert(cm4x2 == Mat4x2d(M))

    val cm2x3: ConstMat2x3d = Mat2x3f(M)
    expectResult(classOf[ConstMat2x3d]) { cm2x3.getClass }
    assert(cm2x3 == Mat2x3d(M))

    val cm3x3: ConstMat3x3d = Mat3x3f(M)
    expectResult(classOf[ConstMat3x3d]) { cm3x3.getClass }
    assert(cm3x3 == Mat3x3d(M))

    val cm4x3: ConstMat4x3d = Mat4x3f(M)
    expectResult(classOf[ConstMat4x3d]) { cm4x3.getClass }
    assert(cm4x3 == Mat4x3d(M))

    val cm2x4: ConstMat2x4d = Mat2x4f(M)
    expectResult(classOf[ConstMat2x4d]) { cm2x4.getClass }
    assert(cm2x4 == Mat2x4d(M))

    val cm3x4: ConstMat3x4d = Mat3x4f(M)
    expectResult(classOf[ConstMat3x4d]) { cm3x4.getClass }
    assert(cm3x4 == Mat3x4d(M))

    val cm4x4: ConstMat4x4d = Mat4x4f(M)
    expectResult(classOf[ConstMat4x4d]) { cm4x4.getClass }
    assert(cm4x4 == Mat4x4d(M))
  }

  test("Renamed promotions") {
    import simplex3d.math.double._

    val ci2: ConstVec2 = Vec2i(1, 2)
    expectResult(classOf[ConstVec2]) { ci2.getClass }
    expectResult((1, 2)) { (ci2.x, ci2.y) }

    val ci3: ConstVec3 = Vec3i(1, 2, 3)
    expectResult(classOf[ConstVec3]) { ci3.getClass }
    expectResult((1, 2, 3)) { (ci3.x, ci3.y, ci3.z) }

    val ci4: ConstVec4 = Vec4i(1, 2, 3, 4)
    expectResult(classOf[ConstVec4]) { ci4.getClass }
    expectResult((1, 2, 3, 4)) { (ci4.x, ci4.y, ci4.z, ci4.w) }

    val cf2: ConstVec2 = Vec2f(1, 2)
    expectResult(classOf[ConstVec2]) { cf2.getClass }
    expectResult((1, 2)) { (cf2.x, cf2.y) }

    val cf3: ConstVec3 = Vec3f(1, 2, 3)
    expectResult(classOf[ConstVec3]) { cf3.getClass }
    expectResult((1, 2, 3)) { (cf3.x, cf3.y, cf3.z) }

    val cf4: ConstVec4 = Vec4f(1, 2, 3, 4)
    expectResult(classOf[ConstVec4]) { cf4.getClass }
    expectResult((1, 2, 3, 4)) { (cf4.x, cf4.y, cf4.z, cf4.w) }


    val cq4: ConstQuat4 = Quat4f(1, 2, 3, 4)
    expectResult(classOf[ConstQuat4]) { cq4.getClass }
    expectResult((1, 2, 3, 4)) { (cq4.a, cq4.b, cq4.c, cq4.d) }

    val cm2x2: ConstMat2x2 = Mat2x2f(M)
    expectResult(classOf[ConstMat2x2]) { cm2x2.getClass }
    assert(cm2x2 == Mat2x2(M))

    val cm3x2: ConstMat3x2 = Mat3x2f(M)
    expectResult(classOf[ConstMat3x2]) { cm3x2.getClass }
    assert(cm3x2 == Mat3x2(M))

    val cm4x2: ConstMat4x2 = Mat4x2f(M)
    expectResult(classOf[ConstMat4x2]) { cm4x2.getClass }
    assert(cm4x2 == Mat4x2(M))

    val cm2x3: ConstMat2x3 = Mat2x3f(M)
    expectResult(classOf[ConstMat2x3]) { cm2x3.getClass }
    assert(cm2x3 == Mat2x3(M))

    val cm3x3: ConstMat3x3 = Mat3x3f(M)
    expectResult(classOf[ConstMat3x3]) { cm3x3.getClass }
    assert(cm3x3 == Mat3x3(M))

    val cm4x3: ConstMat4x3 = Mat4x3f(M)
    expectResult(classOf[ConstMat4x3]) { cm4x3.getClass }
    assert(cm4x3 == Mat4x3(M))

    val cm2x4: ConstMat2x4 = Mat2x4f(M)
    expectResult(classOf[ConstMat2x4]) { cm2x4.getClass }
    assert(cm2x4 == Mat2x4(M))

    val cm3x4: ConstMat3x4 = Mat3x4f(M)
    expectResult(classOf[ConstMat3x4]) { cm3x4.getClass }
    assert(cm3x4 == Mat3x4(M))

    val cm4x4: ConstMat4x4 = Mat4x4f(M)
    expectResult(classOf[ConstMat4x4]) { cm4x4.getClass }
    assert(cm4x4 == Mat4x4(M))
  }
}
