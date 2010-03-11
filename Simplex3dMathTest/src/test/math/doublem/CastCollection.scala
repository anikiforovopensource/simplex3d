/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010 Simplex3d Team
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

package test.math.doublem

import org.scalatest._

import simplex3d.math._
import simplex3d.math.doublem.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
class CastCollection extends FunSuite {

  // These tests pass if they compile.
  test("Cast Collection") {
    {
      val m = Vec2(1)
      val c: ConstVec2 = m

      val tam: AnyVec[Double] = m
      val tac: AnyVec[Double] = c
      val tn: Vec[Double] = m
      val tc: ConstVec[Double] = c
    }
    {
      val m = Vec3(1)
      val c: ConstVec3 = m

      val tam: AnyVec[Double] = m
      val tac: AnyVec[Double] = c
      val tn: Vec[Double] = m
      val tc: ConstVec[Double] = c
    }
    {
      val m = Vec4(1)
      val c: ConstVec4 = m

      val tam: AnyVec[Double] = m
      val tac: AnyVec[Double] = c
      val tn: Vec[Double] = m
      val tc: ConstVec[Double] = c
    }

    {
      val m = Quat4(1, 0, 0, 0)
      val c: ConstQuat4 = m

      val tam: AnyQuat[Double] = m
      val tac: AnyQuat[Double] = c
      val tn: Quat[Double] = m
      val tc: ConstQuat[Double] = c
    }

    {
      val m = Mat2x2(1)
      val c: ConstMat2x2 = m

      val tam: AnyMat[ConstVec2] = m
      val tac: AnyMat[ConstVec2] = c
      val tn: Mat[ConstVec2] = m
      val tc: ConstMat[ConstVec2] = c
    }
    {
      val m = Mat2x3(1)
      val c: ConstMat2x3 = m

      val tam: AnyMat[ConstVec2] = m
      val tac: AnyMat[ConstVec2] = c
      val tn: Mat[ConstVec2] = m
      val tc: ConstMat[ConstVec2] = c
    }
    {
      val m = Mat2x4(1)
      val c: ConstMat2x4 = m

      val tam: AnyMat[ConstVec2] = m
      val tac: AnyMat[ConstVec2] = c
      val tn: Mat[ConstVec2] = m
      val tc: ConstMat[ConstVec2] = c
    }

    {
      val m = Mat3x2(1)
      val c: ConstMat3x2 = m

      val tam: AnyMat[ConstVec3] = m
      val tac: AnyMat[ConstVec3] = c
      val tn: Mat[ConstVec3] = m
      val tc: ConstMat[ConstVec3] = c
    }
    {
      val m = Mat3x3(1)
      val c: ConstMat3x3 = m

      val tam: AnyMat[ConstVec3] = m
      val tac: AnyMat[ConstVec3] = c
      val tn: Mat[ConstVec3] = m
      val tc: ConstMat[ConstVec3] = c
    }
    {
      val m = Mat3x4(1)
      val c: ConstMat3x4 = m

      val tam: AnyMat[ConstVec3] = m
      val tac: AnyMat[ConstVec3] = c
      val tn: Mat[ConstVec3] = m
      val tc: ConstMat[ConstVec3] = c
    }

    {
      val m = Mat4x2(1)
      val c: ConstMat4x2 = m

      val tam: AnyMat[ConstVec4] = m
      val tac: AnyMat[ConstVec4] = c
      val tn: Mat[ConstVec4] = m
      val tc: ConstMat[ConstVec4] = c
    }
    {
      val m = Mat4x3(1)
      val c: ConstMat4x3 = m

      val tam: AnyMat[ConstVec4] = m
      val tac: AnyMat[ConstVec4] = c
      val tn: Mat[ConstVec4] = m
      val tc: ConstMat[ConstVec4] = c
    }
    {
      val m = Mat4x4(1)
      val c: ConstMat4x4 = m

      val tam: AnyMat[ConstVec4] = m
      val tac: AnyMat[ConstVec4] = c
      val tn: Mat[ConstVec4] = m
      val tc: ConstMat[ConstVec4] = c
    }
  }
}
