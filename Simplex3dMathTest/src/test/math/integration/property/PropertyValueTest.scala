/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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

package test.math.integration.property

import org.scalatest._
import simplex3d.math.integration.property._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PropertyValueTest extends FunSuite {

  def propVal[T](v: PropertyValue[T]) = v

  test("PropertyPrimitive") {
    {
      val p = propVal(true)
      val pc = p.clone()
      val v = p.cloneValue()
      assert(p.asReadInstance() == true)
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := false
      assert(p.asReadInstance() == false)
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(1)
      val pc = p.clone()
      val v = p.cloneValue()
      assert(p.asReadInstance() == 1)
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := 2
      assert(p.asReadInstance() == 2)
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(1f)
      val pc = p.clone()
      val v = p.cloneValue()
      assert(p.asReadInstance() == 1f)
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := 2f
      assert(p.asReadInstance() == 2f)
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(1d)
      val pc = p.clone()
      val v = p.cloneValue()
      assert(p.asReadInstance() == 1d)
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := 2d
      assert(p.asReadInstance() == 2d)
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
  }

  test("Renamed implicits") {
    {
      import simplex3d.math.floatm.renamed._
      
      {
        val p = propVal(Vec2(1).asInstanceOf[ReadVec2])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Vec2(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Vec2(2)
        assert(p.asReadInstance() == Vec2(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Vec3(1).asInstanceOf[ReadVec3])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Vec3(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Vec3(2)
        assert(p.asReadInstance() == Vec3(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Vec4(1).asInstanceOf[ReadVec4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Vec4(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Vec4(2)
        assert(p.asReadInstance() == Vec4(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }

      {
        val p = propVal(Quat4(1, 1, 1, 1).asInstanceOf[ReadQuat4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Quat4(1, 1, 1, 1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Quat4(2, 2, 2, 2)
        assert(p.asReadInstance() == Quat4(2, 2, 2, 2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }

      {
        val p = propVal(Mat2x2(1).asInstanceOf[ReadMat2x2])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat2x2(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat2x2(2)
        assert(p.asReadInstance() == Mat2x2(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat2x3(1).asInstanceOf[ReadMat2x3])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat2x3(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat2x3(2)
        assert(p.asReadInstance() == Mat2x3(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat2x4(1).asInstanceOf[ReadMat2x4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat2x4(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat2x4(2)
        assert(p.asReadInstance() == Mat2x4(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }

      {
        val p = propVal(Mat3x2(1).asInstanceOf[ReadMat3x2])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat3x2(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat3x2(2)
        assert(p.asReadInstance() == Mat3x2(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat3x3(1).asInstanceOf[ReadMat3x3])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat3x3(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat3x3(2)
        assert(p.asReadInstance() == Mat3x3(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat3x4(1).asInstanceOf[ReadMat3x4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat3x4(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat3x4(2)
        assert(p.asReadInstance() == Mat3x4(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }

      {
        val p = propVal(Mat4x2(1).asInstanceOf[ReadMat4x2])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat4x2(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat4x2(2)
        assert(p.asReadInstance() == Mat4x2(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat4x3(1).asInstanceOf[ReadMat4x3])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat4x3(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat4x3(2)
        assert(p.asReadInstance() == Mat4x3(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat4x4(1).asInstanceOf[ReadMat4x4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat4x4(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat4x4(2)
        assert(p.asReadInstance() == Mat4x4(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
    }


    {
      import simplex3d.math.doublem.renamed._

      {
        val p = propVal(Vec2(1).asInstanceOf[ReadVec2])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Vec2(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Vec2(2)
        assert(p.asReadInstance() == Vec2(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Vec3(1).asInstanceOf[ReadVec3])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Vec3(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Vec3(2)
        assert(p.asReadInstance() == Vec3(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Vec4(1).asInstanceOf[ReadVec4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Vec4(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Vec4(2)
        assert(p.asReadInstance() == Vec4(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }

      {
        val p = propVal(Quat4(1, 1, 1, 1).asInstanceOf[ReadQuat4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Quat4(1, 1, 1, 1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Quat4(2, 2, 2, 2)
        assert(p.asReadInstance() == Quat4(2, 2, 2, 2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }

      {
        val p = propVal(Mat2x2(1).asInstanceOf[ReadMat2x2])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat2x2(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat2x2(2)
        assert(p.asReadInstance() == Mat2x2(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat2x3(1).asInstanceOf[ReadMat2x3])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat2x3(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat2x3(2)
        assert(p.asReadInstance() == Mat2x3(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat2x4(1).asInstanceOf[ReadMat2x4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat2x4(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat2x4(2)
        assert(p.asReadInstance() == Mat2x4(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }

      {
        val p = propVal(Mat3x2(1).asInstanceOf[ReadMat3x2])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat3x2(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat3x2(2)
        assert(p.asReadInstance() == Mat3x2(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat3x3(1).asInstanceOf[ReadMat3x3])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat3x3(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat3x3(2)
        assert(p.asReadInstance() == Mat3x3(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat3x4(1).asInstanceOf[ReadMat3x4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat3x4(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat3x4(2)
        assert(p.asReadInstance() == Mat3x4(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }

      {
        val p = propVal(Mat4x2(1).asInstanceOf[ReadMat4x2])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat4x2(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat4x2(2)
        assert(p.asReadInstance() == Mat4x2(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat4x3(1).asInstanceOf[ReadMat4x3])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat4x3(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat4x3(2)
        assert(p.asReadInstance() == Mat4x3(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
      {
        val p = propVal(Mat4x4(1).asInstanceOf[ReadMat4x4])
        val pc = p.clone()
        val v = p.cloneValue()
        checkType(p.asReadInstance())
        assert(p.asReadInstance() == Mat4x4(1))
        assert(p.asReadInstance() == v)
        assert(p.asReadInstance() == pc.asReadInstance())
        p := Mat4x4(2)
        assert(p.asReadInstance() == Mat4x4(2))
        assert(p.asReadInstance() != v)
        assert(p.asReadInstance() != pc.asReadInstance())
      }
    }
  }

  import simplex3d.math._
  import simplex3d.math.intm._
  import simplex3d.math.floatm._
  import simplex3d.math.doublem._

  test("PropertyObject") {
    {
      val p = propVal(Vec2b(true).asInstanceOf[ReadVec2b])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec2b(true))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec2b(false)
      assert(p.asReadInstance() == Vec2b(false))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Vec3b(true).asInstanceOf[ReadVec3b])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec3b(true))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec3b(false)
      assert(p.asReadInstance() == Vec3b(false))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Vec4b(true).asInstanceOf[ReadVec4b])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec4b(true))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec4b(false)
      assert(p.asReadInstance() == Vec4b(false))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(Vec2i(1).asInstanceOf[ReadVec2i])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec2i(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec2i(2)
      assert(p.asReadInstance() == Vec2i(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Vec3i(1).asInstanceOf[ReadVec3i])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec3i(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec3i(2)
      assert(p.asReadInstance() == Vec3i(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Vec4i(1).asInstanceOf[ReadVec4i])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec4i(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec4i(2)
      assert(p.asReadInstance() == Vec4i(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(Vec2f(1).asInstanceOf[ReadVec2f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec2f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec2f(2)
      assert(p.asReadInstance() == Vec2f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Vec3f(1).asInstanceOf[ReadVec3f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec3f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec3f(2)
      assert(p.asReadInstance() == Vec3f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Vec4f(1).asInstanceOf[ReadVec4f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec4f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec4f(2)
      assert(p.asReadInstance() == Vec4f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(Vec2d(1).asInstanceOf[ReadVec2d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec2d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec2d(2)
      assert(p.asReadInstance() == Vec2d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Vec3d(1).asInstanceOf[ReadVec3d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec3d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec3d(2)
      assert(p.asReadInstance() == Vec3d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Vec4d(1).asInstanceOf[ReadVec4d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Vec4d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Vec4d(2)
      assert(p.asReadInstance() == Vec4d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }


    {
      val p = propVal(Quat4f(1, 1, 1, 1).asInstanceOf[ReadQuat4f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Quat4f(1, 1, 1, 1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Quat4f(2, 2, 2, 2)
      assert(p.asReadInstance() == Quat4f(2, 2, 2, 2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Quat4d(1, 1, 1, 1).asInstanceOf[ReadQuat4d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Quat4d(1, 1, 1, 1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Quat4d(2, 2, 2, 2)
      assert(p.asReadInstance() == Quat4d(2, 2, 2, 2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }


    {
      val p = propVal(Mat2x2f(1).asInstanceOf[ReadMat2x2f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat2x2f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat2x2f(2)
      assert(p.asReadInstance() == Mat2x2f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat2x3f(1).asInstanceOf[ReadMat2x3f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat2x3f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat2x3f(2)
      assert(p.asReadInstance() == Mat2x3f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat2x4f(1).asInstanceOf[ReadMat2x4f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat2x4f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat2x4f(2)
      assert(p.asReadInstance() == Mat2x4f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(Mat3x2f(1).asInstanceOf[ReadMat3x2f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat3x2f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat3x2f(2)
      assert(p.asReadInstance() == Mat3x2f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat3x3f(1).asInstanceOf[ReadMat3x3f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat3x3f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat3x3f(2)
      assert(p.asReadInstance() == Mat3x3f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat3x4f(1).asInstanceOf[ReadMat3x4f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat3x4f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat3x4f(2)
      assert(p.asReadInstance() == Mat3x4f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(Mat4x2f(1).asInstanceOf[ReadMat4x2f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat4x2f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat4x2f(2)
      assert(p.asReadInstance() == Mat4x2f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat4x3f(1).asInstanceOf[ReadMat4x3f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat4x3f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat4x3f(2)
      assert(p.asReadInstance() == Mat4x3f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat4x4f(1).asInstanceOf[ReadMat4x4f])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat4x4f(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat4x4f(2)
      assert(p.asReadInstance() == Mat4x4f(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }


    {
      val p = propVal(Mat2x2d(1).asInstanceOf[ReadMat2x2d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat2x2d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat2x2d(2)
      assert(p.asReadInstance() == Mat2x2d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat2x3d(1).asInstanceOf[ReadMat2x3d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat2x3d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat2x3d(2)
      assert(p.asReadInstance() == Mat2x3d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat2x4d(1).asInstanceOf[ReadMat2x4d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat2x4d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat2x4d(2)
      assert(p.asReadInstance() == Mat2x4d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(Mat3x2d(1).asInstanceOf[ReadMat3x2d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat3x2d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat3x2d(2)
      assert(p.asReadInstance() == Mat3x2d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat3x3d(1).asInstanceOf[ReadMat3x3d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat3x3d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat3x3d(2)
      assert(p.asReadInstance() == Mat3x3d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat3x4d(1).asInstanceOf[ReadMat3x4d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat3x4d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat3x4d(2)
      assert(p.asReadInstance() == Mat3x4d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }

    {
      val p = propVal(Mat4x2d(1).asInstanceOf[ReadMat4x2d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat4x2d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat4x2d(2)
      assert(p.asReadInstance() == Mat4x2d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat4x3d(1).asInstanceOf[ReadMat4x3d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat4x3d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat4x3d(2)
      assert(p.asReadInstance() == Mat4x3d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
    {
      val p = propVal(Mat4x4d(1).asInstanceOf[ReadMat4x4d])
      val pc = p.clone()
      val v = p.cloneValue()
      checkType(p.asReadInstance())
      assert(p.asReadInstance() == Mat4x4d(1))
      assert(p.asReadInstance() == v)
      assert(p.asReadInstance() == pc.asReadInstance())
      p := Mat4x4d(2)
      assert(p.asReadInstance() == Mat4x4d(2))
      assert(p.asReadInstance() != v)
      assert(p.asReadInstance() != pc.asReadInstance())
    }
  }

  def checkType(value: ReadVec2b) {}
  def checkType(value: Vec2b) { throw new AssertionError }
  def checkType(value: ReadVec3b) {}
  def checkType(value: Vec3b) { throw new AssertionError }
  def checkType(value: ReadVec4b) {}
  def checkType(value: Vec4b) { throw new AssertionError }

  def checkType(value: ReadVec2i) {}
  def checkType(value: Vec2i) { throw new AssertionError }
  def checkType(value: ReadVec3i) {}
  def checkType(value: Vec3i) { throw new AssertionError }
  def checkType(value: ReadVec4i) {}
  def checkType(value: Vec4i) { throw new AssertionError }

  def checkType(value: ReadVec2f) {}
  def checkType(value: Vec2f) { throw new AssertionError }
  def checkType(value: ReadVec3f) {}
  def checkType(value: Vec3f) { throw new AssertionError }
  def checkType(value: ReadVec4f) {}
  def checkType(value: Vec4f) { throw new AssertionError }

  def checkType(value: ReadVec2d) {}
  def checkType(value: Vec2d) { throw new AssertionError }
  def checkType(value: ReadVec3d) {}
  def checkType(value: Vec3d) { throw new AssertionError }
  def checkType(value: ReadVec4d) {}
  def checkType(value: Vec4d) { throw new AssertionError }

  def checkType(value: ReadQuat4f) {}
  def checkType(value: Quat4f) { throw new AssertionError }
  def checkType(value: ReadQuat4d) {}
  def checkType(value: Quat4d) { throw new AssertionError }

  def checkType(value: ReadMat2x2f) {}
  def checkType(value: Mat2x2f) { throw new AssertionError }
  def checkType(value: ReadMat2x3f) {}
  def checkType(value: Mat2x3f) { throw new AssertionError }
  def checkType(value: ReadMat2x4f) {}
  def checkType(value: Mat2x4f) { throw new AssertionError }

  def checkType(value: ReadMat3x2f) {}
  def checkType(value: Mat3x2f) { throw new AssertionError }
  def checkType(value: ReadMat3x3f) {}
  def checkType(value: Mat3x3f) { throw new AssertionError }
  def checkType(value: ReadMat3x4f) {}
  def checkType(value: Mat3x4f) { throw new AssertionError }

  def checkType(value: ReadMat4x2f) {}
  def checkType(value: Mat4x2f) { throw new AssertionError }
  def checkType(value: ReadMat4x3f) {}
  def checkType(value: Mat4x3f) { throw new AssertionError }
  def checkType(value: ReadMat4x4f) {}
  def checkType(value: Mat4x4f) { throw new AssertionError }

  def checkType(value: ReadMat2x2d) {}
  def checkType(value: Mat2x2d) { throw new AssertionError }
  def checkType(value: ReadMat2x3d) {}
  def checkType(value: Mat2x3d) { throw new AssertionError }
  def checkType(value: ReadMat2x4d) {}
  def checkType(value: Mat2x4d) { throw new AssertionError }

  def checkType(value: ReadMat3x2d) {}
  def checkType(value: Mat3x2d) { throw new AssertionError }
  def checkType(value: ReadMat3x3d) {}
  def checkType(value: Mat3x3d) { throw new AssertionError }
  def checkType(value: ReadMat3x4d) {}
  def checkType(value: Mat3x4d) { throw new AssertionError }

  def checkType(value: ReadMat4x2d) {}
  def checkType(value: Mat4x2d) { throw new AssertionError }
  def checkType(value: ReadMat4x3d) {}
  def checkType(value: Mat4x3d) { throw new AssertionError }
  def checkType(value: ReadMat4x4d) {}
  def checkType(value: Mat4x4d) { throw new AssertionError }
}
