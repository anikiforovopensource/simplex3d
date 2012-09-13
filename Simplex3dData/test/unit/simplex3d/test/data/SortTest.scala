/*
 * Simplex3dData - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.test.data

import org.scalatest._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.data._
import simplex3d.data.float._
import simplex3d.data.double._

import Descriptors._
import SortTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class SortTest extends FunSuite {

  test("Sort SInt") {
    testSort(DataFactory[SInt, UByte])
    testSort(DataFactory[SInt, SByte])
    testSort(DataFactory[SInt, UShort])
    testSort(DataFactory[SInt, SShort])
    testSort(DataFactory[SInt, UInt])
    testSort(DataFactory[SInt, SInt])
  }

  test("Sort Vec2i") {
    testSort(DataFactory[Vec2i, UByte])
    testSort(DataFactory[Vec2i, SByte])
    testSort(DataFactory[Vec2i, UShort])
    testSort(DataFactory[Vec2i, SShort])
    testSort(DataFactory[Vec2i, UInt])
    testSort(DataFactory[Vec2i, SInt])
  }

  test("Sort Vec3i") {
    testSort(DataFactory[Vec3i, UByte])
    testSort(DataFactory[Vec3i, SByte])
    testSort(DataFactory[Vec3i, UShort])
    testSort(DataFactory[Vec3i, SShort])
    testSort(DataFactory[Vec3i, UInt])
    testSort(DataFactory[Vec3i, SInt])
  }

  test("Sort Vec4i") {
    testSort(DataFactory[Vec4i, UByte])
    testSort(DataFactory[Vec4i, SByte])
    testSort(DataFactory[Vec4i, UShort])
    testSort(DataFactory[Vec4i, SShort])
    testSort(DataFactory[Vec4i, UInt])
    testSort(DataFactory[Vec4i, SInt])
  }


  test("Sort RFloat") {
    testSort(DataFactory[RFloat, UByte])
    testSort(DataFactory[RFloat, SByte])
    testSort(DataFactory[RFloat, UShort])
    testSort(DataFactory[RFloat, SShort])
    testSort(DataFactory[RFloat, UInt])
    testSort(DataFactory[RFloat, SInt])
    testSort(DataFactory[RFloat, HFloat])
    testSort(DataFactory[RFloat, RFloat])
  }

  test("Sort Vec2f") {
    testSort(DataFactory[Vec2f, UByte])
    testSort(DataFactory[Vec2f, SByte])
    testSort(DataFactory[Vec2f, UShort])
    testSort(DataFactory[Vec2f, SShort])
    testSort(DataFactory[Vec2f, UInt])
    testSort(DataFactory[Vec2f, SInt])
    testSort(DataFactory[Vec2f, HFloat])
    testSort(DataFactory[Vec2f, RFloat])
  }

  test("Sort Vec3f") {
    testSort(DataFactory[Vec3f, UByte])
    testSort(DataFactory[Vec3f, SByte])
    testSort(DataFactory[Vec3f, UShort])
    testSort(DataFactory[Vec3f, SShort])
    testSort(DataFactory[Vec3f, UInt])
    testSort(DataFactory[Vec3f, SInt])
    testSort(DataFactory[Vec3f, HFloat])
    testSort(DataFactory[Vec3f, RFloat])
  }

  test("Sort Vec4f") {
    testSort(DataFactory[Vec4f, UByte])
    testSort(DataFactory[Vec4f, SByte])
    testSort(DataFactory[Vec4f, UShort])
    testSort(DataFactory[Vec4f, SShort])
    testSort(DataFactory[Vec4f, UInt])
    testSort(DataFactory[Vec4f, SInt])
    testSort(DataFactory[Vec4f, HFloat])
    testSort(DataFactory[Vec4f, RFloat])
  }

  test("Sort Mat3x2f") {
    testSort(DataFactory[Mat3x2f, RFloat])
  }


  test("Sort RDouble") {
    testSort(DataFactory[RDouble, UByte])
    testSort(DataFactory[RDouble, SByte])
    testSort(DataFactory[RDouble, UShort])
    testSort(DataFactory[RDouble, SShort])
    testSort(DataFactory[RDouble, UInt])
    testSort(DataFactory[RDouble, SInt])
    testSort(DataFactory[RDouble, HFloat])
    testSort(DataFactory[RDouble, RFloat])
    testSort(DataFactory[RDouble, RDouble])
  }

  test("Sort Vec2d") {
    testSort(DataFactory[Vec2d, UByte])
    testSort(DataFactory[Vec2d, SByte])
    testSort(DataFactory[Vec2d, UShort])
    testSort(DataFactory[Vec2d, SShort])
    testSort(DataFactory[Vec2d, UInt])
    testSort(DataFactory[Vec2d, SInt])
    testSort(DataFactory[Vec2d, HFloat])
    testSort(DataFactory[Vec2d, RFloat])
    testSort(DataFactory[Vec2d, RDouble])
  }

  test("Sort Vec3d") {
    testSort(DataFactory[Vec3d, UByte])
    testSort(DataFactory[Vec3d, SByte])
    testSort(DataFactory[Vec3d, UShort])
    testSort(DataFactory[Vec3d, SShort])
    testSort(DataFactory[Vec3d, UInt])
    testSort(DataFactory[Vec3d, SInt])
    testSort(DataFactory[Vec3d, HFloat])
    testSort(DataFactory[Vec3d, RFloat])
    testSort(DataFactory[Vec3d, RDouble])
  }

  test("Sort Vec4d") {
    testSort(DataFactory[Vec4d, UByte])
    testSort(DataFactory[Vec4d, SByte])
    testSort(DataFactory[Vec4d, UShort])
    testSort(DataFactory[Vec4d, SShort])
    testSort(DataFactory[Vec4d, UInt])
    testSort(DataFactory[Vec4d, SInt])
    testSort(DataFactory[Vec4d, HFloat])
    testSort(DataFactory[Vec4d, RFloat])
    testSort(DataFactory[Vec4d, RDouble])
  }

  test("Sort Mat3x2d") {
    testSort(DataFactory[Mat3x2d, RFloat])
    testSort(DataFactory[Mat3x2d, RDouble])
  }
}
