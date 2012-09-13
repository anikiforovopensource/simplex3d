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
import CopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class CopyTest extends FunSuite {

  // All the copy tests must be run by the same thread.

  test("Copy SInt") {
    testCopy(DataFactory[SInt, UByte])
    testCopy(DataFactory[SInt, SByte])
    testCopy(DataFactory[SInt, UShort])
    testCopy(DataFactory[SInt, SShort])
    testCopy(DataFactory[SInt, UInt])
    testCopy(DataFactory[SInt, SInt])
  }

  test("Copy Vec2i") {
    testCopy(DataFactory[Vec2i, UByte])
    testCopy(DataFactory[Vec2i, SByte])
    testCopy(DataFactory[Vec2i, UShort])
    testCopy(DataFactory[Vec2i, SShort])
    testCopy(DataFactory[Vec2i, UInt])
    testCopy(DataFactory[Vec2i, SInt])
  }

  test("Copy Vec3i") {
    testCopy(DataFactory[Vec3i, UByte])
    testCopy(DataFactory[Vec3i, SByte])
    testCopy(DataFactory[Vec3i, UShort])
    testCopy(DataFactory[Vec3i, SShort])
    testCopy(DataFactory[Vec3i, UInt])
    testCopy(DataFactory[Vec3i, SInt])
  }

  test("Copy Vec4i") {
    testCopy(DataFactory[Vec4i, UByte])
    testCopy(DataFactory[Vec4i, SByte])
    testCopy(DataFactory[Vec4i, UShort])
    testCopy(DataFactory[Vec4i, SShort])
    testCopy(DataFactory[Vec4i, UInt])
    testCopy(DataFactory[Vec4i, SInt])
  }


  test("Copy RFloat") {
    testCopy(DataFactory[RFloat, UByte])
    testCopy(DataFactory[RFloat, SByte])
    testCopy(DataFactory[RFloat, UShort])
    testCopy(DataFactory[RFloat, SShort])
    testCopy(DataFactory[RFloat, UInt])
    testCopy(DataFactory[RFloat, SInt])
    testCopy(DataFactory[RFloat, HFloat])
    testCopy(DataFactory[RFloat, RFloat])
  }

  test("Copy Vec2f") {
    testCopy(DataFactory[Vec2f, UByte])
    testCopy(DataFactory[Vec2f, SByte])
    testCopy(DataFactory[Vec2f, UShort])
    testCopy(DataFactory[Vec2f, SShort])
    testCopy(DataFactory[Vec2f, UInt])
    testCopy(DataFactory[Vec2f, SInt])
    testCopy(DataFactory[Vec2f, HFloat])
    testCopy(DataFactory[Vec2f, RFloat])
  }

  test("Copy Vec3f") {
    testCopy(DataFactory[Vec3f, UByte])
    testCopy(DataFactory[Vec3f, SByte])
    testCopy(DataFactory[Vec3f, UShort])
    testCopy(DataFactory[Vec3f, SShort])
    testCopy(DataFactory[Vec3f, UInt])
    testCopy(DataFactory[Vec3f, SInt])
    testCopy(DataFactory[Vec3f, HFloat])
    testCopy(DataFactory[Vec3f, RFloat])
  }

  test("Copy Vec4f") {
    testCopy(DataFactory[Vec4f, UByte])
    testCopy(DataFactory[Vec4f, SByte])
    testCopy(DataFactory[Vec4f, UShort])
    testCopy(DataFactory[Vec4f, SShort])
    testCopy(DataFactory[Vec4f, UInt])
    testCopy(DataFactory[Vec4f, SInt])
    testCopy(DataFactory[Vec4f, HFloat])
    testCopy(DataFactory[Vec4f, RFloat])
  }

  test("Copy Mat3x2f") {
    testCopy(DataFactory[Mat3x2f, RFloat])
  }


  test("Copy RDouble") {
    testCopy(DataFactory[RDouble, UByte])
    testCopy(DataFactory[RDouble, SByte])
    testCopy(DataFactory[RDouble, UShort])
    testCopy(DataFactory[RDouble, SShort])
    testCopy(DataFactory[RDouble, UInt])
    testCopy(DataFactory[RDouble, SInt])
    testCopy(DataFactory[RDouble, HFloat])
    testCopy(DataFactory[RDouble, RFloat])
    testCopy(DataFactory[RDouble, RDouble])
  }

  test("Copy Vec2d") {
    testCopy(DataFactory[Vec2d, UByte])
    testCopy(DataFactory[Vec2d, SByte])
    testCopy(DataFactory[Vec2d, UShort])
    testCopy(DataFactory[Vec2d, SShort])
    testCopy(DataFactory[Vec2d, UInt])
    testCopy(DataFactory[Vec2d, SInt])
    testCopy(DataFactory[Vec2d, HFloat])
    testCopy(DataFactory[Vec2d, RFloat])
    testCopy(DataFactory[Vec2d, RDouble])
  }

  test("Copy Vec3d") {
    testCopy(DataFactory[Vec3d, UByte])
    testCopy(DataFactory[Vec3d, SByte])
    testCopy(DataFactory[Vec3d, UShort])
    testCopy(DataFactory[Vec3d, SShort])
    testCopy(DataFactory[Vec3d, UInt])
    testCopy(DataFactory[Vec3d, SInt])
    testCopy(DataFactory[Vec3d, HFloat])
    testCopy(DataFactory[Vec3d, RFloat])
    testCopy(DataFactory[Vec3d, RDouble])
  }

  test("Copy Vec4d") {
    testCopy(DataFactory[Vec4d, UByte])
    testCopy(DataFactory[Vec4d, SByte])
    testCopy(DataFactory[Vec4d, UShort])
    testCopy(DataFactory[Vec4d, SShort])
    testCopy(DataFactory[Vec4d, UInt])
    testCopy(DataFactory[Vec4d, SInt])
    testCopy(DataFactory[Vec4d, HFloat])
    testCopy(DataFactory[Vec4d, RFloat])
    testCopy(DataFactory[Vec4d, RDouble])
  }

  test("Copy Mat3x2d") {
    testCopy(DataFactory[Mat3x2d, RFloat])
    testCopy(DataFactory[Mat3x2d, RDouble])
  }
}
