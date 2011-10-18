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

package test.data

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
    testCopy(DataSeq[SInt, UByte])
    testCopy(DataSeq[SInt, SByte])
    testCopy(DataSeq[SInt, UShort])
    testCopy(DataSeq[SInt, SShort])
    testCopy(DataSeq[SInt, UInt])
    testCopy(DataSeq[SInt, SInt])
  }

  test("Copy Vec2i") {
    testCopy(DataSeq[Vec2i, UByte])
    testCopy(DataSeq[Vec2i, SByte])
    testCopy(DataSeq[Vec2i, UShort])
    testCopy(DataSeq[Vec2i, SShort])
    testCopy(DataSeq[Vec2i, UInt])
    testCopy(DataSeq[Vec2i, SInt])
  }

  test("Copy Vec3i") {
    testCopy(DataSeq[Vec3i, UByte])
    testCopy(DataSeq[Vec3i, SByte])
    testCopy(DataSeq[Vec3i, UShort])
    testCopy(DataSeq[Vec3i, SShort])
    testCopy(DataSeq[Vec3i, UInt])
    testCopy(DataSeq[Vec3i, SInt])
  }

  test("Copy Vec4i") {
    testCopy(DataSeq[Vec4i, UByte])
    testCopy(DataSeq[Vec4i, SByte])
    testCopy(DataSeq[Vec4i, UShort])
    testCopy(DataSeq[Vec4i, SShort])
    testCopy(DataSeq[Vec4i, UInt])
    testCopy(DataSeq[Vec4i, SInt])
  }


  test("Copy RFloat") {
    testCopy(DataSeq[RFloat, UByte])
    testCopy(DataSeq[RFloat, SByte])
    testCopy(DataSeq[RFloat, UShort])
    testCopy(DataSeq[RFloat, SShort])
    testCopy(DataSeq[RFloat, UInt])
    testCopy(DataSeq[RFloat, SInt])
    testCopy(DataSeq[RFloat, HFloat])
    testCopy(DataSeq[RFloat, RFloat])
  }

  test("Copy Vec2f") {
    testCopy(DataSeq[Vec2f, UByte])
    testCopy(DataSeq[Vec2f, SByte])
    testCopy(DataSeq[Vec2f, UShort])
    testCopy(DataSeq[Vec2f, SShort])
    testCopy(DataSeq[Vec2f, UInt])
    testCopy(DataSeq[Vec2f, SInt])
    testCopy(DataSeq[Vec2f, HFloat])
    testCopy(DataSeq[Vec2f, RFloat])
  }

  test("Copy Vec3f") {
    testCopy(DataSeq[Vec3f, UByte])
    testCopy(DataSeq[Vec3f, SByte])
    testCopy(DataSeq[Vec3f, UShort])
    testCopy(DataSeq[Vec3f, SShort])
    testCopy(DataSeq[Vec3f, UInt])
    testCopy(DataSeq[Vec3f, SInt])
    testCopy(DataSeq[Vec3f, HFloat])
    testCopy(DataSeq[Vec3f, RFloat])
  }

  test("Copy Vec4f") {
    testCopy(DataSeq[Vec4f, UByte])
    testCopy(DataSeq[Vec4f, SByte])
    testCopy(DataSeq[Vec4f, UShort])
    testCopy(DataSeq[Vec4f, SShort])
    testCopy(DataSeq[Vec4f, UInt])
    testCopy(DataSeq[Vec4f, SInt])
    testCopy(DataSeq[Vec4f, HFloat])
    testCopy(DataSeq[Vec4f, RFloat])
  }

  test("Copy Mat2x3f") {
    testCopy(DataSeq[Mat2x3f, RFloat])
  }


  test("Copy RDouble") {
    testCopy(DataSeq[RDouble, UByte])
    testCopy(DataSeq[RDouble, SByte])
    testCopy(DataSeq[RDouble, UShort])
    testCopy(DataSeq[RDouble, SShort])
    testCopy(DataSeq[RDouble, UInt])
    testCopy(DataSeq[RDouble, SInt])
    testCopy(DataSeq[RDouble, HFloat])
    testCopy(DataSeq[RDouble, RFloat])
    testCopy(DataSeq[RDouble, RDouble])
  }

  test("Copy Vec2d") {
    testCopy(DataSeq[Vec2d, UByte])
    testCopy(DataSeq[Vec2d, SByte])
    testCopy(DataSeq[Vec2d, UShort])
    testCopy(DataSeq[Vec2d, SShort])
    testCopy(DataSeq[Vec2d, UInt])
    testCopy(DataSeq[Vec2d, SInt])
    testCopy(DataSeq[Vec2d, HFloat])
    testCopy(DataSeq[Vec2d, RFloat])
    testCopy(DataSeq[Vec2d, RDouble])
  }

  test("Copy Vec3d") {
    testCopy(DataSeq[Vec3d, UByte])
    testCopy(DataSeq[Vec3d, SByte])
    testCopy(DataSeq[Vec3d, UShort])
    testCopy(DataSeq[Vec3d, SShort])
    testCopy(DataSeq[Vec3d, UInt])
    testCopy(DataSeq[Vec3d, SInt])
    testCopy(DataSeq[Vec3d, HFloat])
    testCopy(DataSeq[Vec3d, RFloat])
    testCopy(DataSeq[Vec3d, RDouble])
  }

  test("Copy Vec4d") {
    testCopy(DataSeq[Vec4d, UByte])
    testCopy(DataSeq[Vec4d, SByte])
    testCopy(DataSeq[Vec4d, UShort])
    testCopy(DataSeq[Vec4d, SShort])
    testCopy(DataSeq[Vec4d, UInt])
    testCopy(DataSeq[Vec4d, SInt])
    testCopy(DataSeq[Vec4d, HFloat])
    testCopy(DataSeq[Vec4d, RFloat])
    testCopy(DataSeq[Vec4d, RDouble])
  }

  test("Copy Mat2x3d") {
    testCopy(DataSeq[Mat2x3d, RFloat])
    testCopy(DataSeq[Mat2x3d, RDouble])
  }
}
