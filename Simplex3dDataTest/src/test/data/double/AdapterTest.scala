/*
 * Simplex3d, DataTest package
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
package double

import org.scalatest._
import simplex3d.math.doublex._
import simplex3d.data._
import simplex3d.data.double._
import AdapterAttribs._
import AdapterTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class AdapterTest extends FunSuite {

  val a1 = Vec2d(2, 3)
  val a2 = Vec2d(4, 5)
  val a3 = Vec2d(6, 7)
  val a4 = Vec2d(8, 9)

  val b1 = Vec3d(2, 3, 4)
  val b2 = Vec3d(5, 6, 7)
  val b3 = Vec3d(8, 9, 10)
  val b4 = Vec3d(11, 12, 13)

  val c1 = Vec4d(2, 3, 4, 5)
  val c2 = Vec4d(6, 7, 8, 9)
  val c3 = Vec4d(10, 11, 12, 13)
  val c4 = Vec4d(14, 15, 16, 17)

  def flatten2(values: Vec2d*) = DataArray[Vec2d, RFloat](values: _*).primitives
  def flatten3(values: Vec3d*) = DataArray[Vec3d, RFloat](values: _*).primitives
  def flatten4(values: Vec4d*) = DataArray[Vec4d, RFloat](values: _*).primitives

  test("Adapters") {
    testAdapter(FactoryQuat4d)(Quat4d(c1.x, c1.y, c1.z, c1.w), flatten4(c1))
    testAdapter(FactoryMat2x2d)(Mat2x2d(a1, a2), flatten2(a1, a2))
    testAdapter(FactoryMat2x3d)(Mat2x3d(a1, a2, a3), flatten2(a1, a2, a3))
    testAdapter(FactoryMat2x4d)(Mat2x4d(a1, a2, a3, a4), flatten2(a1, a2, a3, a4))
    testAdapter(FactoryMat3x2d)(Mat3x2d(b1, b2), flatten3(b1, b2))
    testAdapter(FactoryMat3x3d)(Mat3x3d(b1, b2, b3), flatten3(b1, b2, b3))
    testAdapter(FactoryMat3x4d)(Mat3x4d(b1, b2, b3, b4), flatten3(b1, b2, b3, b4))
    testAdapter(FactoryMat4x2d)(Mat4x2d(c1, c2), flatten4(c1, c2))
    testAdapter(FactoryMat4x3d)(Mat4x3d(c1, c2, c3), flatten4(c1, c2, c3))
    testAdapter(FactoryMat4x4d)(Mat4x4d(c1, c2, c3, c4), flatten4(c1, c2, c3, c4))
  }
}
