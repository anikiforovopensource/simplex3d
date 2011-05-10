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
package float

import org.scalatest._
import simplex3d.math.floatx._
import simplex3d.data._
import simplex3d.data.float._
import AdapterAttribs._
import AdapterTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
class AdapterTest extends FunSuite {

  val a1 = Vec2f(2, 3)
  val a2 = Vec2f(4, 5)
  val a3 = Vec2f(6, 7)
  val a4 = Vec2f(8, 9)

  val b1 = Vec3f(2, 3, 4)
  val b2 = Vec3f(5, 6, 7)
  val b3 = Vec3f(8, 9, 10)
  val b4 = Vec3f(11, 12, 13)

  val c1 = Vec4f(2, 3, 4, 5)
  val c2 = Vec4f(6, 7, 8, 9)
  val c3 = Vec4f(10, 11, 12, 13)
  val c4 = Vec4f(14, 15, 16, 17)

  def flatten2(values: Vec2f*) = DataArray[Vec2f, RFloat](values: _*).primitives
  def flatten3(values: Vec3f*) = DataArray[Vec3f, RFloat](values: _*).primitives
  def flatten4(values: Vec4f*) = DataArray[Vec4f, RFloat](values: _*).primitives

  test("Adapters") {
    testAdapter(FactoryQuat4f)(Quat4f(c1.x, c1.y, c1.z, c1.w), flatten4(c1))
    testAdapter(FactoryMat2x2f)(Mat2x2f(a1, a2), flatten2(a1, a2))
    testAdapter(FactoryMat2x3f)(Mat2x3f(a1, a2, a3), flatten2(a1, a2, a3))
    testAdapter(FactoryMat2x4f)(Mat2x4f(a1, a2, a3, a4), flatten2(a1, a2, a3, a4))
    testAdapter(FactoryMat3x2f)(Mat3x2f(b1, b2), flatten3(b1, b2))
    testAdapter(FactoryMat3x3f)(Mat3x3f(b1, b2, b3), flatten3(b1, b2, b3))
    testAdapter(FactoryMat3x4f)(Mat3x4f(b1, b2, b3, b4), flatten3(b1, b2, b3, b4))
    testAdapter(FactoryMat4x2f)(Mat4x2f(c1, c2), flatten4(c1, c2))
    testAdapter(FactoryMat4x3f)(Mat4x3f(c1, c2, c3), flatten4(c1, c2, c3))
    testAdapter(FactoryMat4x4f)(Mat4x4f(c1, c2, c3, c4), flatten4(c1, c2, c3, c4))
  }
}
