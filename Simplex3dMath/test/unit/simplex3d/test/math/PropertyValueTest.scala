/*
 * Simplex3dMath - Test Package
 * Copyright (C) 2011, Aleksey Nikiforov
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

package simplex3d.test.math

import scala.annotation._
import org.scalatest._
import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PropertyValueTest extends FunSuite {

  val random = new java.util.Random(1)
  def rb = random.nextBoolean
  def ri = random.nextInt
  def rf = random.nextFloat
  def rd = random.nextDouble
  def mf = ConstMat4f(rf, rf, rf, rf, rf, rf, rf, rf, rf, rf, rf, rf, rf, rf, rf, rf)
  def md = ConstMat4d(rd, rd, rd, rd, rd, rd, rd, rd, rd, rd, rd, rd, rd, rd, rd, rd)


  def testRef[W <: PropertyValue[W]](mutable: W, read: W#Read) {
    assert(!mutable.isInstanceOf[Immutable])
    assert(mutable != read)

    val clone = mutable.clone()
    assert(mutable ne clone)
    assert(mutable == clone)

    mutable := read
    assert(mutable == read)
    assert(read != clone)

    val const = mutable.toConst
    assert(
      const.isInstanceOf[Immutable] ||
      const.isInstanceOf[Boolean] ||
      const.isInstanceOf[Int] ||
      const.isInstanceOf[Float] ||
      const.isInstanceOf[Double]
    )
    assert(mutable == const)
    
    val mcopy = mutable.mutableCopy()
    assert(mcopy.isInstanceOf[Mutable])
    assert(mutable ne mcopy)
    assert(mutable == mcopy)
    
    val rcopy = read.mutableCopy()
    assert(rcopy.isInstanceOf[Mutable])
    assert(read ne rcopy)
    assert(read == rcopy)
    
    mutable match {
      case u: AnyVec[_] => testVec(u)
      case m: AnyMat[_] => testMat(m)
      case q: AnyQuat4[_] => // do nothing
    }
  }

  def testVec(u: AnyVec[_]) {
    (u: @unchecked) match {
      case p: PrimitiveRef[_] => assert(p.components == 1)
      case v: AnyVec2[_] => assert(v.components == 2)
      case v: AnyVec3[_] => assert(v.components == 3)
      case v: AnyVec4[_] => assert(v.components == 4)
    }
  }

  def testMat(m: AnyMat[_]) {
    m match {
      case n: AnyMat2x2[_] => assert(n.rows == 2 && n.columns == 2)
      case n: AnyMat2x3[_] => assert(n.rows == 2 && n.columns == 3)
      case n: AnyMat2x4[_] => assert(n.rows == 2 && n.columns == 4)
      case n: AnyMat3x2[_] => assert(n.rows == 3 && n.columns == 2)
      case n: AnyMat3x3[_] => assert(n.rows == 3 && n.columns == 3)
      case n: AnyMat3x4[_] => assert(n.rows == 3 && n.columns == 4)
      case n: AnyMat4x2[_] => assert(n.rows == 4 && n.columns == 2)
      case n: AnyMat4x3[_] => assert(n.rows == 4 && n.columns == 3)
      case n: AnyMat4x4[_] => assert(n.rows == 4 && n.columns == 4)
    }
  }


  test("Primitive Refs") {
    for (i <- 0 until 100) {
      val b = rb
      testRef[BooleanRef](new BooleanRef(b), !b)

      testRef[IntRef](new IntRef(ri), ri)

      testRef[FloatRef](new FloatRef(rf), rf)

      testRef[DoubleRef](new DoubleRef(rd), rd)
    }
  }

  test("Abstract Vectors") {
    for (i <- 0 until 100) {
      val b = rb
      testRef[Vec2b](Vec2b(b, rb), ConstVec2b(!b, rb))
      testRef[Vec3b](Vec3b(b, rb, rb), ConstVec3b(!b, rb, rb))
      testRef[Vec4b](Vec4b(b, rb, rb, rb), ConstVec4b(!b, rb, rb, rb))

      testRef[Vec2i](Vec2i(ri, ri), ConstVec2i(ri, ri))
      testRef[Vec3i](Vec3i(ri, ri, ri), ConstVec3i(ri, ri, ri))
      testRef[Vec4i](Vec4i(ri, ri, ri, ri), ConstVec4i(ri, ri, ri, ri))

      testRef[Vec2f](Vec2f(rf, rf), ConstVec2f(rf, rf))
      testRef[Vec3f](Vec3f(rf, rf, rf), ConstVec3f(rf, rf, rf))
      testRef[Vec4f](Vec4f(rf, rf, rf, rf), ConstVec4f(rf, rf, rf, rf))

      testRef[Vec2d](Vec2d(rd, rd), ConstVec2d(rd, rd))
      testRef[Vec3d](Vec3d(rd, rd, rd), ConstVec3d(rd, rd, rd))
      testRef[Vec4d](Vec4d(rd, rd, rd, rd), ConstVec4d(rd, rd, rd, rd))
    }
  }

  test("Abstract Quaternions") {
    for (i <- 0 until 100) {
      testRef[Quat4f](Quat4f(rf, rf, rf, rf), ConstQuat4f(rf, rf, rf, rf))
      testRef[Quat4d](Quat4d(rd, rd, rd, rd), ConstQuat4d(rd, rd, rd, rd))
    }
  }

  test("Abstract Matrices") {
    for (i <- 0 until 100) {
      testRef[Mat2x2f](Mat2x2f(mf), ConstMat2x2f(mf))
      testRef[Mat2x3f](Mat2x3f(mf), ConstMat2x3f(mf))
      testRef[Mat2x4f](Mat2x4f(mf), ConstMat2x4f(mf))
      testRef[Mat3x2f](Mat3x2f(mf), ConstMat3x2f(mf))
      testRef[Mat3x3f](Mat3x3f(mf), ConstMat3x3f(mf))
      testRef[Mat3x4f](Mat3x4f(mf), ConstMat3x4f(mf))
      testRef[Mat4x2f](Mat4x2f(mf), ConstMat4x2f(mf))
      testRef[Mat4x3f](Mat4x3f(mf), ConstMat4x3f(mf))
      testRef[Mat4x4f](Mat4x4f(mf), ConstMat4x4f(mf))

      testRef[Mat2x2d](Mat2x2d(md), ConstMat2x2d(md))
      testRef[Mat2x3d](Mat2x3d(md), ConstMat2x3d(md))
      testRef[Mat2x4d](Mat2x4d(md), ConstMat2x4d(md))
      testRef[Mat3x2d](Mat3x2d(md), ConstMat3x2d(md))
      testRef[Mat3x3d](Mat3x3d(md), ConstMat3x3d(md))
      testRef[Mat3x4d](Mat3x4d(md), ConstMat3x4d(md))
      testRef[Mat4x2d](Mat4x2d(md), ConstMat4x2d(md))
      testRef[Mat4x3d](Mat4x3d(md), ConstMat4x3d(md))
      testRef[Mat4x4d](Mat4x4d(md), ConstMat4x4d(md))
    }
  }
}
