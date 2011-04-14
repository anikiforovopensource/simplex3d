/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math.doublex

import simplex3d.math._
import simplex3d.math.doublex.functions._


/**
 * Glue code to make floats interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadDoubleRef(protected var x: Double) extends PrimitiveRef[Double] {
  type Read = ReadDoubleRef
  type Const = Double
  def toConst() :Double = x

  def apply(i: Int) :Double = {
    if (i == 0) x
    else throw new IndexOutOfBoundsException("Expected from 0 to 0, got " + i + ".")
  }

  private[math] def bx: Boolean = Boolean(x)
  private[math] def ix: Int = x.toInt
  private[math] def fx: Float = x.toFloat
  private[math] def dx: Double = x


  final override def equals(other: Any) :Boolean = {
    other match {
      case r: BooleanRef => false
      case r: PrimitiveRef[_] => dx == r.dx
      case a => x == a
    }
  }

  final override def hashCode() :Int = {
    x.hashCode
  }

  final override def toString() :String = {
    "DoubleRef" + "(" + x + ")"
  }


  def *(u: inVec2d) = u*x
  def /(u: inVec2d) = new Vec2d(x/u.x, x/u.y)
  def +(u: inVec2d) = u + x
  def -(u: inVec2d) = new Vec2d(x - u.x, x - u.y)

  def *(u: inVec3d) = u*x
  def /(u: inVec3d) = new Vec3d(x/u.x, x/u.y, x/u.z)
  def +(u: inVec3d) = u + x
  def -(u: inVec3d) = new Vec3d(x - u.x, x - u.y, x - u.z)

  def *(u: inVec4d) = u*x
  def /(u: inVec4d) = new Vec4d(x/u.x, x/u.y, x/u.z, x/u.w)
  def +(u: inVec4d) = u + x
  def -(u: inVec4d) = new Vec4d(x - u.x, x - u.y, x - u.z, x - u.w)

  def *(q: inQuat4d) = q*x
  def /(q: inQuat4d) = new Quat4d(x/q.a, x/q.b, x/q.c, x/q.d)
  def +(q: inQuat4d) = q + x
  def -(q: inQuat4d) = new Quat4d(x - q.a, x - q.b, x - q.c, x - q.d)

  def *(m: inMat2d) = m*x
  def /(m: inMat2d) = m.divByComp(x)
  def +(m: inMat2d) = m + x
  def -(m: inMat2d) = { val t = -m; t += x; t }

  def *(m: inMat2x3d) = m*x
  def /(m: inMat2x3d) = m.divByComp(x)
  def +(m: inMat2x3d) = m + x
  def -(m: inMat2x3d) = { val t = -m; t += x; t }

  def *(m: inMat2x4d) = m*x
  def /(m: inMat2x4d) = m.divByComp(x)
  def +(m: inMat2x4d) = m + x
  def -(m: inMat2x4d) = { val t = -m; t += x; t }

  def *(m: inMat3x2d) = m*x
  def /(m: inMat3x2d) = m.divByComp(x)
  def +(m: inMat3x2d) = m + x
  def -(m: inMat3x2d) = { val t = -m; t += x; t }

  def *(m: inMat3d) = m*x
  def /(m: inMat3d) = m.divByComp(x)
  def +(m: inMat3d) = m + x
  def -(m: inMat3d) = { val t = -m; t += x; t }

  def *(m: inMat3x4d) = m*x
  def /(m: inMat3x4d) = m.divByComp(x)
  def +(m: inMat3x4d) = m + x
  def -(m: inMat3x4d) = { val t = -m; t += x; t }

  def *(m: inMat4x2d) = m*x
  def /(m: inMat4x2d) = m.divByComp(x)
  def +(m: inMat4x2d) = m + x
  def -(m: inMat4x2d) = { val t = -m; t += x; t }

  def *(m: inMat4x3d) = m*x
  def /(m: inMat4x3d) = m.divByComp(x)
  def +(m: inMat4x3d) = m + x
  def -(m: inMat4x3d) = { val t = -m; t += x; t }

  def *(m: inMat4d) = m*x
  def /(m: inMat4d) = m.divByComp(x)
  def +(m: inMat4d) = m + x
  def -(m: inMat4d) = { val t = -m; t += x; t }


  // Veci to Vecd promotion
  def *(u: inVec2i) = new Vec2d(x*u.dx, x*u.dy)
  def /(u: inVec2i) = new Vec2d(x/u.dx, x/u.dy)
  def +(u: inVec2i) = new Vec2d(x + u.dx, x + u.dy)
  def -(u: inVec2i) = new Vec2d(x - u.dx, x - u.dy)

  def *(u: inVec3i) = new Vec3d(x*u.dx, x*u.dy, x*u.dz)
  def /(u: inVec3i) = new Vec3d(x/u.dx, x/u.dy, x/u.dz)
  def +(u: inVec3i) = new Vec3d(x + u.dx, x + u.dy, x + u.dz)
  def -(u: inVec3i) = new Vec3d(x - u.dx, x - u.dy, x - u.dz)

  def *(u: inVec4i) = new Vec4d(x*u.dx, x*u.dy, x*u.dz, x*u.dw)
  def /(u: inVec4i) = new Vec4d(x/u.dx, x/u.dy, x/u.dz, x/u.dw)
  def +(u: inVec4i) = new Vec4d(x + u.dx, x + u.dy, x + u.dz, x + u.dw)
  def -(u: inVec4i) = new Vec4d(x - u.dx, x - u.dy, x - u.dz, x - u.dw)


  // Vecf to Vecd promotion
  def *(u: AnyVec2[Float]) = new Vec2d(x*u.dx, x*u.dy)
  def /(u: AnyVec2[Float]) = new Vec2d(x/u.dx, x/u.dy)
  def +(u: AnyVec2[Float]) = new Vec2d(x + u.dx, x + u.dy)
  def -(u: AnyVec2[Float]) = new Vec2d(x - u.dx, x - u.dy)

  def *(u: AnyVec3[Float]) = new Vec3d(x*u.dx, x*u.dy, x*u.dz)
  def /(u: AnyVec3[Float]) = new Vec3d(x/u.dx, x/u.dy, x/u.dz)
  def +(u: AnyVec3[Float]) = new Vec3d(x + u.dx, x + u.dy, x + u.dz)
  def -(u: AnyVec3[Float]) = new Vec3d(x - u.dx, x - u.dy, x - u.dz)

  def *(u: AnyVec4[Float]) = new Vec4d(x*u.dx, x*u.dy, x*u.dz, x*u.dw)
  def /(u: AnyVec4[Float]) = new Vec4d(x/u.dx, x/u.dy, x/u.dz, x/u.dw)
  def +(u: AnyVec4[Float]) = new Vec4d(x + u.dx, x + u.dy, x + u.dz, x + u.dw)
  def -(u: AnyVec4[Float]) = new Vec4d(x - u.dx, x - u.dy, x - u.dz, x - u.dw)

  def *(q: AnyQuat4[Float]) = new Quat4d(x*q.da, x*q.db, x*q.dc, x*q.dd)
  def /(q: AnyQuat4[Float]) = new Quat4d(x/q.da, x/q.db, x/q.dc, x/q.dd)
  def +(q: AnyQuat4[Float]) = new Quat4d(x + q.da, x + q.db, x + q.dc, x + q.dd)
  def -(q: AnyQuat4[Float]) = new Quat4d(x - q.da, x - q.db, x - q.dc, x - q.dd)

  def *(m: AnyMat2[Float]) = Mat2d(m)*x
  def /(m: AnyMat2[Float]) = Mat2d(m).divByComp(x)
  def +(m: AnyMat2[Float]) = Mat2d(m) + x
  def -(m: AnyMat2[Float]) = { val t = -Mat2d(m); t += x; t }

  def *(m: AnyMat2x3[Float]) = Mat2x3d(m)*x
  def /(m: AnyMat2x3[Float]) = Mat2x3d(m).divByComp(x)
  def +(m: AnyMat2x3[Float]) = Mat2x3d(m) + x
  def -(m: AnyMat2x3[Float]) = { val t = -Mat2x3d(m); t += x; t }

  def *(m: AnyMat2x4[Float]) = Mat2x4d(m)*x
  def /(m: AnyMat2x4[Float]) = Mat2x4d(m).divByComp(x)
  def +(m: AnyMat2x4[Float]) = Mat2x4d(m) + x
  def -(m: AnyMat2x4[Float]) = { val t = -Mat2x4d(m); t += x; t }

  def *(m: AnyMat3x2[Float]) = Mat3x2d(m)*x
  def /(m: AnyMat3x2[Float]) = Mat3x2d(m).divByComp(x)
  def +(m: AnyMat3x2[Float]) = Mat3x2d(m) + x
  def -(m: AnyMat3x2[Float]) = { val t = -Mat3x2d(m); t += x; t }

  def *(m: AnyMat3[Float]) = Mat3d(m)*x
  def /(m: AnyMat3[Float]) = Mat3d(m).divByComp(x)
  def +(m: AnyMat3[Float]) = Mat3d(m) + x
  def -(m: AnyMat3[Float]) = { val t = -Mat3d(m); t += x; t }

  def *(m: AnyMat3x4[Float]) = Mat3x4d(m)*x
  def /(m: AnyMat3x4[Float]) = Mat3x4d(m).divByComp(x)
  def +(m: AnyMat3x4[Float]) = Mat3x4d(m) + x
  def -(m: AnyMat3x4[Float]) = { val t = -Mat3x4d(m); t += x; t }

  def *(m: AnyMat4x2[Float]) = Mat4x2d(m)*x
  def /(m: AnyMat4x2[Float]) = Mat4x2d(m).divByComp(x)
  def +(m: AnyMat4x2[Float]) = Mat4x2d(m) + x
  def -(m: AnyMat4x2[Float]) = { val t = -Mat4x2d(m); t += x; t }

  def *(m: AnyMat4x3[Float]) = Mat4x3d(m)*x
  def /(m: AnyMat4x3[Float]) = Mat4x3d(m).divByComp(x)
  def +(m: AnyMat4x3[Float]) = Mat4x3d(m) + x
  def -(m: AnyMat4x3[Float]) = { val t = -Mat4x3d(m); t += x; t }

  def *(m: AnyMat4[Float]) = Mat4d(m)*x
  def /(m: AnyMat4[Float]) = Mat4d(m).divByComp(x)
  def +(m: AnyMat4[Float]) = Mat4d(m) + x
  def -(m: AnyMat4[Float]) = { val t = -Mat4d(m); t += x; t }
}

final class DoubleRef(cx: Double) extends ReadDoubleRef(cx) with PropertyRef {
  type Clone = DoubleRef
  override def clone() = new DoubleRef(x)

  def :=(s: Double) { x = s }
  def :=(r: ReadDoubleRef) { x = r.toConst }


  def *=(s: Double) { x *= s }
  def /=(s: Double) { x /= s }
  def +=(s: Double) { x += s }
  def -=(s: Double) { x -= s }
}

object DoubleRef {
  def unapply(r: ReadDoubleRef) = Some(r.toConst)
  implicit def toMutable(r: ReadDoubleRef) = new DoubleRef(r.toConst)
  implicit def toMutable(x: Double) = new DoubleRef(x)
}
