/*
 * Simplex3d, FloatMath module
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

package simplex3d.math.floatx

import simplex3d.math._
import simplex3d.math.floatx.functions._


/**
 * Glue code to make floats interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadFloatRef(protected var x: Float)
extends PrimitiveRef[Float] with ReadPropertyRef[ReadFloatRef] with Serializable
{
  type Clone <: ReadFloatRef
  type Const = Float
  def toConst() :Float = x

  def apply(i: Int) :Float = {
    if (i == 0) x
    else throw new IndexOutOfBoundsException("Expected from 0 to 0, got " + i + ".")
  }


  private[math] def bx: Boolean = simplex3d.math.Boolean(x)
  private[math] def ix: Int = x.toInt
  private[math] def fx: Float = x
  private[math] def dx: Double = x


  final override def equals(other: Any) :Boolean = {
    other match {
      case r: BooleanRef => false
      case r: PrimitiveRef[_] => dx == r.dx
      case a: Float => x == a
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    x.hashCode
  }

  final override def toString() :String = {
    "FloatRef" + "(" + x + ")"
  }


  def *(u: inVec2f) = u*x
  def /(u: inVec2f) = new Vec2f(x/u.x, x/u.y)
  def +(u: inVec2f) = u + x
  def -(u: inVec2f) = new Vec2f(x - u.x, x - u.y)

  def *(u: inVec3f) = u*x
  def /(u: inVec3f) = new Vec3f(x/u.x, x/u.y, x/u.z)
  def +(u: inVec3f) = u + x
  def -(u: inVec3f) = new Vec3f(x - u.x, x - u.y, x - u.z)

  def *(u: inVec4f) = u*x
  def /(u: inVec4f) = new Vec4f(x/u.x, x/u.y, x/u.z, x/u.w)
  def +(u: inVec4f) = u + x
  def -(u: inVec4f) = new Vec4f(x - u.x, x - u.y, x - u.z, x - u.w)

  def *(q: inQuat4f) = q*x
  def /(q: inQuat4f) = new Quat4f(x/q.a, x/q.b, x/q.c, x/q.d)
  def +(q: inQuat4f) = q + x
  def -(q: inQuat4f) = new Quat4f(x - q.a, x - q.b, x - q.c, x - q.d)

  def *(m: inMat2f) = m*x
  def /(m: inMat2f) = m.divByComp(x)
  def +(m: inMat2f) = m + x
  def -(m: inMat2f) = { val t = -m; t += x; t }

  def *(m: inMat2x3f) = m*x
  def /(m: inMat2x3f) = m.divByComp(x)
  def +(m: inMat2x3f) = m + x
  def -(m: inMat2x3f) = { val t = -m; t += x; t }

  def *(m: inMat2x4f) = m*x
  def /(m: inMat2x4f) = m.divByComp(x)
  def +(m: inMat2x4f) = m + x
  def -(m: inMat2x4f) = { val t = -m; t += x; t }

  def *(m: inMat3x2f) = m*x
  def /(m: inMat3x2f) = m.divByComp(x)
  def +(m: inMat3x2f) = m + x
  def -(m: inMat3x2f) = { val t = -m; t += x; t }

  def *(m: inMat3f) = m*x
  def /(m: inMat3f) = m.divByComp(x)
  def +(m: inMat3f) = m + x
  def -(m: inMat3f) = { val t = -m; t += x; t }

  def *(m: inMat3x4f) = m*x
  def /(m: inMat3x4f) = m.divByComp(x)
  def +(m: inMat3x4f) = m + x
  def -(m: inMat3x4f) = { val t = -m; t += x; t }

  def *(m: inMat4x2f) = m*x
  def /(m: inMat4x2f) = m.divByComp(x)
  def +(m: inMat4x2f) = m + x
  def -(m: inMat4x2f) = { val t = -m; t += x; t }

  def *(m: inMat4x3f) = m*x
  def /(m: inMat4x3f) = m.divByComp(x)
  def +(m: inMat4x3f) = m + x
  def -(m: inMat4x3f) = { val t = -m; t += x; t }

  def *(m: inMat4f) = m*x
  def /(m: inMat4f) = m.divByComp(x)
  def +(m: inMat4f) = m + x
  def -(m: inMat4f) = { val t = -m; t += x; t }

  
  // Veci to Vecf promotion
  def *(u: inVec2i) = new Vec2f(x*u.fx, x*u.fy)
  def /(u: inVec2i) = new Vec2f(x/u.fx, x/u.fy)
  def +(u: inVec2i) = new Vec2f(x + u.fx, x + u.fy)
  def -(u: inVec2i) = new Vec2f(x - u.fx, x - u.fy)

  def *(u: inVec3i) = new Vec3f(x*u.fx, x*u.fy, x*u.fz)
  def /(u: inVec3i) = new Vec3f(x/u.fx, x/u.fy, x/u.fz)
  def +(u: inVec3i) = new Vec3f(x + u.fx, x + u.fy, x + u.fz)
  def -(u: inVec3i) = new Vec3f(x - u.fx, x - u.fy, x - u.fz)

  def *(u: inVec4i) = new Vec4f(x*u.fx, x*u.fy, x*u.fz, x*u.fw)
  def /(u: inVec4i) = new Vec4f(x/u.fx, x/u.fy, x/u.fz, x/u.fw)
  def +(u: inVec4i) = new Vec4f(x + u.fx, x + u.fy, x + u.fz, x + u.fw)
  def -(u: inVec4i) = new Vec4f(x - u.fx, x - u.fy, x - u.fz, x - u.fw)
}

@SerialVersionUID(8104346712419693669L)
final class FloatRef(cx: Float) extends ReadFloatRef(cx)
with PropertyRef[ReadFloatRef] with Serializable
{
  type Clone = FloatRef
  override def clone() = new FloatRef(x)

  def :=(s: Float) { x = s }
  def :=(r: ReadFloatRef) { x = r.toConst }

  
  def *=(s: Float) { x *= s }
  def /=(s: Float) { x /= s }
  def +=(s: Float) { x += s }
  def -=(s: Float) { x -= s }
}

object FloatRef {
  def unapply(r: ReadFloatRef) = Some(r.toConst)
}
