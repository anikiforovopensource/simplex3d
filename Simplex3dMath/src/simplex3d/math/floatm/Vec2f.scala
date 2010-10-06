/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009-2010, Simplex3d Team
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

package simplex3d.math.floatm

import scala.reflect.Manifest._
import simplex3d.math.integration.buffer._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadVec2f extends ProtectedVec2f[Float]
{
  private[math] type R2 = ReadVec2f
  private[math] type R3 = ReadVec3f
  private[math] type R4 = ReadVec4f

  private[math] type C2 = ConstVec2f
  private[math] type C3 = ConstVec3f
  private[math] type C4 = ConstVec4f
  
  protected final def make2(x: Double, y: Double) =
    new ConstVec2f(x.toFloat, y.toFloat)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3f(x.toFloat, y.toFloat, z.toFloat)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4f(x.toFloat, y.toFloat, z.toFloat, w.toFloat)

  private[math] final def bx: Boolean = bool(x)
  private[math] final def by: Boolean = bool(y)

  private[math] final def ix: Int = x.toInt
  private[math] final def iy: Int = y.toInt

  private[math] final def fx: Float = x
  private[math] final def fy: Float = y

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y


  final def x = px
  final def y = py

  /** Alias for x.
   * @return component x.
   */
  final def r = x

  /** Alias for y.
   * @return component y.
   */
  final def g = y


  /** Alias for x.
   * @return component x.
   */
  final def s = x

  /** Alias for y.
   * @return component y.
   */
  final def t = y


  protected def x_=(s: Float) { throw new UnsupportedOperationException }
  protected def y_=(s: Float) { throw new UnsupportedOperationException }

  protected def r_=(s: Float) { throw new UnsupportedOperationException }
  protected def g_=(s: Float) { throw new UnsupportedOperationException }

  protected def s_=(s: Float) { throw new UnsupportedOperationException }
  protected def t_=(s: Float) { throw new UnsupportedOperationException }

  
  final def apply(i: Int) :Float = {
    i match {
      case 0 => x
      case 1 => y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  final def unary_+() :ReadVec2f = this
  final def unary_-() = new Vec2f(-x, -y)
  final def *(s: Float) = new Vec2f(x * s, y * s)
  final def /(s: Float) = { val inv = 1/s; new Vec2f(x * inv, y * inv) }

  final def +(s: Float) = new Vec2f(x + s, y + s)
  final def -(s: Float) = new Vec2f(x - s, y - s)

  private[math] final def divideByComponent(s: Float) = new Vec2f(s / x, s / y)

  final def +(u: inVec2f) = new Vec2f(x + u.x, y + u.y)
  final def -(u: inVec2f) = new Vec2f(x - u.x, y - u.y)
  final def *(u: inVec2f) = new Vec2f(x * u.x, y * u.y)
  final def /(u: inVec2f) = new Vec2f(x / u.x, y / u.y)

  final def *(m: inMat2f) :Vec2f = m.transposeMul(this)
  final def *(m: inMat2x3f) :Vec3f = m.transposeMul(this)
  final def *(m: inMat2x4f) :Vec4f = m.transposeMul(this)

  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec2b => false
      case u: AnyVec2[_] => dx == u.dx && dy == u.dy
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 + x.hashCode
    ) + y.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName + "(" + x + ", " + y + ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstVec2f private[math] (cx: Float, cy: Float)
extends ReadVec2f with Immutable {
  px = cx; py = cy

  override def clone() = this
}

object ConstVec2f {
  def apply(s: Float) = new ConstVec2f(s, s)
  /*main factory*/ def apply(x: Float, y: Float) = new ConstVec2f(x, y)
  def apply(u: AnyVec2[_]) = new ConstVec2f(u.fx, u.fy)
  def apply(u: AnyVec3[_]) = new ConstVec2f(u.fx, u.fy)
  def apply(u: AnyVec4[_]) = new ConstVec2f(u.fx, u.fy)

  implicit def toConst(u: ReadVec2f) = new ConstVec2f(u.x, u.y)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec2f private[math] (cx: Float, cy: Float)
extends ReadVec2f with Implicits[On] with Composite
{
  type Element = ReadVec2f
  type Immutable = ConstVec2f
  type Component = Float1

  px = cx; py = cy

  override def x_=(s: Float) { px = s }
  override def y_=(s: Float) { py = s }

  /** Alias for x.
   */
  override def r_=(s: Float) { x = s }

  /** Alias for y.
   */
  override def g_=(s: Float) { y = s }


  /** Alias for x.
   */
  override def s_=(s: Float) { x = s }

  /** Alias for y.
   */
  override def t_=(s: Float) { y = s }


  def *=(s: Float) { x *= s; y *= s }
  def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv }

  def +=(s: Float) { x += s; y += s }
  def -=(s: Float) { x -= s; y -= s }

  def +=(u: inVec2f) { x += u.x; y += u.y }
  def -=(u: inVec2f) { x -= u.x; y -= u.y }
  def *=(u: inVec2f) { x *= u.x; y *= u.y }
  def /=(u: inVec2f) { x /= u.x; y /= u.y }

  def *=(m: inMat2f) { this := m.transposeMul(this) }

  override def clone() = Vec2f(this)
  def :=(u: inVec2f) { x = u.x; y = u.y }

  def update(i: Int, s: Float) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  // Swizzling
  override def xy_=(u: inVec2f) { x = u.x; y = u.y }
  override def yx_=(u: inVec2f) { var t = u.y; y = u.x; x = t }

  override def rg_=(u: inVec2f) { xy_=(u) }
  override def gr_=(u: inVec2f) { yx_=(u) }

  override def st_=(u: inVec2f) { xy_=(u) }
  override def ts_=(u: inVec2f) { yx_=(u) }
}

object Vec2f {
  final val Zero = new ConstVec2f(0, 0)
  final val UnitX = new ConstVec2f(1, 0)
  final val UnitY = new ConstVec2f(0, 1)
  final val One = new ConstVec2f(1, 1)
  final val Manifest = classType[ReadVec2f](classOf[ReadVec2f])

  def apply(s: Float) = new Vec2f(s, s)
  /*main factory*/ def apply(x: Float, y: Float) = new Vec2f(x, y)
  def apply(u: AnyVec2[_]) = new Vec2f(u.fx, u.fy)
  def apply(u: AnyVec3[_]) = new Vec2f(u.fx, u.fy)
  def apply(u: AnyVec4[_]) = new Vec2f(u.fx, u.fy)

  def unapply(u: ReadVec2f) = Some((u.x, u.y))

  implicit def toMutable(u: ReadVec2f) = new Vec2f(u.x, u.y)
  implicit def castInt(u: AnyVec2[Int]) = new Vec2f(u.fx, u.fy)
}
