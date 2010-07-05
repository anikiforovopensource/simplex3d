/*
 * Simplex3d, IntMath module
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

package simplex3d.math.intm

import scala.reflect.Manifest._
import simplex3d.math.types._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2i extends ProtectedVec2i[Int] {

  private[math] type R2 = AnyVec2i
  private[math] type R3 = AnyVec3i
  private[math] type R4 = AnyVec4i

  protected final def make2(x: Double, y: Double) =
    new ConstVec2i(int(x), int(y))
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3i(int(x), int(y), int(z))
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4i(int(x), int(y), int(z), int(w))

  private[math] final def bx: Boolean = bool(x)
  private[math] final def by: Boolean = bool(y)

  private[math] final def ix: Int = x
  private[math] final def iy: Int = y

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


  protected def x_=(s: Int) { throw new UnsupportedOperationException }
  protected def y_=(s: Int) { throw new UnsupportedOperationException }

  protected def r_=(s: Int) { throw new UnsupportedOperationException }
  protected def g_=(s: Int) { throw new UnsupportedOperationException }

  protected def s_=(s: Int) { throw new UnsupportedOperationException }
  protected def t_=(s: Int) { throw new UnsupportedOperationException }


  final def apply(i: Int) :Int = {
    i match {
      case 0 => x
      case 1 => y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  final def unary_+() :AnyVec2i = this
  final def unary_-() = new Vec2i(-x, -y)
  final def unary_~() = new Vec2i(~x, ~y)

  final def *(s: Int) = new Vec2i(x * s, y * s)
  final def /(s: Int) = new Vec2i(x / s, y / s)

  final def +(s: Int) = new Vec2i(x + s, y + s)
  final def -(s: Int) = new Vec2i(x - s, y - s)

  private[math] final def divideByComponent(s: Int) = new Vec2i(s / x, s / y)
  final def %(s: Int) = new Vec2i(x % s, y % s)
  private[math] final def modByComponent(s: Int) = new Vec2i(s % x, s % y)
  final def >>(s: Int) = new Vec2i( x >> s, y >> s)
  final def >>>(s: Int) = new Vec2i( x >>> s, y >>> s)
  final def <<(s: Int) = new Vec2i( x << s, y << s)
  final def &(s: Int) = new Vec2i( x & s, y & s)
  final def |(s: Int) = new Vec2i( x | s, y | s)
  final def ^(s: Int) = new Vec2i( x ^ s, y ^ s)

  final def +(u: inVec2i) = new Vec2i(x + u.x, y + u.y)
  final def -(u: inVec2i) = new Vec2i(x - u.x, y - u.y)
  final def *(u: inVec2i) = new Vec2i(x * u.x, y * u.y)
  final def /(u: inVec2i) = new Vec2i(x / u.x, y / u.y)
  final def %(u: inVec2i) = new Vec2i(x % u.x, y % u.y)
  final def >>(u: inVec2i) = new Vec2i( x >> u.x, y >> u.y)
  final def >>>(u: inVec2i) = new Vec2i( x >>> u.x, y >>> u.y)
  final def <<(u: inVec2i) = new Vec2i( x << u.x, y << u.y)
  final def &(u: inVec2i) = new Vec2i( x & u.x, y & u.y)
  final def |(u: inVec2i) = new Vec2i( x | u.x, y | u.y)
  final def ^(u: inVec2i) = new Vec2i( x ^ u.x, y ^ u.y)

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: AnyVec2i => x == u.x && y == u.y
      case u: AnyVec2b => false
      case u: Read2[_] => dx == u.dx && dy == u.dy
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
final class ConstVec2i private[math] (cx: Int, cy: Int)
extends AnyVec2i with Immutable {
  px = cx; py = cy
}

object ConstVec2i {
  def apply(s: Int) = new ConstVec2i(s, s)
  /* main factory */ def apply(x: Int, y: Int) = new ConstVec2i(x, y)
  def apply(u: Read2[_]) = new ConstVec2i(u.ix, u.iy)
  def apply(u: Read3[_]) = new ConstVec2i(u.ix, u.iy)
  def apply(u: Read4[_]) = new ConstVec2i(u.ix, u.iy)

  implicit def toConst(u: AnyVec2i) = new ConstVec2i(u.x, u.y)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec2i private[math] (cx: Int, cy: Int)
extends AnyVec2i with AssignValue[AnyVec2i] with Implicits[On] with Composite
{
  type Element = AnyVec2i
  type Component = Int1

  px = cx; py = cy

  override def x_=(s: Int) { px = s }
  override def y_=(s: Int) { py = s }

  /** Alias for x.
   */
  override def r_=(s: Int) { x = s }

  /** Alias for y.
   */
  override def g_=(s: Int) { y = s }


  /** Alias for x.
   */
  override def s_=(s: Int) { x = s }

  /** Alias for y.
   */
  override def t_=(s: Int) { y = s }


  def *=(s: Int) { x *= s; y *= s }
  def /=(s: Int) { x /= s; y /= s }

  def +=(s: Int) { x += s; y += s }
  def -=(s: Int) { x -= s; y -= s }
  
  def %=(s: Int) { x %= s; y %= s }
  def >>=(s: Int) = { x >>= s; y >>= s }
  def >>>=(s: Int) = { x >>>= s; y >>>= s }
  def <<=(s: Int) = { x <<= s; y <<= s }
  def &=(s: Int) = { x &= s; y &= s }
  def |=(s: Int) = { x |= s; y |= s }
  def ^=(s: Int) = { x ^= s; y ^= s }

  def +=(u: inVec2i) { x += u.x; y += u.y }
  def -=(u: inVec2i) { x -= u.x; y -= u.y }
  def *=(u: inVec2i) { x *= u.x; y *= u.y }
  def /=(u: inVec2i) { x /= u.x; y /= u.y }
  def %=(u: inVec2i) { x %= u.x; y %= u.y }
  def >>=(u: inVec2i) = { x >>= u.x; y >>= u.y }
  def >>>=(u: inVec2i) = { x >>>= u.x; y >>>= u.y }
  def <<=(u: inVec2i) = { x <<= u.x; y <<= u.y }
  def &=(u: inVec2i) = { x &= u.x; y &= u.y }
  def |=(u: inVec2i) = { x |= u.x; y |= u.y }
  def ^=(u: inVec2i) = { x ^= u.x; y ^= u.y }

  def :=(u: inVec2i) { x = u.x; y = u.y }

  def update(i: Int, s: Int) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  // Swizzling
  override def xy_=(u: inVec2i) { x = u.x; y = u.y }
  override def yx_=(u: inVec2i) { var t = u.y; y = u.x; x = t }

  override def rg_=(u: inVec2i) { xy_=(u) }
  override def gr_=(u: inVec2i) { yx_=(u) }

  override def st_=(u: inVec2i) { xy_=(u) }
  override def ts_=(u: inVec2i) { yx_=(u) }
}

object Vec2i {
  final val Zero = new ConstVec2i(0, 0)
  final val UnitX = new ConstVec2i(1, 0)
  final val UnitY = new ConstVec2i(0, 1)
  final val One = new ConstVec2i(1, 1)
  final val Manifest = classType[AnyVec2i](classOf[AnyVec2i])

  def apply(s: Int) = new Vec2i(s, s)
  /* main factory */ def apply(x: Int, y: Int) = new Vec2i(x, y)
  def apply(u: Read2[_]) = new Vec2i(u.ix, u.iy)
  def apply(u: Read3[_]) = new Vec2i(u.ix, u.iy)
  def apply(u: Read4[_]) = new Vec2i(u.ix, u.iy)

  def unapply(u: AnyVec2i) = Some((u.x, u.y))

  implicit def toMutable(u: AnyVec2i) = new Vec2i(u.x, u.y)
}
