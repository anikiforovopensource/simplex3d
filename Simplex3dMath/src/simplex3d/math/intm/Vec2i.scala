/*
 * Simplex3d, IntMath module
 * Copyright (C) 2009-2010 Simplex3d Team
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

import simplex3d.math._
import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2i extends Read2[Int] {

  private[math] type R2 = ConstVec2i
  private[math] type R3 = ConstVec3i
  private[math] type R4 = ConstVec4i

  protected def make2(x: Int, y: Int) =
    new ConstVec2i(x, y)
  protected def make3(x: Int, y: Int, z: Int) =
    new ConstVec3i(x, y, z)
  protected def make4(x: Int, y: Int, z: Int, w: Int) =
    new ConstVec4i(x, y, z, w)

  private[math] def bx: Boolean = bool(x)
  private[math] def by: Boolean = bool(y)

  private[math] def ix: Int = x
  private[math] def iy: Int = y

  private[math] def fx: Float = x
  private[math] def fy: Float = y

  private[math] def dx: Double = x
  private[math] def dy: Double = y


  def x: Int
  def y: Int

  def r = x
  def g = y

  def s = x
  def t = y


  def apply(i: Int) :Int = {
    i match {
      case 0 => x
      case 1 => y
      case j => throw new IndexOutOfBoundsException(
                          "excpected from 0 to 1, got " + j)
    }
  }

  def unary_+() :this.type = this
  def unary_-() = new Vec2i(-x, -y)
  def unary_~() = new Vec2i(~x, ~y)

  def *(s: Int) = new Vec2i(x * s, y * s)
  def /(s: Int) = new Vec2i(x / s, y / s)

  def +(s: Int) = new Vec2i(x + s, y + s)
  def -(s: Int) = new Vec2i(x - s, y - s)

  private[math] def divideByComponent(s: Int) = new Vec2i(s / x, s / y)
  def %(s: Int) = new Vec2i(x % s, y % s)
  private[math] def modByComponent(s: Int) = new Vec2i(s % x, s % y)
  def >>(s: Int) = new Vec2i( x >> s, y >> s)
  def >>>(s: Int) = new Vec2i( x >>> s, y >>> s)
  def <<(s: Int) = new Vec2i( x << s, y << s)
  def &(s: Int) = new Vec2i( x & s, y & s)
  def |(s: Int) = new Vec2i( x | s, y | s)
  def ^(s: Int) = new Vec2i( x ^ s, y ^ s)

  def +(u: AnyVec2i) = new Vec2i(x + u.x, y + u.y)
  def -(u: AnyVec2i) = new Vec2i(x - u.x, y - u.y)
  def *(u: AnyVec2i) = new Vec2i(x * u.x, y * u.y)
  def /(u: AnyVec2i) = new Vec2i(x / u.x, y / u.y)
  def %(u: AnyVec2i) = new Vec2i(x % u.x, y % u.y)
  def >>(u: AnyVec2i) = new Vec2i( x >> u.x, y >> u.y)
  def >>>(u: AnyVec2i) = new Vec2i( x >>> u.x, y >>> u.y)
  def <<(u: AnyVec2i) = new Vec2i( x << u.x, y << u.y)
  def &(u: AnyVec2i) = new Vec2i( x & u.x, y & u.y)
  def |(u: AnyVec2i) = new Vec2i( x | u.x, y | u.y)
  def ^(u: AnyVec2i) = new Vec2i( x ^ u.x, y ^ u.y)

  def ==(u: AnyVec2i) :Boolean = {
    if (u eq null) false
    else x == u.x && y == u.y
  }

  def !=(u: AnyVec2i) :Boolean = !(this == u)

  override def equals(other: Any) :Boolean = {
    other match {
      case u: AnyVec2i => this == u
      case _ => false
    }
  }

  override def hashCode() :Int = {
    41 * (
      41 + x.hashCode
    ) + y.hashCode
  }

  override def toString() :String = {
    this.getClass.getSimpleName + "(" + x + ", " + y + ")"
  }
}

final class ConstVec2i private[math] (val x: Int, val y: Int)
extends AnyVec2i with ConstVec[Int]

object ConstVec2i {
  def apply(x: Int, y: Int) = new ConstVec2i(x, y)
  def apply(u: Read2[_]) = new ConstVec2i(u.ix, u.iy)

  implicit def toConst(u: AnyVec2i) = new ConstVec2i(u.x, u.y)
}


final class Vec2i private[math] (var x: Int, var y: Int)
extends AnyVec2i with Vec[Int]
{

  override def r = x
  override def g = y

  override def s = x
  override def t = y

  def r_=(r: Int) { x = r }
  def g_=(g: Int) { y = g }

  def s_=(s: Int) { x = s }
  def t_=(t: Int) { y = t }


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

  def +=(u: AnyVec2i) { x += u.x; y += u.y }
  def -=(u: AnyVec2i) { x -= u.x; y -= u.y }
  def *=(u: AnyVec2i) { x *= u.x; y *= u.y }
  def /=(u: AnyVec2i) { x /= u.x; y /= u.y }
  def %=(u: AnyVec2i) { x %= u.x; y %= u.y }
  def >>=(u: AnyVec2i) = { x >>= u.x; y >>= u.y }
  def >>>=(u: AnyVec2i) = { x >>>= u.x; y >>>= u.y }
  def <<=(u: AnyVec2i) = { x <<= u.x; y <<= u.y }
  def &=(u: AnyVec2i) = { x &= u.x; y &= u.y }
  def |=(u: AnyVec2i) = { x |= u.x; y |= u.y }
  def ^=(u: AnyVec2i) = { x ^= u.x; y ^= u.y }

  def :=(u: AnyVec2i) { x = u.x; y = u.y }
  def set(x: Int, y: Int) { this.x = x; this.y = y }

  def update(i: Int, s: Int) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case j => throw new IndexOutOfBoundsException(
                          "excpected from 0 to 1, got " + j)
    }
  }

  // Swizzling
  override def xy: ConstVec2i = new ConstVec2i(x, y)
  override def yx: ConstVec2i = new ConstVec2i(y, x)

  override def rg = xy
  override def gr = yx

  override def st = xy
  override def ts = yx


  def xy_=(u: AnyVec2i) { x = u.x; y = u.y }
  def yx_=(u: AnyVec2i) { var t = u.y; y = u.x; x = t }

  def rg_=(u: AnyVec2i) { xy_=(u) }
  def gr_=(u: AnyVec2i) { yx_=(u) }

  def st_=(u: AnyVec2i) { xy_=(u) }
  def ts_=(u: AnyVec2i) { yx_=(u) }
}

object Vec2i {
  val Zero = new ConstVec2i(0, 0)
  val UnitX = new ConstVec2i(1, 0)
  val UnitY = new ConstVec2i(0, 1)
  val One = new ConstVec2i(1, 1)

  def apply(s: Int) = new Vec2i(s, s)
  def apply(x: Int, y: Int) = new Vec2i(x, y)
  def apply(u: Read2[_]) = new Vec2i(u.ix, u.iy)
  def apply(u: Read3[_]) = new Vec2i(u.ix, u.iy)
  def apply(u: Read4[_]) = new Vec2i(u.ix, u.iy)

  def unapply(u: AnyVec2i) = Some((u.x, u.y))

  implicit def toMutable(u: AnyVec2i) = new Vec2i(u.x, u.y)
}
