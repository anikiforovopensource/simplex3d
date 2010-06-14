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

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2i extends Read2[Int] {

  private[math] type R2 = ConstVec2i
  private[math] type R3 = ConstVec3i
  private[math] type R4 = ConstVec4i

  protected final def make2(x: Int, y: Int) =
    new ConstVec2i(x, y)
  protected final def make3(x: Int, y: Int, z: Int) =
    new ConstVec3i(x, y, z)
  protected final def make4(x: Int, y: Int, z: Int, w: Int) =
    new ConstVec4i(x, y, z, w)

  private[math] final def bx: Boolean = bool(x)
  private[math] final def by: Boolean = bool(y)

  private[math] final def ix: Int = x
  private[math] final def iy: Int = y

  private[math] final def fx: Float = x
  private[math] final def fy: Float = y

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y


  def x: Int
  def y: Int

  def r = x
  def g = y

  def s = x
  def t = y


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
final class ConstVec2i private[math] (val x: Int, val y: Int)
extends AnyVec2i with Immutable

object ConstVec2i {
  def apply(s: Int) = new ConstVec2i(s, s)
  /* main factory */ def apply(x: Int, y: Int) = new ConstVec2i(x, y)
  def apply(u: Read2[_]) = new ConstVec2i(u.ix, u.iy)
  def apply(u: Read3[_]) = new ConstVec2i(u.ix, u.iy)
  def apply(u: Read4[_]) = new ConstVec2i(u.ix, u.iy)

  implicit def toConst(u: AnyVec2i) = new ConstVec2i(u.x, u.y)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec2i private[math] (var x: Int, var y: Int)
extends AnyVec2i with Mutable with Implicits[On] with Composite
{
  type Element = AnyVec2i
  type Component = Int1

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
  def set(x: Int, y: Int) { this.x = x; this.y = y }

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
  override def xy: ConstVec2i = new ConstVec2i(x, y)
  override def yx: ConstVec2i = new ConstVec2i(y, x)

  override def rg = xy
  override def gr = yx

  override def st = xy
  override def ts = yx


  def xy_=(u: inVec2i) { x = u.x; y = u.y }
  def yx_=(u: inVec2i) { var t = u.y; y = u.x; x = t }

  def rg_=(u: inVec2i) { xy_=(u) }
  def gr_=(u: inVec2i) { yx_=(u) }

  def st_=(u: inVec2i) { xy_=(u) }
  def ts_=(u: inVec2i) { yx_=(u) }
}

object Vec2i {
  val Zero = new ConstVec2i(0, 0)
  val UnitX = new ConstVec2i(1, 0)
  val UnitY = new ConstVec2i(0, 1)
  val One = new ConstVec2i(1, 1)

  def apply(s: Int) = new Vec2i(s, s)
  /* main factory */ def apply(x: Int, y: Int) = new Vec2i(x, y)
  def apply(u: Read2[_]) = new Vec2i(u.ix, u.iy)
  def apply(u: Read3[_]) = new Vec2i(u.ix, u.iy)
  def apply(u: Read4[_]) = new Vec2i(u.ix, u.iy)

  def unapply(u: AnyVec2i) = Some((u.x, u.y))

  implicit def toMutable(u: AnyVec2i) = new Vec2i(u.x, u.y)
}
