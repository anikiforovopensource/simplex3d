/*
 * Simplex3d, DoubleMath module
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

package simplex3d.math.doublem

import simplex3d.math._
import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2d extends Read2[Double] {

  private[math] type R2 = ConstVec2d
  private[math] type R3 = ConstVec3d
  private[math] type R4 = ConstVec4d
  
  protected def make2(x: Double, y: Double) =
    new ConstVec2d(x, y)
  protected def make3(x: Double, y: Double, z: Double) =
    new ConstVec3d(x, y, z)
  protected def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4d(x, y, z, w)

  private[math] def bx: Boolean = bool(x)
  private[math] def by: Boolean = bool(y)

  private[math] def ix: Int = int(x)
  private[math] def iy: Int = int(y)

  private[math] def fx: Float = float(x)
  private[math] def fy: Float = float(y)

  private[math] def dx: Double = x
  private[math] def dy: Double = y


  def x: Double
  def y: Double

  def r = x
  def g = y

  def s = x
  def t = y

  
  def apply(i: Int) :Double = {
    i match {
      case 0 => x
      case 1 => y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  def unary_+() :this.type = this
  def unary_-() = new Vec2d(-x, -y)
  def *(s: Double) = new Vec2d(x * s, y * s)
  def /(s: Double) = { val inv = 1/s; new Vec2d(x * inv, y * inv) }

  def +(s: Double) = new Vec2d(x + s, y + s)
  def -(s: Double) = new Vec2d(x - s, y - s)

  private[math] def divideByComponent(s: Double) = new Vec2d(s / x, s / y)

  def +(u: AnyVec2d) = new Vec2d(x + u.x, y + u.y)
  def -(u: AnyVec2d) = new Vec2d(x - u.x, y - u.y)
  def *(u: AnyVec2d) = new Vec2d(x * u.x, y * u.y)
  def /(u: AnyVec2d) = new Vec2d(x / u.x, y / u.y)

  def *(m: AnyMat2d) :Vec2d = m.transposeMul(this)
  def *(m: AnyMat2x3d) :Vec3d = m.transposeMul(this)
  def *(m: AnyMat2x4d) :Vec4d = m.transposeMul(this)

  def ==(u: AnyVec2d) :Boolean = {
    if (u eq null) false
    else x == u.x && y == u.y
  }

  def !=(u: AnyVec2d) :Boolean = !(this == u)

  private[math] def hasErrors: Boolean = {
    import java.lang.Double._
    (
      isNaN(x) || isInfinite(x) ||
      isNaN(y) || isInfinite(y)
    )
  }

  override def equals(other: Any) :Boolean = {
    other match {
      case u: AnyVec2d => this == u
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

final class ConstVec2d private[math] (val x: Double, val y: Double)
extends AnyVec2d with ConstVec[Double]

object ConstVec2d {
  def apply(x: Double, y: Double) = new ConstVec2d(x, y)
  def apply(u: Read2[_]) = new ConstVec2d(u.dx, u.dy)

  implicit def toConst(u: AnyVec2d) = new ConstVec2d(u.x, u.y)
}


final class Vec2d private[math] (var x: Double, var y: Double)
extends AnyVec2d with Vec[Double]
{
  override def r = x
  override def g = y

  override def s = x
  override def t = y

  def r_=(r: Double) { x = r }
  def g_=(g: Double) { y = g }

  def s_=(s: Double) { x = s }
  def t_=(t: Double) { y = t }

  
  def *=(s: Double) { x *= s; y *= s }
  def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv }

  def +=(s: Double) { x += s; y += s }
  def -=(s: Double) { x -= s; y -= s }

  def +=(u: AnyVec2d) { x += u.x; y += u.y }
  def -=(u: AnyVec2d) { x -= u.x; y -= u.y }
  def *=(u: AnyVec2d) { x *= u.x; y *= u.y }
  def /=(u: AnyVec2d) { x /= u.x; y /= u.y }

  def *=(m: AnyMat2d) { this := m.transposeMul(this) }

  def :=(u: AnyVec2d) { x = u.x; y = u.y }
  def set(x: Double, y: Double) { this.x = x; this.y = y }

  def update(i: Int, s: Double) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  // Swizzling
  override def xy: ConstVec2d = new ConstVec2d(x, y)
  override def yx: ConstVec2d = new ConstVec2d(y, x)

  override def rg = xy
  override def gr = yx

  override def st = xy
  override def ts = yx


  def xy_=(u: AnyVec2d) { x = u.x; y = u.y }
  def yx_=(u: AnyVec2d) { var t = u.y; y = u.x; x = t }

  def rg_=(u: AnyVec2d) { xy_=(u) }
  def gr_=(u: AnyVec2d) { yx_=(u) }

  def st_=(u: AnyVec2d) { xy_=(u) }
  def ts_=(u: AnyVec2d) { yx_=(u) }
}

object Vec2d {
  val Zero = new ConstVec2d(0, 0)
  val UnitX = new ConstVec2d(1, 0)
  val UnitY = new ConstVec2d(0, 1)
  val One = new ConstVec2d(1, 1)

  def apply(s: Double) = new Vec2d(s, s)
  def apply(x: Double, y: Double) = new Vec2d(x, y)
  def apply(u: Read2[_]) = new Vec2d(u.dx, u.dy)
  def apply(u: Read3[_]) = new Vec2d(u.dx, u.dy)
  def apply(u: Read4[_]) = new Vec2d(u.dx, u.dy)

  def unapply(u: AnyVec2d) = Some((u.x, u.y))

  implicit def toMutable(u: AnyVec2d) = new Vec2d(u.x, u.y)
  implicit def castInt(u: Read2[Int]) = new Vec2d(u.dx, u.dy)
  implicit def castFloat(u: Read2[Float]) = new Vec2d(u.dx, u.dy)
}
