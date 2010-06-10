/*
 * Simplex3d, DoubleMath module
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

package simplex3d.math.doublem

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2d extends Read2[Double] {

  private[math] type R2 = ConstVec2d
  private[math] type R3 = ConstVec3d
  private[math] type R4 = ConstVec4d
  
  protected final def make2(x: Double, y: Double) =
    new ConstVec2d(x, y)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3d(x, y, z)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4d(x, y, z, w)

  private[math] final def bx: Boolean = bool(x)
  private[math] final def by: Boolean = bool(y)

  private[math] final def ix: Int = int(x)
  private[math] final def iy: Int = int(y)

  private[math] final def fx: Float = float(x)
  private[math] final def fy: Float = float(y)

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y


  def x: Double
  def y: Double

  def r = x
  def g = y

  def s = x
  def t = y

  
  final def apply(i: Int) :Double = {
    i match {
      case 0 => x
      case 1 => y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  final def unary_+() :AnyVec2d = this
  final def unary_-() = new Vec2d(-x, -y)
  final def *(s: Double) = new Vec2d(x * s, y * s)
  final def /(s: Double) = { val inv = 1/s; new Vec2d(x * inv, y * inv) }

  final def +(s: Double) = new Vec2d(x + s, y + s)
  final def -(s: Double) = new Vec2d(x - s, y - s)

  private[math] final def divideByComponent(s: Double) = new Vec2d(s / x, s / y)

  final def +(u: inVec2d) = new Vec2d(x + u.x, y + u.y)
  final def -(u: inVec2d) = new Vec2d(x - u.x, y - u.y)
  final def *(u: inVec2d) = new Vec2d(x * u.x, y * u.y)
  final def /(u: inVec2d) = new Vec2d(x / u.x, y / u.y)

  final def *(m: inMat2d) :Vec2d = m.transposeMul(this)
  final def *(m: inMat2x3d) :Vec3d = m.transposeMul(this)
  final def *(m: inMat2x4d) :Vec4d = m.transposeMul(this)

  final def ==(u: inVec2d) :Boolean = {
    if (u eq null) false
    else x == u.x && y == u.y
  }

  final def !=(u: inVec2d) :Boolean = !(this == u)

  private[math] final def hasErrors: Boolean = {
    import java.lang.Double._
    (
      isNaN(x) || isInfinite(x) ||
      isNaN(y) || isInfinite(y)
    )
  }

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: inVec2d => this == u
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
final class ConstVec2d private[math] (val x: Double, val y: Double)
extends AnyVec2d with Immutable

object ConstVec2d {
  /* main factory */ def apply(x: Double, y: Double) = new ConstVec2d(x, y)
  def apply(u: Read2[_]) = new ConstVec2d(u.dx, u.dy)

  implicit def toConst(u: AnyVec2d) = new ConstVec2d(u.x, u.y)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec2d private[math] (var x: Double, var y: Double)
extends AnyVec2d with Mutable with Implicits[On] with Composite
{
  type Element = AnyVec2d
  type Component = Double1

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

  def +=(u: inVec2d) { x += u.x; y += u.y }
  def -=(u: inVec2d) { x -= u.x; y -= u.y }
  def *=(u: inVec2d) { x *= u.x; y *= u.y }
  def /=(u: inVec2d) { x /= u.x; y /= u.y }

  def *=(m: inMat2d) { this := m.transposeMul(this) }

  def :=(u: inVec2d) { x = u.x; y = u.y }
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


  def xy_=(u: inVec2d) { x = u.x; y = u.y }
  def yx_=(u: inVec2d) { var t = u.y; y = u.x; x = t }

  def rg_=(u: inVec2d) { xy_=(u) }
  def gr_=(u: inVec2d) { yx_=(u) }

  def st_=(u: inVec2d) { xy_=(u) }
  def ts_=(u: inVec2d) { yx_=(u) }
}

object Vec2d {
  val Zero = new ConstVec2d(0, 0)
  val UnitX = new ConstVec2d(1, 0)
  val UnitY = new ConstVec2d(0, 1)
  val One = new ConstVec2d(1, 1)

  def apply(s: Double) = new Vec2d(s, s)
  /* main factory */ def apply(x: Double, y: Double) = new Vec2d(x, y)
  def apply(u: Read2[_]) = new Vec2d(u.dx, u.dy)
  def apply(u: Read3[_]) = new Vec2d(u.dx, u.dy)
  def apply(u: Read4[_]) = new Vec2d(u.dx, u.dy)

  def unapply(u: AnyVec2d) = Some((u.x, u.y))

  implicit def toMutable(u: AnyVec2d) = new Vec2d(u.x, u.y)
  implicit def castInt(u: Read2[Int]) = new Vec2d(u.dx, u.dy)
  implicit def castFloat(u: Read2[Float]) = new Vec2d(u.dx, u.dy)
}
