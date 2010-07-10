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

import scala.reflect.Manifest._
import simplex3d.math.integration._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2d extends ProtectedVec2d[Double, AnyVec2d]
{
  private[math] type R2 = AnyVec2d
  private[math] type R3 = AnyVec3d
  private[math] type R4 = AnyVec4d
  
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


  protected def x_=(s: Double) { throw new UnsupportedOperationException }
  protected def y_=(s: Double) { throw new UnsupportedOperationException }

  protected def r_=(s: Double) { throw new UnsupportedOperationException }
  protected def g_=(s: Double) { throw new UnsupportedOperationException }

  protected def s_=(s: Double) { throw new UnsupportedOperationException }
  protected def t_=(s: Double) { throw new UnsupportedOperationException }

  
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

  final def copyAsMutable() = Vec2d(this)
  final def copyAsImmutable() = ConstVec2d(this)

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: AnyVec2b => false
      case u: Read2[_, _] => dx == u.dx && dy == u.dy
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
final class ConstVec2d private[math] (cx: Double, cy: Double)
extends AnyVec2d with Immutable {
  px = cx; py = cy
}

object ConstVec2d {
  def apply(s: Double) = new ConstVec2d(s, s)
  /* main factory */ def apply(x: Double, y: Double) = new ConstVec2d(x, y)
  def apply(u: Read2[_, _]) = new ConstVec2d(u.dx, u.dy)
  def apply(u: Read3[_, _]) = new ConstVec2d(u.dx, u.dy)
  def apply(u: Read4[_, _]) = new ConstVec2d(u.dx, u.dy)

  implicit def toConst(u: AnyVec2d) = new ConstVec2d(u.x, u.y)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Vec2d private[math] (cx: Double, cy: Double)
extends AnyVec2d with MutableObject[AnyVec2d] with Implicits[On] with Composite
{
  type Element = AnyVec2d
  type Component = Double1

  px = cx; py = cy

  override def x_=(s: Double) { px = s }
  override def y_=(s: Double) { py = s }

  /** Alias for x.
   */
  override def r_=(s: Double) { x = s }

  /** Alias for y.
   */
  override def g_=(s: Double) { y = s }


  /** Alias for x.
   */
  override def s_=(s: Double) { x = s }

  /** Alias for y.
   */
  override def t_=(s: Double) { y = s }

  
  def *=(s: Double) { x *= s; y *= s }
  def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv }

  def +=(s: Double) { x += s; y += s }
  def -=(s: Double) { x -= s; y -= s }

  def +=(u: inVec2d) { x += u.x; y += u.y }
  def -=(u: inVec2d) { x -= u.x; y -= u.y }
  def *=(u: inVec2d) { x *= u.x; y *= u.y }
  def /=(u: inVec2d) { x /= u.x; y /= u.y }

  def *=(m: inMat2d) { this := m.transposeMul(this) }

  override def :=(u: inVec2d) { x = u.x; y = u.y }

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
  override def xy_=(u: inVec2d) { x = u.x; y = u.y }
  override def yx_=(u: inVec2d) { var t = u.y; y = u.x; x = t }

  override def rg_=(u: inVec2d) { xy_=(u) }
  override def gr_=(u: inVec2d) { yx_=(u) }

  override def st_=(u: inVec2d) { xy_=(u) }
  override def ts_=(u: inVec2d) { yx_=(u) }
}

object Vec2d {
  final val Zero = new ConstVec2d(0, 0)
  final val UnitX = new ConstVec2d(1, 0)
  final val UnitY = new ConstVec2d(0, 1)
  final val One = new ConstVec2d(1, 1)
  final val Manifest = classType[AnyVec2d](classOf[AnyVec2d])

  def apply(s: Double) = new Vec2d(s, s)
  /* main factory */ def apply(x: Double, y: Double) = new Vec2d(x, y)
  def apply(u: Read2[_, _]) = new Vec2d(u.dx, u.dy)
  def apply(u: Read3[_, _]) = new Vec2d(u.dx, u.dy)
  def apply(u: Read4[_, _]) = new Vec2d(u.dx, u.dy)

  def unapply(u: AnyVec2d) = Some((u.x, u.y))

  implicit def toMutable(u: AnyVec2d) = new Vec2d(u.x, u.y)
  implicit def castInt(u: Read2[Int, _]) = new Vec2d(u.dx, u.dy)
  implicit def castFloat(u: Read2[Float, _]) = new Vec2d(u.dx, u.dy)
}
