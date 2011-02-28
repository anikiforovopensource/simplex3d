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

package simplex3d.math
package doublex

import scala.reflect.ClassManifest.{classType}
import simplex3d.math.integration._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec2d extends ProtectedVec2d[Double]
{
  private[math] type R2 = ReadVec2d
  private[math] type R3 = ReadVec3d
  private[math] type R4 = ReadVec4d

  private[math] type C2 = ConstVec2d
  private[math] type C3 = ConstVec3d
  private[math] type C4 = ConstVec4d
  
  protected final def make2(x: Double, y: Double) =
    new ConstVec2d(x, y)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3d(x, y, z)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4d(x, y, z, w)

  private[math] final def bx: Boolean = Bool(x)
  private[math] final def by: Boolean = Bool(y)

  private[math] final def ix: Int = x.toInt
  private[math] final def iy: Int = y.toInt

  private[math] final def fx: Float = x.toFloat
  private[math] final def fy: Float = y.toFloat

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

  final def unary_+() :ReadVec2d = this
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
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Vec2" + "(" + x + ", " + y + ")"
  }
}


@serializable @SerialVersionUID(8104346712419693669L)
final class ConstVec2d private[math] (cx: Double, cy: Double)
extends ReadVec2d with Immutable {
  px = cx; py = cy

  override def clone() = this
}

object ConstVec2d {
  def apply(s: Double) = new ConstVec2d(s, s)
  /*main factory*/ def apply(x: Double, y: Double) = new ConstVec2d(x, y)
  def apply(u: AnyVec[_]) = new ConstVec2d(u.dx, u.dy)

  implicit def toConst(u: ReadVec2d) = new ConstVec2d(u.x, u.y)
}


@serializable @SerialVersionUID(8104346712419693669L)
final class Vec2d private[math] (cx: Double, cy: Double)
extends ReadVec2d with Implicits[On] with Composite
{
  type Read = ReadVec2d
  type Const = ConstVec2d
  type Component = RDouble

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

  override def clone() = Vec2d(this)
  def :=(u: inVec2d) { x = u.x; y = u.y }

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

  final val Manifest = classType[Vec2d](classOf[Vec2d])
  final val ConstManifest = classType[ConstVec2d](classOf[ConstVec2d])
  final val ReadManifest = classType[ReadVec2d](classOf[ReadVec2d])

  def apply(s: Double) = new Vec2d(s, s)
  /*main factory*/ def apply(x: Double, y: Double) = new Vec2d(x, y)
  def apply(u: AnyVec[_]) = new Vec2d(u.dx, u.dy)

  def unapply(u: ReadVec2d) = Some((u.x, u.y))

  implicit def toMutable(u: ReadVec2d) = new Vec2d(u.x, u.y)
  implicit def castInt(u: AnyVec2[Int]) = new Vec2d(u.dx, u.dy)
  implicit def castFloat(u: AnyVec2[Float]) = new Vec2d(u.dx, u.dy)
}
