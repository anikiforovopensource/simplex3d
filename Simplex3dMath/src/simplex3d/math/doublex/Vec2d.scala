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


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec2d extends ProtectedVec2d[Double]
with ReadPropertyRef[ReadVec2d] with Serializable
{

  type Clone <: ReadVec2d
  type Const = ConstVec2d
  type Mutable = Vec2d
  def toConst() :ConstVec2d
  final def mutableCopy() = Vec2d(this)

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

  private[math] final def bx: Boolean = simplex3d.math.Boolean(x)
  private[math] final def by: Boolean = simplex3d.math.Boolean(y)

  private[math] final def ix: Int = x.toInt
  private[math] final def iy: Int = y.toInt

  private[math] final def fx: Float = x.toFloat
  private[math] final def fy: Float = y.toFloat

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y


  final def x = px
  final def y = py

  final def r = px
  final def g = py

  final def s = px
  final def t = py


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
          "Trying to read index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  final def unary_+() :ReadVec2d = this
  final def unary_-() = new Vec2d(-x, -y)

  final def *(s: Double) = new Vec2d(x * s, y * s)
  final def /(s: Double) = new Vec2d(x / s, y / s)
  private[math] final def divByComp(s: Double) = new Vec2d(s / x, s / y)
  final def +(s: Double) = new Vec2d(x + s, y + s)
  final def -(s: Double) = new Vec2d(x - s, y - s)

  final def *(u: inVec2d) = new Vec2d(x * u.x, y * u.y)
  final def /(u: inVec2d) = new Vec2d(x / u.x, y / u.y)
  final def +(u: inVec2d) = new Vec2d(x + u.x, y + u.y)
  final def -(u: inVec2d) = new Vec2d(x - u.x, y - u.y)

  final def *(m: inMat2d) :Vec2d = m.transposeMult(this)
  final def *(m: inMat2x3d) :Vec3d = m.transposeMult(this)
  final def *(m: inMat2x4d) :Vec4d = m.transposeMult(this)


  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec2b => false
      case u: AnyVec2[_] => x == u.dx && y == u.dy
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 + y.hashCode
    ) + x.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Vec2" + "(" + x + ", " + y + ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstVec2d private[math] (cx: Double, cy: Double)
extends ReadVec2d with Immutable with Serializable {
  px = cx; py = cy

  type Clone = ConstVec2d
  override def clone() = this
  def toConst() = this
}


object ConstVec2d {

  def apply(s: Double) = new ConstVec2d(s, s)
  def apply(x: Double, y: Double) = new ConstVec2d(x, y)

  def apply(u: AnyVec2[_]) = new ConstVec2d(u.dx, u.dy)
  def apply(u: AnyVec3[_]) = new ConstVec2d(u.dx, u.dy)
  def apply(u: AnyVec4[_]) = new ConstVec2d(u.dx, u.dy)

  implicit def toConst(u: ReadVec2d) = apply(u)
}


@SerialVersionUID(8104346712419693669L)
final class Vec2d private[math] (cx: Double, cy: Double)
extends ReadVec2d with Accessor with CompositeFormat
with PropertyRef[ReadVec2d] with Serializable
{
  px = cx; py = cy

  type Read = ReadVec2d

  type Accessor = Vec2d
  type Component = RDouble

  type Clone = Vec2d
  override def clone() = Vec2d(this)
  def toConst() = ConstVec2d(this)
  def :=(u: ConstVec2d) { this := u.asInstanceOf[inVec2d] }
  def :=(u: inVec2d) { x = u.x; y = u.y }


  @noinline override def x_=(s: Double) { px = s }
  @noinline override def y_=(s: Double) { py = s }

  override def r_=(s: Double) { px = s }
  override def g_=(s: Double) { py = s }

  override def s_=(s: Double) { px = s }
  override def t_=(s: Double) { py = s }

  def update(i: Int, s: Double) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case j => throw new IndexOutOfBoundsException(
          "Trying to update index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  def *=(s: Double) { x *= s; y *= s }
  def /=(s: Double) { x /= s; y /= s }
  def +=(s: Double) { x += s; y += s }
  def -=(s: Double) { x -= s; y -= s }

  def *=(u: inVec2d) { x *= u.x; y *= u.y }
  def /=(u: inVec2d) { x /= u.x; y /= u.y }
  def +=(u: inVec2d) { x += u.x; y += u.y }
  def -=(u: inVec2d) { x -= u.x; y -= u.y }

  def *=(m: inMat2d) { this := m.transposeMult(this) }

  // @SwizzlingStart
  override def xy_=(u: inVec2d) { x = u.x; y = u.y }
  override def yx_=(u: inVec2d) { var t = u.y; y = u.x; x = t }

  override def rg_=(u: inVec2d) { xy_=(u) }
  override def gr_=(u: inVec2d) { yx_=(u) }

  override def st_=(u: inVec2d) { xy_=(u) }
  override def ts_=(u: inVec2d) { yx_=(u) }
  // @SwizzlingEnd
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
  def apply(x: Double, y: Double) = new Vec2d(x, y)

  def apply(u: AnyVec2[_]) = new Vec2d(u.dx, u.dy)
  def apply(u: AnyVec3[_]) = new Vec2d(u.dx, u.dy)
  def apply(u: AnyVec4[_]) = new Vec2d(u.dx, u.dy)

  def unapply(u: ReadVec2d) = Some((u.x, u.y))
}
