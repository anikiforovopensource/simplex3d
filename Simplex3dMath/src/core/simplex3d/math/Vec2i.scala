/*
 * Simplex3dMath - Core Module
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

import scala.reflect.ClassManifest.{classType}
import simplex3d.math.integration._
import simplex3d.math.types._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec2i extends ProtectedVec2i[Int]
with Protected with Serializable
{

  type Clone <: ReadVec2i
  def toConst() :ConstVec2i
  
  type Read = ReadVec2i
  type Mutable = Vec2i
  final def readType: Class[Read] = classOf[ReadVec2i]
  final def mutableCopy() = Vec2i(this)

  private[math] type R2 = ReadVec2i
  private[math] type R3 = ReadVec3i
  private[math] type R4 = ReadVec4i

  private[math] type C2 = ConstVec2i
  private[math] type C3 = ConstVec3i
  private[math] type C4 = ConstVec4i

  protected final def make2(x: Double, y: Double) =
    new ConstVec2i(x.toInt, y.toInt)
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3i(x.toInt, y.toInt, z.toInt)
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4i(x.toInt, y.toInt, z.toInt, w.toInt)

  private[math] final def bx: Boolean = simplex3d.math.toBoolean(x)
  private[math] final def by: Boolean = simplex3d.math.toBoolean(y)

  private[math] final def ix: Int = x
  private[math] final def iy: Int = y

  private[math] final def fx: Float = x
  private[math] final def fy: Float = y

  private[math] final def dx: Double = x
  private[math] final def dy: Double = y


  final def x = px
  final def y = py

  final def r = px
  final def g = py

  final def s = px
  final def t = py


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
          "Trying to read index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  final def unary_+() :ReadVec2i = this
  final def unary_-() = new Vec2i(-x, -y)

  final def *(s: Int) = new Vec2i(x * s, y * s)
  final def /(s: Int) = new Vec2i(x / s, y / s)
  private[math] final def divByComp(s: Int) = new Vec2i(s / x, s / y)
  final def +(s: Int) = new Vec2i(x + s, y + s)
  final def -(s: Int) = new Vec2i(x - s, y - s)

  final def *(u: inVec2i) = new Vec2i(x * u.x, y * u.y)
  final def /(u: inVec2i) = new Vec2i(x / u.x, y / u.y)
  final def +(u: inVec2i) = new Vec2i(x + u.x, y + u.y)
  final def -(u: inVec2i) = new Vec2i(x - u.x, y - u.y)

  final def unary_~() = new Vec2i(~x, ~y)

  final def %(s: Int) = new Vec2i(x % s, y % s)
  private[math] final def remByComp(s: Int) = new Vec2i(s % x, s % y)
  final def >>(s: Int) = new Vec2i(x >> s, y >> s)
  final def >>>(s: Int) = new Vec2i(x >>> s, y >>> s)
  final def <<(s: Int) = new Vec2i(x << s, y << s)
  final def &(s: Int) = new Vec2i(x & s, y & s)
  final def |(s: Int) = new Vec2i(x | s, y | s)
  final def ^(s: Int) = new Vec2i(x ^ s, y ^ s)

  final def %(u: inVec2i) = new Vec2i(x % u.x, y % u.y)
  final def >>(u: inVec2i) = new Vec2i(x >> u.x, y >> u.y)
  final def >>>(u: inVec2i) = new Vec2i(x >>> u.x, y >>> u.y)
  final def <<(u: inVec2i) = new Vec2i(x << u.x, y << u.y)
  final def &(u: inVec2i) = new Vec2i(x & u.x, y & u.y)
  final def |(u: inVec2i) = new Vec2i(x | u.x, y | u.y)
  final def ^(u: inVec2i) = new Vec2i(x ^ u.x, y ^ u.y)


  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec2i => x == u.x && y == u.y
      case u: ReadVec2b => false
      case u: AnyVec2[_] => dx == u.dx && dy == u.dy
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
    prefix + "Vec2i" + "(" + x + ", " + y + ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstVec2i private[math] (cx: Int, cy: Int)
extends ReadVec2i with Immutable with Serializable {
  px = cx; py = cy

  type Clone = ConstVec2i
  override def clone() = this
  def toConst() = this
}


object ConstVec2i {

  def apply(s: Int) = new ConstVec2i(s, s)
  def apply(x: Int, y: Int) = new ConstVec2i(x, y)

  def apply(u: AnyVec2[_]) = new ConstVec2i(u.ix, u.iy)
  def apply(u: AnyVec3[_]) = new ConstVec2i(u.ix, u.iy)
  def apply(u: AnyVec4[_]) = new ConstVec2i(u.ix, u.iy)

  implicit def toConst(u: ReadVec2i) = apply(u)
}


@SerialVersionUID(8104346712419693669L)
final class Vec2i private[math] (cx: Int, cy: Int)
extends ReadVec2i with Accessor with CompositeFormat
with Accessible with Serializable
{
  px = cx; py = cy

  private[math] def this() { this(0, 0) }
  
  type Clone = Vec2i

  type Const = ConstVec2i
  type Accessor = Vec2i
  type Component = SInt

  override def clone() = Vec2i(this)
  def toConst() = ConstVec2i(this)
  def :=(u: inVec2i) { x = u.x; y = u.y }


  @noinline override def x_=(s: Int) { px = s }
  @noinline override def y_=(s: Int) { py = s }

  override def r_=(s: Int) { px = s }
  override def g_=(s: Int) { py = s }

  override def s_=(s: Int) { px = s }
  override def t_=(s: Int) { py = s }

  def update(i: Int, s: Int) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case j => throw new IndexOutOfBoundsException(
          "Trying to update index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  def *=(s: Int) { x *= s; y *= s }
  def /=(s: Int) { x /= s; y /= s }
  def +=(s: Int) { x += s; y += s }
  def -=(s: Int) { x -= s; y -= s }

  def *=(u: inVec2i) { x *= u.x; y *= u.y }
  def /=(u: inVec2i) { x /= u.x; y /= u.y }
  def +=(u: inVec2i) { x += u.x; y += u.y }
  def -=(u: inVec2i) { x -= u.x; y -= u.y }

  def %=(s: Int) { x %= s; y %= s }
  def >>=(s: Int) { x >>= s; y >>= s }
  def >>>=(s: Int) { x >>>= s; y >>>= s }
  def <<=(s: Int) { x <<= s; y <<= s }
  def &=(s: Int) { x &= s; y &= s }
  def |=(s: Int) { x |= s; y |= s }
  def ^=(s: Int) { x ^= s; y ^= s }

  def %=(u: inVec2i) { x %= u.x; y %= u.y }
  def >>=(u: inVec2i) { x >>= u.x; y >>= u.y }
  def >>>=(u: inVec2i) { x >>>= u.x; y >>>= u.y }
  def <<=(u: inVec2i) { x <<= u.x; y <<= u.y }
  def &=(u: inVec2i) { x &= u.x; y &= u.y }
  def |=(u: inVec2i) { x |= u.x; y |= u.y }
  def ^=(u: inVec2i) { x ^= u.x; y ^= u.y }

  // @SwizzlingStart
  override def xy_=(u: inVec2i) { x = u.x; y = u.y }
  override def yx_=(u: inVec2i) { var t = u.y; y = u.x; x = t }

  override def rg_=(u: inVec2i) { xy_=(u) }
  override def gr_=(u: inVec2i) { yx_=(u) }

  override def st_=(u: inVec2i) { xy_=(u) }
  override def ts_=(u: inVec2i) { yx_=(u) }
  // @SwizzlingEnd
}


object Vec2i {
  final val Zero = new ConstVec2i(0, 0)
  final val UnitX = new ConstVec2i(1, 0)
  final val UnitY = new ConstVec2i(0, 1)
  final val One = new ConstVec2i(1, 1)

  final val Manifest = classType[Vec2i](classOf[Vec2i])
  final val ConstManifest = classType[ConstVec2i](classOf[ConstVec2i])
  final val ReadManifest = classType[ReadVec2i](classOf[ReadVec2i])


  def apply(s: Int) = new Vec2i(s, s)
  def apply(x: Int, y: Int) = new Vec2i(x, y)

  def apply(u: AnyVec2[_]) = new Vec2i(u.ix, u.iy)
  def apply(u: AnyVec3[_]) = new Vec2i(u.ix, u.iy)
  def apply(u: AnyVec4[_]) = new Vec2i(u.ix, u.iy)

  def unapply(u: ReadVec2i) = Some((u.x, u.y))
}
