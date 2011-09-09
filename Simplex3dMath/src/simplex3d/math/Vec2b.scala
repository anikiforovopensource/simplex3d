/*
 * Simplex3d, CoreMath module
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
import simplex3d.math.{Boolean => Bool}


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadVec2b extends ProtectedVec2b[Boolean]
with ReadPropertyRef[ReadVec2b] with Serializable
{

  type Clone <: ReadVec2b
  type Const = ConstVec2b
  type Mutable = Vec2b
  def toConst() :ConstVec2b
  final def toMutable() = Vec2b(this)

  private[math] type R2 = ReadVec2b
  private[math] type R3 = ReadVec3b
  private[math] type R4 = ReadVec4b

  private[math] type C2 = ConstVec2b
  private[math] type C3 = ConstVec3b
  private[math] type C4 = ConstVec4b

  protected final def make2(x: Double, y: Double) =
    new ConstVec2b(Bool(x), Bool(y))
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3b(Bool(x), Bool(y), Bool(z))
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4b(Bool(x), Bool(y), Bool(z), Bool(w))

  private[math] final def bx: Boolean = x
  private[math] final def by: Boolean = y

  private[math] final def ix: Int = simplex3d.math.Int(x)
  private[math] final def iy: Int = simplex3d.math.Int(y)

  private[math] final def fx: Float = simplex3d.math.Float(x)
  private[math] final def fy: Float = simplex3d.math.Float(y)

  private[math] final def dx: Double = simplex3d.math.Double(x)
  private[math] final def dy: Double = simplex3d.math.Double(y)


  final def x = px
  final def y = py

  final def r = px
  final def g = py

  final def s = px
  final def t = py


  protected def x_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def y_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def r_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def g_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def s_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def t_=(s: Boolean) { throw new UnsupportedOperationException }

  final def apply(i: Int) :Boolean = {
    i match {
      case 0 => x
      case 1 => y
      case j => throw new IndexOutOfBoundsException(
          "Trying to read index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }



  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec2b => x == u.x && y == u.y
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
    prefix + "Vec2b" + "(" + x + ", " + y + ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstVec2b private[math] (cx: Boolean, cy: Boolean)
extends ReadVec2b with Immutable with Serializable {
  px = cx; py = cy

  type Clone = ConstVec2b
  override def clone() = this
  def toConst() = this
}


object ConstVec2b {

  def apply(s: Boolean) = new ConstVec2b(s, s)
  def apply(x: Boolean, y: Boolean) = new ConstVec2b(x, y)

  def apply(u: AnyVec2[_]) = new ConstVec2b(u.bx, u.by)
  def apply(u: AnyVec3[_]) = new ConstVec2b(u.bx, u.by)
  def apply(u: AnyVec4[_]) = new ConstVec2b(u.bx, u.by)

  implicit def toConst(u: ReadVec2b) = apply(u)
}


@SerialVersionUID(8104346712419693669L)
final class Vec2b private[math] (cx: Boolean, cy: Boolean)
extends ReadVec2b with Accessor with CompositeFormat
with PropertyRef[ReadVec2b] with Serializable
{
  px = cx; py = cy

  type Read = ReadVec2b

  type Accessor = Vec2b
  type Component = Bool

  type Clone = Vec2b
  override def clone() = Vec2b(this)
  def toConst() = ConstVec2b(this)
  def :=(u: ConstVec2b) { this := u.asInstanceOf[inVec2b] }
  def :=(u: inVec2b) { x = u.x; y = u.y }


  @noinline override def x_=(s: Boolean) { px = s }
  @noinline override def y_=(s: Boolean) { py = s }

  override def r_=(s: Boolean) { px = s }
  override def g_=(s: Boolean) { py = s }

  override def s_=(s: Boolean) { px = s }
  override def t_=(s: Boolean) { py = s }

  def update(i: Int, s: Boolean) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case j => throw new IndexOutOfBoundsException(
          "Trying to update index (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }


  // @SwizzlingStart
  override def xy_=(u: inVec2b) { x = u.x; y = u.y }
  override def yx_=(u: inVec2b) { var t = u.y; y = u.x; x = t }

  override def rg_=(u: inVec2b) { xy_=(u) }
  override def gr_=(u: inVec2b) { yx_=(u) }

  override def st_=(u: inVec2b) { xy_=(u) }
  override def ts_=(u: inVec2b) { yx_=(u) }
  // @SwizzlingEnd
}


object Vec2b {
  final val True = new ConstVec2b(true, true)
  final val False = new ConstVec2b(false, false)

  final val Manifest = classType[Vec2b](classOf[Vec2b])
  final val ConstManifest = classType[ConstVec2b](classOf[ConstVec2b])
  final val ReadManifest = classType[ReadVec2b](classOf[ReadVec2b])


  def apply(s: Boolean) = new Vec2b(s, s)
  def apply(x: Boolean, y: Boolean) = new Vec2b(x, y)

  def apply(u: AnyVec2[_]) = new Vec2b(u.bx, u.by)
  def apply(u: AnyVec3[_]) = new Vec2b(u.bx, u.by)
  def apply(u: AnyVec4[_]) = new Vec2b(u.bx, u.by)

  def unapply(u: ReadVec2b) = Some((u.x, u.y))
}
