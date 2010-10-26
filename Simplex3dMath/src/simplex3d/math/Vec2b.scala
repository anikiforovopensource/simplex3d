/*
 * Simplex3d, CoreMath module
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

package simplex3d.math

import scala.reflect.Manifest._
import simplex3d.math.integration.buffer._


/** The <code>ReadVec2b</code> class represents Boolean 2-dimensional vectors,
 * either constant or mutable.
 * <p>
 *   Boolean vectors do not contain many useful methods. You can operate on them
 *   using <code>BaseMath.any(bvec)</code>, <code>BaseMath.all(bvec)</code>,
 *   and <code>BaseMath.not(bvec)</code>.
 * </p>
 * <p>
 *   Boolean vectors are produced by relational functions in IntMath, FloatMath,
 *   and DoubleMath:
 *   <ul>
 *     <li><code>lessThan(vec1, vec2)</code></li>
 *     <li><code>lessThanEqual(vec1, vec2)</code></li>
 *     <li><code>greaterThan(vec1, vec2)</code></li>
 *     <li><code>greaterThanEqual(vec1, vec2)</code></li>
 *     <li><code>equal(vec1, vec2)</code></li>
 *     <li><code>notEqual(vec1, vec2)</code></li>
 *   </ul>
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadVec2b extends ProtectedVec2b[Boolean]
{
  private[math] type R2 = ReadVec2b
  private[math] type R3 = ReadVec3b
  private[math] type R4 = ReadVec4b

  private[math] type C2 = ConstVec2b
  private[math] type C3 = ConstVec3b
  private[math] type C4 = ConstVec4b

  protected final def make2(x: Double, y: Double) =
    new ConstVec2b(bool(x), bool(y))
  protected final def make3(x: Double, y: Double, z: Double) =
    new ConstVec3b(bool(x), bool(y), bool(z))
  protected final def make4(x: Double, y: Double, z: Double, w: Double) =
    new ConstVec4b(bool(x), bool(y), bool(z), bool(w))

  private[math] final def bx: Boolean = x
  private[math] final def by: Boolean = y

  private[math] final def ix: Int = simplex3d.math.int(x)
  private[math] final def iy: Int = simplex3d.math.int(y)

  private[math] final def fx: Float = simplex3d.math.float(x)
  private[math] final def fy: Float = simplex3d.math.float(y)

  private[math] final def dx: Double = simplex3d.math.double(x)
  private[math] final def dy: Double = simplex3d.math.double(y)


  @noinline final def x = px
  @noinline final def y = py

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


  protected def x_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def y_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def r_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def g_=(s: Boolean) { throw new UnsupportedOperationException }

  protected def s_=(s: Boolean) { throw new UnsupportedOperationException }
  protected def t_=(s: Boolean) { throw new UnsupportedOperationException }


  /** Read a component using sequence notation.
   * @param i index of the component (0 -> x, 1 -> y).
   * @return component with index i.
   * @exception IndexOutOfBoundsException if i is outside the range of [0, 1].
   */
  final def apply(i: Int) :Boolean = {
    i match {
      case 0 => x
      case 1 => y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case u: ReadVec2b => x == u.x && y == u.y
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


/** The <code>ConstVec2b</code> class represents constant Boolean 2-dimensional
 * vectors.
 * <p>
 *   Constant objects cannot be modified after creation. This makes them a good
 *   choise for sharing data in multithreaded context. While the constant
 *   objects cannot be modified themselves, a mutable reference to a constant
 *   object can be rassigned. To ensure that your value never changes you should
 *   use a constant assigned to val: <code> val c = constObject</code>
 * </p>
 * <p>
 *   All the mathematical and logical operations return mutable objects. To
 *   obtain an immutable object you can use an explicit cast
 *   <code>val c = ConstVec2(mutable)</code> or implicit cast
 *   <code>val c: ConstVec2 = mutable</code>.
 * </p>
 * <p>
 *   Methods that return a part of a bigger structure as vector should return
 *   constant objects to prevent errors when a vector is modified but the
 *   changes are not propagated to the original structure. For example, this
 *   approach is used for matrix column accessors.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(5359695191257934190L)
final class ConstVec2b private[math] (cx: Boolean, cy: Boolean)
extends ReadVec2b with Immutable {
  px = cx; py = cy

  override def clone() = this
}


/** The companion object <code>ConstVec2b</code> that contains factory methods.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object ConstVec2b {

  /** Makes a new instance of ConstVec2b with all the components initialized
   * to the specified value.
   *
   * @param s value for all components.
   * @return a new instance of ConstVec2b with all the components initialized
   *         to the specified value.
   */
  def apply(s: Boolean) = new ConstVec2b(s, s)
  
  /** Makes a new instance of ConstVec2b from the specified values.
   * @param x component x.
   * @param y component y.
   * @return a new instance of ConstVec2b with components initialized
   *         to the arguments.
   */
  /*main factory*/ def apply(x: Boolean, y: Boolean) = new ConstVec2b(x, y)

  /** Makes a new instance of ConstVec2b from a 2-dimensional vector.
   * @param u any 2-dimensional vector.
   * @return a new instance of ConstVec2b with components initialized
   *         to the components of u converted to Boolean.
   */
  def apply(u: AnyVec2[_]) = new ConstVec2b(u.bx, u.by)

  /** Makes a new instance of ConstVec2b from the first two components
   * of a 3-dimensional vector.
   *
   * @param u any 3-dimensional vector.
   * @return a new instance of ConstVec2b with components initialized
   *         to the first two components of u converted to Boolean.
   */
  def apply(u: AnyVec3[_]) = new ConstVec2b(u.bx, u.by)

  /** Makes a new instance of ConstVec2b from the first two components
   * of a 4-dimensional vector.
   *
   * @param u any 4-dimensional vector.
   * @return a new instance of ConstVec2b with components initialized
   *         to the first two components of u converted to Boolean.
   */
  def apply(u: AnyVec4[_]) = new ConstVec2b(u.bx, u.by)
  
  implicit def toConst(u: ReadVec2b) = new ConstVec2b(u.x, u.y)
}


/** The <code>Vec2b</code> class represents mutable Boolean 2-dimensional
 * vectors.
 * <p>
 *   Boolean vectors do not contain many useful methods. You can operate on them
 *   using <code>BaseMath.any(bvec)</code>, <code>BaseMath.all(bvec)</code>,
 *   and <code>BaseMath.not(bvec)</code>.
 * </p>
 * <p>
 *   Boolean vectors are produced by relational functions in IntMath, FloatMath,
 *   and DoubleMath:
 *   <ul>
 *     <li><code>lessThan(vec1, vec2)</code></li>
 *     <li><code>lessThanEqual(vec1, vec2)</code></li>
 *     <li><code>greaterThan(vec1, vec2)</code></li>
 *     <li><code>greaterThanEqual(vec1, vec2)</code></li>
 *     <li><code>equal(vec1, vec2)</code></li>
 *     <li><code>notEqual(vec1, vec2)</code></li>
 *   </ul>
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(5359695191257934190L)
final class Vec2b private[math] (cx: Boolean, cy: Boolean)
extends ReadVec2b with Implicits[On]
{
  px = cx; py = cy

  @noinline override def x_=(s: Boolean) { px = s }
  @noinline override def y_=(s: Boolean) { py = s }

  /** Alias for x.
   */
  override def r_=(s: Boolean) { x = s }

  /** Alias for y.
   */
  override def g_=(s: Boolean) { y = s }


  /** Alias for x.
   */
  override def s_=(s: Boolean) { x = s }

  /** Alias for y.
   */
  override def t_=(s: Boolean) { y = s }

  override def clone() = Vec2b(this)

  /** Set vector components to values from another vector.
   * @param u 2-dimensional Boolean vector.
   */
  def :=(u: inVec2b) { x = u.x; y = u.y }

  /** Set a component using sequence notation.
   * @param i index of the component (0 -> x, 1 -> y).
   * @param s new component value.
   * @exception IndexOutOfBoundsException if i is outside the range of [0, 1].
   */
  def update(i: Int, s: Boolean) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  // Swizzling
  override def xy_=(u: inVec2b) { x = u.x; y = u.y }
  override def yx_=(u: inVec2b) { var t = u.y; y = u.x; x = t }

  override def rg_=(u: inVec2b) { xy_=(u) }
  override def gr_=(u: inVec2b) { yx_=(u) }

  override def st_=(u: inVec2b) { xy_=(u) }
  override def ts_=(u: inVec2b) { yx_=(u) }
}


/** The companion object <code>Vec2b</code> that contains factory methods
 * and common constant.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object Vec2b {
  final val True = new ConstVec2b(true, true)
  final val False = new ConstVec2b(false, false)

  final val Manifest = classType[Vec2b](classOf[Vec2b])
  final val ConstManifest = classType[ConstVec2b](classOf[ConstVec2b])
  final val ReadManifest = classType[ReadVec2b](classOf[ReadVec2b])

  /** Makes a new instance of Vec2b with all the components initialized
   * to the specified value.
   *
   * @param s value for all components.
   * @return a new instance of Vec2b with all the components initialized
   *         to the specified value.
   */
  def apply(s: Boolean) = new Vec2b(s, s)

  /** Makes a new instance of Vec2b from the specified values.
   * @param x component x.
   * @param y component y.
   * @return a new instance of Vec2b with components initialized
   *         to the arguments.
   */
  /*main factory*/ def apply(x: Boolean, y: Boolean) = new Vec2b(x, y)

  /** Makes a new instance of Vec2b from a 2-dimensional vector.
   * @param u any 2-dimensional vector.
   * @return a new instance of Vec2b with components initialized
   *         to the components of u converted to Boolean.
   */
  def apply(u: AnyVec2[_]) = new Vec2b(u.bx, u.by)

  /** Makes a new instance of Vec2b from the first two components
   * of a 3-dimensional vector.
   *
   * @param u any 3-dimensional vector.
   * @return a new instance of Vec2b with components initialized
   *         to the first two components of u converted to Boolean.
   */
  def apply(u: AnyVec3[_]) = new Vec2b(u.bx, u.by)
  
  /** Makes a new instance of Vec2b from the first two components
   * of a 4-dimensional vector.
   *
   * @param u any 4-dimensional vector.
   * @return a new instance of Vec2b with components initialized
   *         to the first two components of u converted to Boolean.
   */
  def apply(u: AnyVec4[_]) = new Vec2b(u.bx, u.by)

  def unapply(u: ReadVec2b) = Some((u.x, u.y))

  implicit def toMutable(u: ReadVec2b) = new Vec2b(u.x, u.y)
}
