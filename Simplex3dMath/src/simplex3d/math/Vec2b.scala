/*
 * Simplex3d, BaseMath module
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

package simplex3d.math

import simplex3d.math.BaseMath._


/** The <code>AnyVec2b</code> class represents Boolean 2-dimensional vectors,
 * either constant or mutable.
 * <p>
 *   It is recommended to use this common supertype for function arguments
 *   unless you explicitly want to modify the argument vector.
 * </p>
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
sealed abstract class AnyVec2b extends Read2[Boolean] {

    private[math] type R2 = ConstVec2b
    private[math] type R3 = ConstVec3b
    private[math] type R4 = ConstVec4b

    protected def make2(x: Boolean, y: Boolean) =
        new ConstVec2b(x, y)
    protected def make3(x: Boolean, y: Boolean, z: Boolean) =
        new ConstVec3b(x, y, z)
    protected def make4(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
        new ConstVec4b(x, y, z, w)

    private[math] def bx: Boolean = x
    private[math] def by: Boolean = y

    private[math] def ix: Int = int(x)
    private[math] def iy: Int = int(y)

    private[math] def fx: Float = float(x)
    private[math] def fy: Float = float(y)

    private[math] def dx: Double = double(x)
    private[math] def dy: Double = double(y)

    
    def x: Boolean
    def y: Boolean

    /** Alias for x.
     * @return component x.
     */
    def r = x

    /** Alias for y.
     * @return component y.
     */
    def g = y


    /** Alias for x.
     * @return component x.
     */
    def s = x

    /** Alias for y.
     * @return component y.
     */
    def t = y

    
    /** Read a component using sequence notation.
     * @param i index of the component (0 -> x, 1 -> y).
     * @return component with index i.
     * @exception IndexOutOfBoundsException if i is outside the range of [0, 1].
     */
    def apply(i: Int) :Boolean = {
        i match {
            case 0 => x
            case 1 => y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    /** Component-based equality.
     * <p>
     *   Two vectors are equal if all of their components are equal.
     * </p>
     * @param u a vector for comparision.
     * @return true if all the components are equal, false otherwise.
     */
    def ==(u: AnyVec2b) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y
    }

    /** Component-based equality inverse.
     * <p>
     *   Two vectors are non-equal if any of their components are non-equal.
     * </p>
     * @param u a vector for comparision.
     * @return true if any of the components are not equal, false otherwise.
     */
    def !=(u: AnyVec2b) :Boolean = !(this == u)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec2b => this == u
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


/** Constant Boolean 2-dimensional vector.
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
final class ConstVec2b private[math] (val x: Boolean, val y: Boolean)
extends AnyVec2b


/** Factory for creating constant Boolean 2-dimensional vectors.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object ConstVec2b {

    /** Makes a new instance of ConstVec2b from the specified values.
     * @param x component x.
     * @param y component y.
     * @return a new instance of ConstVec2b with components initialized
     *         to the arguments.
     */
    def apply(x: Boolean, y: Boolean) = new ConstVec2b(x, y)

    /** Makes a new instance of ConstVec2b from a 2-dimensional vector.
     * @param u any 2-dimensional vector.
     * @return a new instance of ConstVec2b with components initialized
     *         to the components of u casted as Boolean.
     */
    def apply(u: Read2[_]) = new ConstVec2b(u.bx, u.by)
    
    implicit def toConst(u: AnyVec2b) = new ConstVec2b(u.x, u.y)
}


/** Mutable Boolean 2-dimensional vector.
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
final class Vec2b private[math] (var x: Boolean, var y: Boolean)
extends AnyVec2b
{

    /** Alias for x.
     * @return component x.
     */
    override def r = x

    /** Alias for y.
     * @return component y.
     */
    override def g = y


    /** Alias for x.
     * @return component x.
     */
    override def s = x

    /** Alias for y.
     * @return component y.
     */
    override def t = y


    /** Alias for x.
     * @return component x.
     */
    def r_=(r: Boolean) { x = r }

    /** Alias for y.
     * @return component y.
     */
    def g_=(g: Boolean) { y = g }


    /** Alias for x.
     * @return component x.
     */
    def s_=(s: Boolean) { x = s }

    /** Alias for y.
     * @return component y.
     */
    def t_=(t: Boolean) { y = t }


    /** Set vector components to values from another vector.
     * @param u 2-dimensional Boolean vector.
     */
    def :=(u: AnyVec2b) { x = u.x; y = u.y }

    /** Set vector components to the specified values.
     * @param x component x.
     * @param y component y.
     */
    def set(x: Boolean, y: Boolean) { this.x = x; this.y = y }

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
                    "excpected from 0 to 1, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2b = new ConstVec2b(x, y)
    override def yx: ConstVec2b = new ConstVec2b(y, x)

    override def rg = xy
    override def gr = yx

    override def st = xy
    override def ts = yx


    def xy_=(u: AnyVec2b) { x = u.x; y = u.y }
    def yx_=(u: AnyVec2b) { var t = u.y; y = u.x; x = t }

    def rg_=(u: AnyVec2b) { xy_=(u) }
    def gr_=(u: AnyVec2b) { yx_=(u) }

    def st_=(u: AnyVec2b) { xy_=(u) }
    def ts_=(u: AnyVec2b) { yx_=(u) }
}


/** Factory for creating mutable Boolean 2-dimensional vectors.
 * <p>
 *   To keep the code consistent all the constructors are hidden. Use the
 *   corresponding companion objects as factories to create new instances.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
object Vec2b {
    val True = new ConstVec2b(true, true)
    val False = new ConstVec2b(false, false)

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
    def apply(x: Boolean, y: Boolean) = new Vec2b(x, y)

    /** Makes a new instance of Vec2b from a 2-dimensional vector.
     * @param u any 2-dimensional vector.
     * @return a new instance of Vec2b with components initialized
     *         to the components of u casted as Boolean.
     */
    def apply(u: Read2[_]) = new Vec2b(u.bx, u.by)

    /** Makes a new instance of Vec2b from the first two components
     * of a 3-dimensional vector.
     *
     * @param u any 3-dimensional vector.
     * @return a new instance of Vec2b with components initialized
     *         to the first two components of u casted as Boolean.
     */
    def apply(u: Read3[_]) = new Vec2b(u.bx, u.by)
    
    /** Makes a new instance of Vec2b from the first two components
     * of a 4-dimensional vector.
     *
     * @param u any 4-dimensional vector.
     * @return a new instance of Vec2b with components initialized
     *         to the first two components of u casted as Boolean.
     */
    def apply(u: Read4[_]) = new Vec2b(u.bx, u.by)

    def unapply(u: AnyVec2b) = Some((u.x, u.y))

    implicit def toMutable(u: AnyVec2b) = new Vec2b(u.x, u.y)
}
