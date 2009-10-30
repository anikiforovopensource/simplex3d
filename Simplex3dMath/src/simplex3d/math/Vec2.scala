/*
 * Simplex3D, Math package
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * CLASSPATH EXCEPTION FOR UNMODIFIED WORK:
 * Linking this library statically or dynamically with other modules is making
 * a combined work based on this library. Thus, the terms and conditions of
 * the GNU General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce
 * an executable, regardless of the license terms of these independent modules,
 * and to copy and distribute the resulting executable under terms of your
 * choice, provided that you also meet, for each linked independent module,
 * the terms and conditions of the license of that module. An independent module
 * is a module which is not derived from or based on this library. If you modify
 * this library in any way, then this exception is null and void and no longer
 * applies, in this case delete this exception statement from your version.
 */

package simplex3d.math


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2 extends Read2[Float] {

    def x: Float
    def y: Float

    def r = x
    def g = y

    def s = x
    def t = y

    
    def apply(i: Int) :Float = {
        i match {
            case 0 => x
            case 1 => y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    def unary_-() = Vec2(-x, -y)
    def *(s: Float) = Vec2(x*s, y*s)
    def /(s: Float) = { val inv = 1/s; Vec2(x*inv, y*inv) }
    private[math] def divideByComponent(s: Float) = Vec2(s/x, s/y)

    def +(u: AnyVec2) = Vec2(x + u.x, y + u.y)
    def -(u: AnyVec2) = Vec2(x - u.x, y - u.y)
    def *(u: AnyVec2) = Vec2(x * u.x, y * u.y)
    def /(u: AnyVec2) = Vec2(x / u.x, y / u.y)

    def *(m: AnyMat2) :Vec2 = m.transposeMul(this)
    def *(m: AnyMat2x3) :Vec3 = m.transposeMul(this)
    def *(m: AnyMat2x4) :Vec4 = m.transposeMul(this)

    def ==(u: AnyVec2) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y
    }

    def !=(u: AnyVec2) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y)
        )
    }

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ")"
    }
}

final class ConstVec2 private (val x: Float, val y: Float) extends AnyVec2

object ConstVec2 {
    def apply(s: Float) = new ConstVec2(s, s)
    def apply(x: Float, y: Float) = new ConstVec2(x, y)
    def apply(u: AnyVec2) = new ConstVec2(u.x, u.y)
    def apply(u: AnyVec3) = new ConstVec2(u.x, u.y)
    def apply(u: AnyVec4) = new ConstVec2(u.x, u.y)
    def apply(u: AnyVec2i) = new ConstVec2(u.x, u.y)
    def apply(u: AnyVec3i) = new ConstVec2(u.x, u.y)
    def apply(u: AnyVec4i) = new ConstVec2(u.x, u.y)

    implicit def mutableToConst(u: Vec2) = ConstVec2(u)
    implicit def constVec2ToSwizzled(u: ConstVec2) = new ConstVec2Swizzled(u)
}

final class Vec2 private (var x: Float, var y: Float) extends AnyVec2 {

    override def r = x
    override def g = y

    override def s = x
    override def t = y

    def r_=(r: Float) { x = r }
    def g_=(g: Float) { y = g }

    def s_=(s: Float) { x = s }
    def t_=(t: Float) { y = t }

    
    def *=(s: Float) { x *= s; y *= s }
    def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv }

    def +=(u: AnyVec2) { x += u.x; y += u.y }
    def -=(u: AnyVec2) { x -= u.x; y -= u.y }
    def *=(u: AnyVec2) { x *= u.x; y *= u.y }
    def /=(u: AnyVec2) { x /= u.x; y /= u.y }

    def :=(u: AnyVec2) { x = u.x; y = u.y }
    def set(x: Float, y: Float) { this.x = x; this.y = y }

    def update(i: Int, s: Float) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }
}

object Vec2 {
    val Zero = ConstVec2(0)
    val One = ConstVec2(1)
    val UnitX = ConstVec2(1, 0)
    val UnitY = ConstVec2(0, 1)

    def apply(s: Float) = new Vec2(s, s)
    def apply(x: Float, y: Float) = new Vec2(x, y)
    def apply(u: AnyVec2) = new Vec2(u.x, u.y)
    def apply(u: AnyVec3) = new Vec2(u.x, u.y)
    def apply(u: AnyVec4) = new Vec2(u.x, u.y)
    def apply(u: AnyVec2i) = new Vec2(u.x, u.y)
    def apply(u: AnyVec3i) = new Vec2(u.x, u.y)
    def apply(u: AnyVec4i) = new Vec2(u.x, u.y)

    implicit def vec2ToSwizzled(u: Vec2) = new Vec2Swizzled(u)
}

private[math] class ConstVec2Swizzled(u: AnyVec2) extends FloatVecFactory
with Swizzle2Read[Float, ConstVec2, ConstVec3, ConstVec4]
{
    def x = u.x
    def y = u.y
}

private[math] class Vec2Swizzled(u: Vec2) extends ConstVec2Swizzled(u)
with Swizzle2Write[Float, ConstVec2, ConstVec3, ConstVec4]
{
    def x_=(x: Float) { u.x = x }
    def y_=(y: Float) { u.y = y }
}
