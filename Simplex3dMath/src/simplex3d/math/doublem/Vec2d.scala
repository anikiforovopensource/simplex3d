/*
 * Simplex3D, DoubleMath module
 * Copyright (C) 2009 Simplex3D team
 *
 * This file is part of Simplex3d.
 *
 * Simplex3d is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3d is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math.doublem

import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2d extends Read2Double {

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
                    "excpected from 0 to 1, got " + j)
        }
    }

    def unary_-() = Vec2d(-x, -y)
    def *(s: Double) = Vec2d(x*s, y*s)
    def /(s: Double) = { val inv = 1/s; Vec2d(x*inv, y*inv) }
    private[math] def divideByComponent(s: Double) = Vec2d(s/x, s/y)

    def +(u: AnyVec2d) = Vec2d(x + u.x, y + u.y)
    def -(u: AnyVec2d) = Vec2d(x - u.x, y - u.y)
    def *(u: AnyVec2d) = Vec2d(x * u.x, y * u.y)
    def /(u: AnyVec2d) = Vec2d(x / u.x, y / u.y)

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

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ")"
    }
}

final class ConstVec2d private[math] (val x: Double, val y: Double)
extends AnyVec2d


final class Vec2d private[math] (var x: Double, var y: Double)
extends AnyVec2d
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
                    "excpected from 0 to 1, got " + j)
        }
    }
}

object Vec2d {
    val Origin = const(Vec2d(0))
    val UnitX = const(Vec2d(1, 0))
    val UnitY = const(Vec2d(0, 1))

    def apply(s: Double) = new Vec2d(s, s)
    def apply(x: Double, y: Double) = new Vec2d(x, y)
    def apply(u: AnyVec2d) = new Vec2d(u.x, u.y)
    def apply(u: AnyVec3d) = new Vec2d(u.x, u.y)
    def apply(u: AnyVec4d) = new Vec2d(u.x, u.y)
    def apply(u: Read2Int) = new Vec2d(u.x, u.y)
    def apply(u: Read3Int) = new Vec2d(u.x, u.y)
    def apply(u: Read4Int) = new Vec2d(u.x, u.y)
    def apply(u: Read2Float) = new Vec2d(u.x, u.y)
    def apply(u: Read3Float) = new Vec2d(u.x, u.y)
    def apply(u: Read4Float) = new Vec2d(u.x, u.y)

    implicit def constToMutable(u: ConstVec2d) = Vec2d(u)
    implicit def Vec2dToSwizzled(u: Vec2d) = new Vec2dSwizzled(u)
}

private[math] class ConstVec2dSwizzled(u: AnyVec2d) extends DoubleVecFactory
with Swizzle2Read[Double, ConstVec2d, ConstVec3d, ConstVec4d]
{
    def x = u.x
    def y = u.y
}

private[math] class Vec2dSwizzled(u: Vec2d) extends ConstVec2dSwizzled(u)
with Swizzle2Write[Double, ConstVec2d, ConstVec3d, ConstVec4d]
{
    def x_=(x: Double) { u.x = x }
    def y_=(y: Double) { u.y = y }
}
