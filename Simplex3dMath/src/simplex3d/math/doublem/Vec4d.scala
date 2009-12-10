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
sealed abstract class AnyVec4d extends Read4Double {

    def x: Double
    def y: Double
    def z: Double
    def w: Double

    def r = x
    def g = y
    def b = z
    def a = w

    def s = x
    def t = y
    def p = z
    def q = w


    def apply(i: Int) :Double = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case 3 => w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    def unary_-() = Vec4d(-x, -y, -z, -w)
    def *(s: Double) = Vec4d(x*s, y*s, z*s, w*s)
    def /(s: Double) = { val inv = 1/s; Vec4d(x*inv, y*inv, z*inv, w*inv) }
    private[math] def divideByComponent(s: Double) = Vec4d(s/x, s/y, s/z, s/w)

    def +(u: AnyVec4d) = Vec4d(x + u.x, y + u.y, z + u.z, w + u.w)
    def -(u: AnyVec4d) = Vec4d(x - u.x, y - u.y, z - u.z, w - u.w)
    def *(u: AnyVec4d) = Vec4d(x * u.x, y * u.y, z * u.z, w * u.w)
    def /(u: AnyVec4d) = Vec4d(x / u.x, y / u.y, z / u.z, w / u.w)

    def *(m: AnyMat4x2d) :Vec2d = m.transposeMul(this)
    def *(m: AnyMat4x3d) :Vec3d = m.transposeMul(this)
    def *(m: AnyMat4d) :Vec4d = m.transposeMul(this)

    def ==(u: AnyVec4d) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4d) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y) ||
            isNaN(z) || isInfinite(z) ||
            isNaN(w) || isInfinite(w)
        )
    }
    
    override def toString = {
        this.getClass.getSimpleName +
        "(" + x + ", " + y + ", " + z + ", " + w + ")"
    }
}

final class ConstVec4d private[math] (
    val x: Double, val y: Double, val z: Double, val w: Double
) extends AnyVec4d

object ConstVec4d {
    def apply(x: Double, y: Double, z: Double, w: Double) = {
        new ConstVec4d(x, y, z, w)
    }
    def apply(u: AnyVec4d) = new ConstVec4d(u.x, u.y, u.z, u.w)

    implicit def mutableToConst(u: Vec4d) = new ConstVec4d(u.x, u.y, u.z, u.w)
    implicit def constVec4dToSwizzled(u: ConstVec4d) = new ConstVec4dSwizzled(u)
}


final class Vec4d private[math] (
    var x: Double, var y: Double, var z: Double, var w: Double
) extends AnyVec4d
{
    override def r = x
    override def g = y
    override def b = z
    override def a = w

    override def s = x
    override def t = y
    override def p = z
    override def q = w

    def r_=(r: Double) { x = r }
    def g_=(g: Double) { y = g }
    def b_=(b: Double) { z = b }
    def a_=(a: Double) { w = a }

    def s_=(s: Double) { x = s }
    def t_=(t: Double) { y = t }
    def p_=(p: Double) { z = p }
    def q_=(q: Double) { w = q }


    def *=(s: Double) { x *= s; y *= s; z *= s; w *= s }
    def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }

    def +=(u: AnyVec4d) { x += u.x; y += u.y; z += u.z; w += u.w }
    def -=(u: AnyVec4d) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
    def *=(u: AnyVec4d) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
    def /=(u: AnyVec4d) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }

    def *=(m: AnyMat4d) { this := m.transposeMul(this) }

    def :=(u: AnyVec4d) { x = u.x; y = u.y; z = u.z; w = u.w }
    def set(x: Double, y: Double, z: Double, w: Double) {
        this.x = x; this.y = y; this.z = z; this.w = w
    }

    def update(i: Int, s: Double) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case 3 => w = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }
}

object Vec4d {
    val Origin = new ConstVec4d(0, 0, 0, 0)
    val UnitX = new ConstVec4d(1, 0, 0, 0)
    val UnitY = new ConstVec4d(0, 1, 0, 0)
    val UnitZ = new ConstVec4d(0, 0, 1, 0)
    val UnitW = new ConstVec4d(0, 0, 0, 1)

    def apply(s: Double) =
        new Vec4d(s, s, s, s)

    def apply(x: Double, y: Double, z: Double, w: Double) =
        new Vec4d(x, y, z, w)

    def apply(u: AnyVec4d) =
        new Vec4d(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2d, z: Double, w: Double) =
        new Vec4d(xy.x, xy.y, z, w)

    def apply(x: Double, yz: AnyVec2d, w: Double) =
        new Vec4d(x, yz.x, yz.y, w)

    def apply(x: Double, y: Double, zw: AnyVec2d) =
        new Vec4d(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2d, zw: AnyVec2d) =
        new Vec4d(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3d, w: Double) =
        new Vec4d(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Double, yzw: AnyVec3d) =
        new Vec4d(x, yzw.x, yzw.y, yzw.z)

    def apply(m: AnyMat2d) =
        new Vec4d(m.m00, m.m10, m.m01, m.m11)

    def apply(u: Read4Int) =
        new Vec4d(u.x, u.y, u.z, u.w)

    def apply(u: Read4Float) =
        new Vec4d(u.x, u.y, u.z, u.w)

    implicit def constToMutable(u: ConstVec4d) = Vec4d(u)
    implicit def vec4ToSwizzled(u: Vec4d) = new Vec4dSwizzled(u)
}

private[math] class ConstVec4dSwizzled(u: AnyVec4d)
extends ConstVec3dSwizzled(null)
with Swizzle4Read[Double, ConstVec2d, ConstVec3d, ConstVec4d]
{
    override def x = u.x
    override def y = u.y
    override def z = u.z
    def w = u.w
}

private[math] class Vec4dSwizzled(u: Vec4d) extends ConstVec4dSwizzled(u)
with Swizzle4Write[Double, ConstVec2d, ConstVec3d, ConstVec4d]
{
    def x_=(x: Double) { u.x = x }
    def y_=(y: Double) { u.y = y }
    def z_=(z: Double) { u.z = z }
    def w_=(w: Double) { u.w = w }
}
