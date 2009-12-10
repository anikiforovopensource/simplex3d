/*
 * Simplex3D, FloatMath module
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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec4f extends Read4Float {

    def x: Float
    def y: Float
    def z: Float
    def w: Float

    def r = x
    def g = y
    def b = z
    def a = w

    def s = x
    def t = y
    def p = z
    def q = w


    def apply(i: Int) :Float = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case 3 => w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    def unary_-() = Vec4f(-x, -y, -z, -w)
    def *(s: Float) = Vec4f(x*s, y*s, z*s, w*s)
    def /(s: Float) = { val inv = 1/s; Vec4f(x*inv, y*inv, z*inv, w*inv) }
    private[math] def divideByComponent(s: Float) = Vec4f(s/x, s/y, s/z, s/w)

    def +(u: AnyVec4f) = Vec4f(x + u.x, y + u.y, z + u.z, w + u.w)
    def -(u: AnyVec4f) = Vec4f(x - u.x, y - u.y, z - u.z, w - u.w)
    def *(u: AnyVec4f) = Vec4f(x * u.x, y * u.y, z * u.z, w * u.w)
    def /(u: AnyVec4f) = Vec4f(x / u.x, y / u.y, z / u.z, w / u.w)

    def *(m: AnyMat4x2f) :Vec2f = m.transposeMul(this)
    def *(m: AnyMat4x3f) :Vec3f = m.transposeMul(this)
    def *(m: AnyMat4f) :Vec4f = m.transposeMul(this)

    def ==(u: AnyVec4f) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4f) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._
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

final class ConstVec4f private[math] (
    val x: Float, val y: Float, val z: Float, val w: Float)
extends AnyVec4f

object ConstVec4f {
    def apply(x: Float, y: Float, z: Float, w: Float) = {
        new ConstVec4f(x, y, z, w)
    }
    def apply(u: AnyVec4f) = new ConstVec4f(u.x, u.y, u.z, u.w)

    implicit def mutableToConst(u: Vec4f) = new ConstVec4f(u.x, u.y, u.z, u.w)
    implicit def constVec4fToSwizzled(u: ConstVec4f) = new ConstVec4fSwizzled(u)
}


final class Vec4f private[math] (
    var x: Float, var y: Float, var z: Float, var w: Float)
extends AnyVec4f
{
    override def r = x
    override def g = y
    override def b = z
    override def a = w

    override def s = x
    override def t = y
    override def p = z
    override def q = w

    def r_=(r: Float) { x = r }
    def g_=(g: Float) { y = g }
    def b_=(b: Float) { z = b }
    def a_=(a: Float) { w = a }

    def s_=(s: Float) { x = s }
    def t_=(t: Float) { y = t }
    def p_=(p: Float) { z = p }
    def q_=(q: Float) { w = q }


    def *=(s: Float) { x *= s; y *= s; z *= s; w *= s }
    def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }

    def +=(u: AnyVec4f) { x += u.x; y += u.y; z += u.z; w += u.w }
    def -=(u: AnyVec4f) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
    def *=(u: AnyVec4f) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
    def /=(u: AnyVec4f) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }

    def *=(m: AnyMat4f) { this := m.transposeMul(this) }

    def :=(u: AnyVec4f) { x = u.x; y = u.y; z = u.z; w = u.w }
    def set(x: Float, y: Float, z: Float, w: Float) {
        this.x = x; this.y = y; this.z = z; this.w = w
    }

    def update(i: Int, s: Float) {
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

object Vec4f {
    val Origin = new ConstVec4f(0, 0, 0, 0)
    val UnitX = new ConstVec4f(1, 0, 0, 0)
    val UnitY = new ConstVec4f(0, 1, 0, 0)
    val UnitZ = new ConstVec4f(0, 0, 1, 0)
    val UnitW = new ConstVec4f(0, 0, 0, 1)

    def apply(s: Float) =
        new Vec4f(s, s, s, s)

    def apply(x: Float, y: Float, z: Float, w: Float) =
        new Vec4f(x, y, z, w)

    def apply(u: AnyVec4f) =
        new Vec4f(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2f, z: Float, w: Float) =
        new Vec4f(xy.x, xy.y, z, w)

    def apply(x: Float, yz: AnyVec2f, w: Float) =
        new Vec4f(x, yz.x, yz.y, w)

    def apply(x: Float, y: Float, zw: AnyVec2f) =
        new Vec4f(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2f, zw: AnyVec2f) =
        new Vec4f(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3f, w: Float) =
        new Vec4f(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Float, yzw: AnyVec3f) =
        new Vec4f(x, yzw.x, yzw.y, yzw.z)

    def apply(m: AnyMat2f) =
        new Vec4f(m.m00, m.m10, m.m01, m.m11)

    def apply(u: Read4Int) =
        new Vec4f(u.x, u.y, u.z, u.w)

    def apply(u: Read4Double) =
        new Vec4f(float(u.x), float(u.y), float(u.z), float(u.w))

    implicit def constToMutable(u: ConstVec4f) = Vec4f(u)
    implicit def vec4ToSwizzled(u: Vec4f) = new Vec4fSwizzled(u)
}

private[math] class ConstVec4fSwizzled(u: AnyVec4f)
extends ConstVec3fSwizzled(null)
with Swizzle4Read[Float, ConstVec2f, ConstVec3f, ConstVec4f]
{
    override def x = u.x
    override def y = u.y
    override def z = u.z
    def w = u.w
}

private[math] class Vec4fSwizzled(u: Vec4f) extends ConstVec4fSwizzled(u)
with Swizzle4Write[Float, ConstVec2f, ConstVec3f, ConstVec4f]
{
    def x_=(x: Float) { u.x = x }
    def y_=(y: Float) { u.y = y }
    def z_=(z: Float) { u.z = z }
    def w_=(w: Float) { u.w = w }
}
