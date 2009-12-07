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
sealed abstract class AnyVec3f extends Read3Float {

    def x: Float
    def y: Float
    def z: Float

    def r = x
    def g = y
    def b = z

    def s = x
    def t = y
    def p = z

    
    def apply(i: Int) :Float = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def unary_-() = Vec3f(-x, -y, -z)
    def *(s: Float) = Vec3f(x*s, y*s, z*s)
    def /(s: Float) = { val inv = 1/s; Vec3f(x*inv, y*inv, z*inv) }
    private[math] def divideByComponent(s: Float) = Vec3f(s/x, s/y, s/z)

    def +(u: AnyVec3f) = Vec3f(x + u.x, y + u.y, z + u.z)
    def -(u: AnyVec3f) = Vec3f(x - u.x, y - u.y, z - u.z)
    def *(u: AnyVec3f) = Vec3f(x * u.x, y * u.y, z * u.z)
    def /(u: AnyVec3f) = Vec3f(x / u.x, y / u.y, z / u.z)

    def *(m: AnyMat3x2f) :Vec2f = m.transposeMul(this)
    def *(m: AnyMat3f) :Vec3f = m.transposeMul(this)
    def *(m: AnyMat3x4f) :Vec4f = m.transposeMul(this)

    def ==(u: AnyVec3f) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3f) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y) ||
            isNaN(z) || isInfinite(z)
        )
    }

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
    }

}

final class ConstVec3f private[math] (val x: Float, val y: Float, val z: Float)
extends AnyVec3f


final class Vec3f private[math] (var x: Float, var y: Float, var z: Float)
extends AnyVec3f
{
    override def r = x
    override def g = y
    override def b = z

    override def s = x
    override def t = y
    override def p = z

    def r_=(r: Float) { x = r }
    def g_=(g: Float) { y = g }
    def b_=(b: Float) { z = b }

    def s_=(s: Float) { x = s }
    def t_=(t: Float) { y = t }
    def p_=(p: Float) { z = p }


    def *=(s: Float) { x *= s; y *= s; z *= s }
    def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

    def +=(u: AnyVec3f) { x += u.x; y += u.y; z += u.z }
    def -=(u: AnyVec3f) { x -= u.x; y -= u.y; z -= u.z }
    def *=(u: AnyVec3f) { x *= u.x; y *= u.y; z *= u.z }
    def /=(u: AnyVec3f) { x /= u.x; y /= u.y; z /= u.z }

    def *=(m: AnyMat3f) { this := m.transposeMul(this) }

    def :=(u: AnyVec3f) { x = u.x; y = u.y; z = u.z }
    def set(x: Float, y: Float, z: Float) { this.x = x; this.y = y; this.z = z }

    def update(i: Int, s: Float) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }
}

object Vec3f {
    val Origin = const(Vec3f(0))
    val UnitX = const(Vec3f(1, 0, 0))
    val UnitY = const(Vec3f(0, 1, 0))
    val UnitZ = const(Vec3f(0, 0, 1))

    def apply(s: Float) = new Vec3f(s, s, s)
    def apply(x: Float, y: Float, z: Float) = new Vec3f(x, y, z)
    def apply(u: AnyVec3f) = new Vec3f(u.x, u.y, u.z)
    def apply(u: AnyVec4f) = new Vec3f(u.x, u.y, u.z)
    def apply(xy: AnyVec2f, z: Float) = new Vec3f(xy.x, xy.y, z)
    def apply(x: Float, yz: AnyVec2f) = new Vec3f(x, yz.x, yz.y)
    def apply(u: Read3Int) = new Vec3f(u.x, u.y, u.z)
    def apply(u: Read4Int) = new Vec3f(u.x, u.y, u.z)

    def apply(u: Read3Double) = {
        new Vec3f(float(u.x), float(u.y), float(u.z))
    }
    def apply(u: Read4Double) = {
        new Vec3f(float(u.x), float(u.y), float(u.z))
    }
    def apply(xy: Read2Double, z: Float) = {
        new Vec3f(float(xy.x), float(xy.y), z)
    }
    def apply(x: Float, yz: Read2Double) = {
        new Vec3f(x, float(yz.x), float(yz.y))
    }

    implicit def constToMutable(u: ConstVec3f) = Vec3f(u)
    implicit def vec3ToSwizzled(u: Vec3f) = new Vec3fSwizzled(u)
}

private[math] class ConstVec3fSwizzled(u: AnyVec3f)
extends ConstVec2fSwizzled(null)
with Swizzle3Read[Float, ConstVec2f, ConstVec3f, ConstVec4f]
{
    override def x = u.x
    override def y = u.y
    def z = u.z
}

private[math] class Vec3fSwizzled(u: Vec3f) extends ConstVec3fSwizzled(u)
with Swizzle3Write[Float, ConstVec2f, ConstVec3f, ConstVec4f]
{
    def x_=(x: Float) { u.x = x }
    def y_=(y: Float) { u.y = y }
    def z_=(z: Float) { u.z = z }
}
