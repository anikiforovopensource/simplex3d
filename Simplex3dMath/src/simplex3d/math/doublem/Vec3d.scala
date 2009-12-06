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
sealed abstract class AnyVec3d extends Read3Double {

    def x: Double
    def y: Double
    def z: Double

    def r = x
    def g = y
    def b = z

    def s = x
    def t = y
    def p = z

    
    def apply(i: Int) :Double = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def unary_-() = Vec3d(-x, -y, -z)
    def *(s: Double) = Vec3d(x*s, y*s, z*s)
    def /(s: Double) = { val inv = 1/s; Vec3d(x*inv, y*inv, z*inv) }
    private[math] def divideByComponent(s: Double) = Vec3d(s/x, s/y, s/z)

    def +(u: AnyVec3d) = Vec3d(x + u.x, y + u.y, z + u.z)
    def -(u: AnyVec3d) = Vec3d(x - u.x, y - u.y, z - u.z)
    def *(u: AnyVec3d) = Vec3d(x * u.x, y * u.y, z * u.z)
    def /(u: AnyVec3d) = Vec3d(x / u.x, y / u.y, z / u.z)

    def *(m: AnyMat3x2d) :Vec2d = m.transposeMul(this)
    def *(m: AnyMat3d) :Vec3d = m.transposeMul(this)
    def *(m: AnyMat3x4d) :Vec4d = m.transposeMul(this)

    def ==(u: AnyVec3d) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3d) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._
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

final class ConstVec3d private[math] (
    val x: Double, val y: Double, val z: Double
) extends AnyVec3d


final class Vec3d private[math] (
    var x: Double, var y: Double, var z: Double
) extends AnyVec3d
{
    override def r = x
    override def g = y
    override def b = z

    override def s = x
    override def t = y
    override def p = z

    def r_=(r: Double) { x = r }
    def g_=(g: Double) { y = g }
    def b_=(b: Double) { z = b }

    def s_=(s: Double) { x = s }
    def t_=(t: Double) { y = t }
    def p_=(p: Double) { z = p }


    def *=(s: Double) { x *= s; y *= s; z *= s }
    def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

    def +=(u: AnyVec3d) { x += u.x; y += u.y; z += u.z }
    def -=(u: AnyVec3d) { x -= u.x; y -= u.y; z -= u.z }
    def *=(u: AnyVec3d) { x *= u.x; y *= u.y; z *= u.z }
    def /=(u: AnyVec3d) { x /= u.x; y /= u.y; z /= u.z }

    def *=(m: AnyMat3d) { this := m.transposeMul(this) }

    def :=(u: AnyVec3d) { x = u.x; y = u.y; z = u.z }
    def set(x: Double, y: Double, z: Double) { this.x = x; this.y = y; this.z = z }

    def update(i: Int, s: Double) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }
}

object Vec3d {
    val Origin = const(Vec3d(0))
    val UnitX = const(Vec3d(1, 0, 0))
    val UnitY = const(Vec3d(0, 1, 0))
    val UnitZ = const(Vec3d(0, 0, 1))

    def apply(s: Double) = new Vec3d(s, s, s)
    def apply(x: Double, y: Double, z: Double) = new Vec3d(x, y, z)
    def apply(u: AnyVec3d) = new Vec3d(u.x, u.y, u.z)
    def apply(u: AnyVec4d) = new Vec3d(u.x, u.y, u.z)
    def apply(xy: AnyVec2d, z: Double) = new Vec3d(xy.x, xy.y, z)
    def apply(x: Double, yz: AnyVec2d) = new Vec3d(x, yz.x, yz.y)
    def apply(u: Read3Int) = new Vec3d(u.x, u.y, u.z)
    def apply(u: Read4Int) = new Vec3d(u.x, u.y, u.z)
    def apply(u: Read3Float) = new Vec3d(u.x, u.y, u.z)
    def apply(u: Read4Float) = new Vec3d(u.x, u.y, u.z)

    implicit def constToMutable(u: ConstVec3d) = Vec3d(u)
    implicit def vec3ToSwizzled(u: Vec3d) = new Vec3dSwizzled(u)
}

private[math] class ConstVec3dSwizzled(u: AnyVec3d)
extends ConstVec2dSwizzled(null)
with Swizzle3Read[Double, ConstVec2d, ConstVec3d, ConstVec4d]
{
    override def x = u.x
    override def y = u.y
    def z = u.z
}

private[math] class Vec3dSwizzled(u: Vec3d) extends ConstVec3dSwizzled(u)
with Swizzle3Write[Double, ConstVec2d, ConstVec3d, ConstVec4d]
{
    def x_=(x: Double) { u.x = x }
    def y_=(y: Double) { u.y = y }
    def z_=(z: Double) { u.z = z }
}
