/*
 * Simplex3D, Math module
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


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3 extends Read3[Float] {

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

    def unary_-() = Vec3(-x, -y, -z)
    def *(s: Float) = Vec3(x*s, y*s, z*s)
    def /(s: Float) = { val inv = 1/s; Vec3(x*inv, y*inv, z*inv) }
    private[math] def divideByComponent(s: Float) = Vec3(s/x, s/y, s/z)

    def +(u: AnyVec3) = Vec3(x + u.x, y + u.y, z + u.z)
    def -(u: AnyVec3) = Vec3(x - u.x, y - u.y, z - u.z)
    def *(u: AnyVec3) = Vec3(x * u.x, y * u.y, z * u.z)
    def /(u: AnyVec3) = Vec3(x / u.x, y / u.y, z / u.z)

    def *(m: AnyMat3x2) :Vec2 = m.transposeMul(this)
    def *(m: AnyMat3) :Vec3 = m.transposeMul(this)
    def *(m: AnyMat3x4) :Vec4 = m.transposeMul(this)

    def ==(u: AnyVec3) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3) :Boolean = !(this == u)

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

final class ConstVec3 private (val x: Float, val y: Float, val z: Float)
extends AnyVec3

object ConstVec3 {
    def apply(s: Float) = new ConstVec3(s, s, s)
    def apply(x: Float, y: Float, z: Float) = new ConstVec3(x, y, z)
    def apply(u: AnyVec3) = new ConstVec3(u.x, u.y, u.z)
    def apply(u: AnyVec4) = new ConstVec3(u.x, u.y, u.z)
    def apply(xy: AnyVec2, z: Float) = new ConstVec3(xy.x, xy.y, z)
    def apply(x: Float, yz: AnyVec2) = new ConstVec3(x, yz.x, yz.y)
    def apply(u: Read3[Int]) = new ConstVec3(u.x, u.y, u.z)
    def apply(u: Read4[Int]) = new ConstVec3(u.x, u.y, u.z)

    implicit def mutableToConst(u: Vec3) = ConstVec3(u)
    implicit def constVec3ToSwizzled(u: ConstVec3) = new ConstVec3Swizzled(u)
}

final class Vec3 private (var x: Float, var y: Float, var z: Float)
extends AnyVec3
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

    def +=(u: AnyVec3) { x += u.x; y += u.y; z += u.z }
    def -=(u: AnyVec3) { x -= u.x; y -= u.y; z -= u.z }
    def *=(u: AnyVec3) { x *= u.x; y *= u.y; z *= u.z }
    def /=(u: AnyVec3) { x /= u.x; y /= u.y; z /= u.z }

    def *=(m: AnyMat3) { this := m.transposeMul(this) }

    def :=(u: AnyVec3) { x = u.x; y = u.y; z = u.z }
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

object Vec3 {
    val Origin = ConstVec3(0)
    val UnitX = ConstVec3(1, 0, 0)
    val UnitY = ConstVec3(0, 1, 0)
    val UnitZ = ConstVec3(0, 0, 1)

    def apply(s: Float) = new Vec3(s, s, s)
    def apply(x: Float, y: Float, z: Float) = new Vec3(x, y, z)
    def apply(u: AnyVec3) = new Vec3(u.x, u.y, u.z)
    def apply(u: AnyVec4) = new Vec3(u.x, u.y, u.z)
    def apply(xy: AnyVec2, z: Float) = new Vec3(xy.x, xy.y, z)
    def apply(x: Float, yz: AnyVec2) = new Vec3(x, yz.x, yz.y)
    def apply(u: Read3[Int]) = new Vec3(u.x, u.y, u.z)
    def apply(u: Read4[Int]) = new Vec3(u.x, u.y, u.z)

    implicit def vec3ToSwizzled(u: Vec3) = new Vec3Swizzled(u)
}

private[math] class ConstVec3Swizzled(u: AnyVec3)
extends ConstVec2Swizzled(null)
with Swizzle3Read[Float, ConstVec2, ConstVec3, ConstVec4]
{
    override def x = u.x
    override def y = u.y
    def z = u.z
}

private[math] class Vec3Swizzled(u: Vec3) extends ConstVec3Swizzled(u)
with Swizzle3Write[Float, ConstVec2, ConstVec3, ConstVec4]
{
    def x_=(x: Float) { u.x = x }
    def y_=(y: Float) { u.y = y }
    def z_=(z: Float) { u.z = z }
}
