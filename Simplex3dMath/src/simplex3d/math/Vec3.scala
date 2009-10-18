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

import simplex3d.math.VecMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3 extends Read3[Float] {

    def x: Float
    def y: Float
    def z: Float

    
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

    /**
     * Approximate comparision.
     * Read: "this isApproximately u".
     */
    def ~=(u: AnyVec3) :Boolean = {
        if (u == null)
            false
        else
            abs(x - u.x) < ApproximationDelta &&
            abs(y - u.y) < ApproximationDelta &&
            abs(z - u.z) < ApproximationDelta
    }

    /**
     * Inverse of approximate comparision.
     * Read: "this isNotApproximately u" or "this isDistinctFrom u".
     */
    def !~(u: AnyVec3) :Boolean = !(this ~= u)

    def isValid: Boolean = {
        import java.lang.Float._
        !(
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
    def apply(u: AnyVec3i) = new ConstVec3(u.x, u.y, u.z)
    def apply(u: AnyVec4i) = new ConstVec3(u.x, u.y, u.z)
    def apply(xy: AnyVec2i, z: Float) = new ConstVec3(xy.x, xy.y, z)
    def apply(x: Float, yz: AnyVec2i) = new ConstVec3(x, yz.x, yz.y)

    implicit def mutableToConst(u: Vec3) = ConstVec3(u)
    implicit def constVec3ToSwizzled(u: ConstVec3) = new ConstVec3Swizzled(u)
}

final class Vec3 private (var x: Float, var y: Float, var z: Float)
extends AnyVec3
{

    def *=(s: Float) { x *= s; y *= s; z *= s }
    def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

    def +=(u: AnyVec3) { x += u.x; y += u.y; z += u.z }
    def -=(u: AnyVec3) { x -= u.x; y -= u.y; z -= u.z }
    def *=(u: AnyVec3) { x *= u.x; y *= u.y; z *= u.z }
    def /=(u: AnyVec3) { x /= u.x; y /= u.y; z /= u.z }

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
    val Zero = ConstVec3(0)
    val One = ConstVec3(1)
    val UnitX = ConstVec3(1, 0, 0)
    val UnitY = ConstVec3(0, 1, 0)
    val UnitZ = ConstVec3(0, 0, 1)

    def apply(s: Float) = new Vec3(s, s, s)
    def apply(x: Float, y: Float, z: Float) = new Vec3(x, y, z)
    def apply(u: AnyVec3) = new Vec3(u.x, u.y, u.z)
    def apply(u: AnyVec4) = new Vec3(u.x, u.y, u.z)
    def apply(xy: AnyVec2, z: Float) = new Vec3(xy.x, xy.y, z)
    def apply(x: Float, yz: AnyVec2) = new Vec3(x, yz.x, yz.y)
    def apply(u: AnyVec3i) = new Vec3(u.x, u.y, u.z)
    def apply(u: AnyVec4i) = new Vec3(u.x, u.y, u.z)
    def apply(xy: AnyVec2i, z: Float) = new Vec3(xy.x, xy.y, z)
    def apply(x: Float, yz: AnyVec2i) = new Vec3(x, yz.x, yz.y)

    implicit def vec3ToSwizzled(u: Vec3) = new Vec3Swizzled(u)
}

class ConstVec3Swizzled(u: AnyVec3) extends VecSwizzled
with Swizzle3Read[Float, ConstVec2, ConstVec3, ConstVec4]
{
    def x = u.x
    def y = u.y
    def z = u.z
}

class Vec3Swizzled(u: Vec3) extends ConstVec3Swizzled(u)
with Swizzle3Write[Float, ConstVec2, ConstVec3, ConstVec4]
{
    def x_=(x: Float) { u.x = x }
    def y_=(y: Float) { u.y = y }
    def z_=(z: Float) { u.z = z }
}
