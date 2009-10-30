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
sealed abstract class AnyVec4 extends Read4[Float] {

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

    def unary_-() = Vec4(-x, -y, -z, -w)
    def *(s: Float) = Vec4(x*s, y*s, z*s, w*s)
    def /(s: Float) = { val inv = 1/s; Vec4(x*inv, y*inv, z*inv, w*inv) }
    private[math] def divideByComponent(s: Float) = Vec4(s/x, s/y, s/z, s/w)

    def +(u: AnyVec4) = Vec4(x + u.x, y + u.y, z + u.z, w + u.w)
    def -(u: AnyVec4) = Vec4(x - u.x, y - u.y, z - u.z, w - u.w)
    def *(u: AnyVec4) = Vec4(x * u.x, y * u.y, z * u.z, w * u.w)
    def /(u: AnyVec4) = Vec4(x / u.x, y / u.y, z / u.z, w / u.w)

    def *(m: AnyMat4x2) :Vec2 = m.transposeMul(this)
    def *(m: AnyMat4x3) :Vec3 = m.transposeMul(this)
    def *(m: AnyMat4) :Vec4 = m.transposeMul(this)

    def ==(u: AnyVec4) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4) :Boolean = !(this == u)

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

final class ConstVec4 private (val x: Float, val y: Float,
                               val z: Float, val w: Float)
extends AnyVec4

object ConstVec4 {
    def apply(s: Float) =
        new ConstVec4(s, s, s, s)

    def apply(x: Float, y: Float, z: Float, w: Float) =
        new ConstVec4(x, y, z, w)

    def apply(u: AnyVec4) =
        new ConstVec4(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2, z: Float, w: Float) =
        new ConstVec4(xy.x, xy.y, z, w)

    def apply(x: Float, yz: AnyVec2, w: Float) =
        new ConstVec4(x, yz.x, yz.y, w)

    def apply(x: Float, y: Float, zw: AnyVec2) =
        new ConstVec4(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2, zw: AnyVec2) =
        new ConstVec4(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3, w: Float) =
        new ConstVec4(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Float, yzw: AnyVec3) =
        new ConstVec4(x, yzw.x, yzw.y, yzw.z)
        
    def apply(m: AnyMat2) =
        new ConstVec4(m.m00, m.m10, m.m01, m.m11)

    def apply(u: AnyVec4i) =
        new ConstVec4(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2i, z: Float, w: Float) =
        new ConstVec4(xy.x, xy.y, z, w)

    def apply(x: Float, yz: AnyVec2i, w: Float) =
        new ConstVec4(x, yz.x, yz.y, w)

    def apply(x: Float, y: Float, zw: AnyVec2i) =
        new ConstVec4(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2i, zw: AnyVec2i) =
        new ConstVec4(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3i, w: Float) =
        new ConstVec4(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Float, yzw: AnyVec3i) =
        new ConstVec4(x, yzw.x, yzw.y, yzw.z)

    implicit def mutableToConst(u: Vec4) = ConstVec4(u)
    implicit def constVec4ToSwizzled(u: ConstVec4) = new ConstVec4Swizzled(u)
}

final class Vec4 private (var x: Float, var y: Float,
                          var z: Float, var w: Float)
extends AnyVec4
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

    def +=(u: AnyVec4) { x += u.x; y += u.y; z += u.z; w += u.w }
    def -=(u: AnyVec4) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
    def *=(u: AnyVec4) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
    def /=(u: AnyVec4) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }

    def :=(u: AnyVec4) { x = u.x; y = u.y; z = u.z; w = u.w }
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

object Vec4 {
    val Zero = ConstVec4(0)
    val One = ConstVec4(1)
    val UnitX = ConstVec4(1, 0, 0, 0)
    val UnitY = ConstVec4(0, 1, 0, 0)
    val UnitZ = ConstVec4(0, 0, 1, 0)
    val UnitW = ConstVec4(0, 0, 0, 1)

    def apply(s: Float) =
        new Vec4(s, s, s, s)

    def apply(x: Float, y: Float, z: Float, w: Float) =
        new Vec4(x, y, z, w)

    def apply(u: AnyVec4) =
        new Vec4(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2, z: Float, w: Float) =
        new Vec4(xy.x, xy.y, z, w)

    def apply(x: Float, yz: AnyVec2, w: Float) =
        new Vec4(x, yz.x, yz.y, w)

    def apply(x: Float, y: Float, zw: AnyVec2) =
        new Vec4(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2, zw: AnyVec2) =
        new Vec4(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3, w: Float) =
        new Vec4(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Float, yzw: AnyVec3) =
        new Vec4(x, yzw.x, yzw.y, yzw.z)

    def apply(m: AnyMat2) =
        new Vec4(m.m00, m.m10, m.m01, m.m11)

    def apply(u: AnyVec4i) =
        new Vec4(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2i, z: Float, w: Float) =
        new Vec4(xy.x, xy.y, z, w)

    def apply(x: Float, yz: AnyVec2i, w: Float) =
        new Vec4(x, yz.x, yz.y, w)

    def apply(x: Float, y: Float, zw: AnyVec2i) =
        new Vec4(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2i, zw: AnyVec2i) =
        new Vec4(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3i, w: Float) =
        new Vec4(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Float, yzw: AnyVec3i) =
        new Vec4(x, yzw.x, yzw.y, yzw.z)

    implicit def vec4ToSwizzled(u: Vec4) = new Vec4Swizzled(u)
}

private[math] class ConstVec4Swizzled(u: AnyVec4) extends FloatVecFactory
with Swizzle4Read[Float, ConstVec2, ConstVec3, ConstVec4]
{
    def x = u.x
    def y = u.y
    def z = u.z
    def w = u.w
}

private[math] class Vec4Swizzled(u: Vec4) extends ConstVec4Swizzled(u)
with Swizzle4Write[Float, ConstVec2, ConstVec3, ConstVec4]
{
    def x_=(x: Float) { u.x = x }
    def y_=(y: Float) { u.y = y }
    def z_=(z: Float) { u.z = z }
    def w_=(w: Float) { u.w = w }
}
