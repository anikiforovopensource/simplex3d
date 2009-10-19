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
object ExtendedMath {

    private val ColorToFloat = 1/255f;

    // Color conversions
    def rgb(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        (u.r << 16) | (u.g << 8) | u.b
    }
    def rgb(c: AnyVec3) :Int = {
        rgb(Vec3i(c*255))
    }

    def argb(c: AnyVec4i) :Int = {
        val u = clamp(c, 0, 255)
        (u.a << 24) | (u.r << 16) | (u.g << 8) | u.b
    }
    def argb(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        0xFF000000 | (u.r << 16) | (u.g << 8) | u.b
    }
    def argb(c: AnyVec4) :Int = {
        argb(Vec4i(c*255))
    }
    def argb(c: AnyVec3) :Int = {
        argb(Vec3i(c*255))
    }
    
    def rgba(c: AnyVec4i) :Int = {
        val u = clamp(c, 0, 255)
        (u.r << 24) | (u.g << 16) | (u.b << 8) | u.a
    }
    def rgba(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        (u.r << 24) | (u.g << 16) | (u.b << 8) | 0x000000FF
    }
    def rgba(c: AnyVec4) :Int = {
        rgba(Vec4i(c*255))
    }
    def rgba(c: AnyVec3) :Int = {
        rgba(Vec3i(c*255))
    }

    def bgr(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        (u.b << 16) | (u.g << 8) | u.r
    }
    def bgr(c: AnyVec3) :Int = {
        bgr(Vec3i(c*255))
    }

    def abgr(c: AnyVec4i) :Int = {
        val u = clamp(c, 0, 255)
        (u.a << 24) | (u.b << 16) | (u.g << 8) | u.r
    }
    def abgr(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        0xFF000000 | (u.b << 16) | (u.g << 8) | u.r
    }
    def abgr(c: AnyVec4) :Int = {
        abgr(Vec4i(c*255))
    }
    def abgr(c: AnyVec3) :Int = {
        abgr(Vec3i(c*255))
    }

    def bgra(c: AnyVec4i) :Int = {
        val u = clamp(c, 0, 255)
        (u.b << 24) | (u.g << 16) | (u.r << 8) | u.a
    }
    def bgra(c: AnyVec3i) :Int = {
        val u = clamp(c, 0, 255)
        (u.b << 24) | (u.g << 16) | (u.r << 8) | 0x000000FF
    }
    def bgra(c: AnyVec4) :Int = {
        bgra(Vec4i(c*255))
    }
    def bgra(c: AnyVec3) :Int = {
        bgra(Vec3i(c*255))
    }

    def rgb(c: Int) :Vec3 = {
        Vec3(
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            (c & 0x000000FF)*ColorToFloat
        )
    }

    def argb(c: Int) :Vec4 = {
        Vec4(
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            (c & 0x000000FF)*ColorToFloat,
            ((c & 0xFF000000) >> 24)*ColorToFloat
        )
    }

    def rgba(c: Int) :Vec4 = {
        Vec4(
            ((c & 0xFF000000) >> 24)*ColorToFloat,
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            (c & 0x000000FF)*ColorToFloat
        )
    }

    def bgr(c: Int) :Vec3 = {
        Vec3(
            (c & 0x000000FF)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            ((c & 0x00FF0000) >> 16)*ColorToFloat
        )
    }

    def abgr(c: Int) :Vec4 = {
        Vec4(
            (c & 0x000000FF)*ColorToFloat,
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0xFF000000) >> 24)*ColorToFloat
        )
    }

    def bgra(c: Int) :Vec4 = {
        Vec4(
            ((c & 0x0000FF00) >> 8)*ColorToFloat,
            ((c & 0x00FF0000) >> 16)*ColorToFloat,
            ((c & 0xFF000000) >> 24)*ColorToFloat,
            (c & 0x000000FF)*ColorToFloat,
        )
    }

    // Rotation
    def invert(m: Mat2) :Mat2 = {
        val detInv = 1/(m.m00*m.m11 - m.m01*m.m10)
        Mat2(
            m.m11*detInv, -m.m10*detInv,
            -m.m01*detInv, m.m00*detInv
          )
    }
}
