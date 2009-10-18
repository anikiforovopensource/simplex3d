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
class Random extends scala.util.Random {

    def nextVec2() :Vec2 = Vec2(nextFloat(), nextFloat())
    def nextVec3() :Vec3 = Vec3(nextFloat(), nextFloat(), nextFloat())
    def nextVec4() :Vec4 = {
        Vec4(nextFloat(), nextFloat(), nextFloat(), nextFloat())
    }

    def nextVec2i() :Vec2i = Vec2i(nextInt(), nextInt())
    def nextVec3i() :Vec3i = Vec3i(nextInt(), nextInt(), nextInt())
    def nextVec4i() :Vec4i = {
        Vec4i(nextInt(), nextInt(), nextInt(), nextInt())
    }

    def nextVec2i(n: Int) :Vec2i = Vec2i(nextInt(n), nextInt(n))
    def nextVec3i(n: Int) :Vec3i = Vec3i(nextInt(n), nextInt(n), nextInt(n))
    def nextVec4i(n: Int) :Vec4i = {
        Vec4i(nextInt(n), nextInt(n), nextInt(n), nextInt(n))
    }

    def nextVec2b() :Vec2b = Vec2b(nextBoolean(), nextBoolean())
    def nextVec3b() :Vec3b = Vec3b(nextBoolean(), nextBoolean(), nextBoolean())
    def nextVec4b() :Vec4b = {
        Vec4b(nextBoolean(), nextBoolean(), nextBoolean(), nextBoolean())
    }
    
}
