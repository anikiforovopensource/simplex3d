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
 * Glue code to make ints interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedInt(val value: Int) extends Read1[Int] {
    def *(u: AnyVec2i) = u*value
    def *(u: AnyVec3i) = u*value
    def *(u: AnyVec4i) = u*value

    def *(u: AnyVec2) = u*value
    def *(u: AnyVec3) = u*value
    def *(u: AnyVec4) = u*value

    def *(q: AnyQuat4) = q*value

    def *(m: AnyMat2) = m*value
    def *(m: AnyMat2x3) = m*value
    def *(m: AnyMat2x4) = m*value
    def *(m: AnyMat3x2) = m*value
    def *(m: AnyMat3) = m*value
    def *(m: AnyMat3x4) = m*value
    def *(m: AnyMat4x2) = m*value
    def *(m: AnyMat4x3) = m*value
    def *(m: AnyMat4) = m*value

    def /(u: AnyVec2i) = u.divideByComponent(value)
    def /(u: AnyVec3i) = u.divideByComponent(value)
    def /(u: AnyVec4i) = u.divideByComponent(value)

    def /(u: AnyVec2) = u.divideByComponent(value)
    def /(u: AnyVec3) = u.divideByComponent(value)
    def /(u: AnyVec4) = u.divideByComponent(value)

    def /(q: AnyQuat4) = q.divideByComponent(value)

    def /(m: AnyMat2) = m.divideByComponent(value)
    def /(m: AnyMat2x3) = m.divideByComponent(value)
    def /(m: AnyMat2x4) = m.divideByComponent(value)
    def /(m: AnyMat3x2) = m.divideByComponent(value)
    def /(m: AnyMat3) = m.divideByComponent(value)
    def /(m: AnyMat3x4) = m.divideByComponent(value)
    def /(m: AnyMat4x2) = m.divideByComponent(value)
    def /(m: AnyMat4x3) = m.divideByComponent(value)
    def /(m: AnyMat4) = m.divideByComponent(value)
}
