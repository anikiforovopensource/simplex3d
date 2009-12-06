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


/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] object Read {
    def read(arg: ReadAny[AnyVal], mat: Array[Float], index: Int) :Int = {
        var i = index
        arg match {
            case s: Read1Int => {
                mat(i) = s.value
                i += 1
            }
            case v2: Read2Int => {
                mat(i) = v2.x
                i += 1
                mat(i) = v2.y
                i += 1
            }
            case v3: Read3Int => {
                mat(i) = v3.x
                i += 1
                mat(i) = v3.y
                i += 1
                mat(i) = v3.z
                i += 1
            }
            case v4: Read4Int => {
                mat(i) = v4.x
                i += 1
                mat(i) = v4.y
                i += 1
                mat(i) = v4.z
                i += 1
                mat(i) = v4.w
                i += 1
            }
            case s: Read1Float => {
                mat(i) = s.value
                i += 1
            }
            case v2: AnyVec2f => {
                mat(i) = v2.x
                i += 1
                mat(i) = v2.y
                i += 1
            }
            case v3: AnyVec3f => {
                mat(i) = v3.x
                i += 1
                mat(i) = v3.y
                i += 1
                mat(i) = v3.z
                i += 1
            }
            case v4: AnyVec4f => {
                mat(i) = v4.x
                i += 1
                mat(i) = v4.y
                i += 1
                mat(i) = v4.z
                i += 1
                mat(i) = v4.w
                i += 1
            }
            case s: Read1Double => {
                mat(i) = float(s.value)
                i += 1
            }
            case v2: Read2Double => {
                mat(i) = float(v2.x)
                i += 1
                mat(i) = float(v2.y)
                i += 1
            }
            case v3: Read3Double => {
                mat(i) = float(v3.x)
                i += 1
                mat(i) = float(v3.y)
                i += 1
                mat(i) = float(v3.z)
                i += 1
            }
            case v4: Read4Double => {
                mat(i) = float(v4.x)
                i += 1
                mat(i) = float(v4.y)
                i += 1
                mat(i) = float(v4.z)
                i += 1
                mat(i) = float(v4.w)
                i += 1
            }
            case _ => throw new IllegalArgumentException(
                "Expected a scalar or a vector of type Float, " +
                "got " + arg.getClass.getName)
        }
        i
    }
}
