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
private[math] trait VecFactory[P, R2, R3, R4] {
    protected def make2(x: P, y: P) :R2
    protected def make3(x: P, y: P, z: P) :R3
    protected def make4(x: P, y: P, z: P, w: P) :R4
}

private[math] class FloatVecFactory
extends VecFactory[Float, ConstVec2, ConstVec3, ConstVec4]
{
    protected def make2(x: Float, y: Float) = ConstVec2(x, y)
    protected def make3(x: Float,
                               y: Float,
                               z: Float) = ConstVec3(x, y, z)
    protected def make4(x: Float,
                               y: Float,
                               z: Float,
                               w: Float) = ConstVec4(x, y, z, w)
}

private[math] class IntVecFactory
extends VecFactory[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    protected def make2(x: Int, y: Int) = ConstVec2i(x, y)
    protected def make3(x: Int,
                               y: Int,
                               z: Int) = ConstVec3i(x, y, z)
    protected def make4(x: Int,
                               y: Int,
                               z: Int,
                               w: Int) = ConstVec4i(x, y, z, w)
}

private[math] class BooleanVecFactory
extends VecFactory[Boolean, ConstVec2b, ConstVec3b, ConstVec4b]
{
    protected def make2(x: Boolean, y: Boolean) = ConstVec2b(x, y)
    protected def make3(x: Boolean,
                               y: Boolean,
                               z: Boolean) = ConstVec3b(x, y, z)
    protected def make4(x: Boolean,
                               y: Boolean,
                               z: Boolean,
                               w: Boolean) = ConstVec4b(x, y, z, w)
}
