/*
 * Simplex3d, FloatBuffer module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer.floatm

import scala.reflect.ClassManifest._
import simplex3d.math.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Manifest {
  final val Vec2fClassManifest = classType[Vec2f#Element](classOf[AnyVec2f])
  final val Vec3fClassManifest = classType[Vec3f#Element](classOf[AnyVec3f])
  final val Vec4fClassManifest = classType[Vec4f#Element](classOf[AnyVec4f])
}
