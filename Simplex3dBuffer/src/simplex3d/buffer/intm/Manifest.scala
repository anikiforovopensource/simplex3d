/*
 * Simplex3d, IntBuffer module
 * Copyright (C) 2010 Simplex3d Team
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

package simplex3d.buffer.intm

import scala.reflect.ClassManifest._
import simplex3d.math.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Manifest {
  final val Vec2iClassManifest = classType[Vec2i#Element](classOf[AnyVec2i])
  final val Vec3iClassManifest = classType[Vec3i#Element](classOf[AnyVec3i])
  final val Vec4iClassManifest = classType[Vec4i#Element](classOf[AnyVec4i])
}
