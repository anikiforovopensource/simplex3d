/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine

import simplex3d.math._


trait RenderContext {
  val capabilities: GraphicsCapabilities
  
  def init(attributes: Attributes[_, _])
  def release(attributes: Attributes[_, _])
  
  def init(texture: Texture[_]) :Unit
  def release(texture: Texture[_]) :Unit
  
  def clearFrameBuffer() :Unit
  def viewportDimensions() :ConstVec2i
  
  def manage() :Unit
  def cleanup() :Unit
}
