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

import simplex3d.engine.graphics._
import simplex3d.engine.transformation._


package object default {
  implicit final def extendGeometry(g: Geometry) = g.asInstanceOf[renderer.Geometry]
  implicit final def extendMaterial(m: Material) = m.asInstanceOf[renderer.Material]
  implicit final def extendEnvironment(e: Environment) = e.asInstanceOf[renderer.Environment]
  
  implicit final val GraphicsContext = new GraphicsContext(
    () => new renderer.Geometry,
    () => new renderer.Material,
    () => new renderer.Environment
  )
  
  
  implicit final def extendReadTransformation(t: ReadTransformation[_]) = t.asInstanceOf[ReadComponentTransformation3d]
  implicit final def extendTransformation(t: Transformation[_]) = t.asInstanceOf[ComponentTransformation3d]
  
  implicit final val TransformationContext = new TransformationContext(() => new ComponentTransformation3d)
}
