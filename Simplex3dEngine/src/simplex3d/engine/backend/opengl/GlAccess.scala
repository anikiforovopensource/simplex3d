/*
 * Simplex3dEngine - GL Module
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
package backend.opengl

import simplex3d.engine.common._
import simplex3d.engine.graphics._
import simplex3d.engine.scene._


private[backend] trait GlAccess extends EngineAccess {
  
  implicit final def engineInfo(attributes: Attributes[_, _]) :ObjectInfo = {
    var data = getEngineInfo(attributes.sharedState).asInstanceOf[ObjectInfo]
    if (data == null) {
      data = new ObjectInfo(ManagedObjects.Attributes)
      setEngineInfo(attributes.sharedState, data)
    }
    data
  }
  
  implicit final def engineInfo(texture: Texture[_]) :TextureInfo = {
    var data = getEngineInfo(texture).asInstanceOf[TextureInfo]
    if (data == null) {
      data = new TextureInfo
      setEngineInfo(texture, data)
    }
    data
  }
  
  implicit final def engineInfo(shader: Shader) :CompiledInfo = {
    var data = getEngineInfo(shader).asInstanceOf[CompiledInfo]
    if (data == null) {
      data = new CompiledInfo(ManagedObjects.Shader)
      setEngineInfo(shader, data)
    }
    data
  }
  
  implicit final def engineInfo(program: Technique) :ProgramInfo = {
    var data = getEngineInfo(program).asInstanceOf[ProgramInfo]
    if (data == null) {
      data = new ProgramInfo
      setEngineInfo(program, data)
    }
    data
  }
  
  implicit final def engineInfo(mesh: AbstractMesh) :MeshInfo = {
    var data = getEngineInfo(mesh).asInstanceOf[MeshInfo]
    if (data == null) {
      data = new MeshInfo
      setEngineInfo(mesh, data)
    }
    data
  }
}

private[backend] trait GlUnsafeAccess extends EngineAccess {
  
  implicit final def engineInfo(attributes: Attributes[_, _]) :ObjectInfo = {
    getEngineInfo(attributes.sharedState).asInstanceOf[ObjectInfo]
  }
  
  implicit final def engineInfo(texture: Texture[_]) :TextureInfo = {
    getEngineInfo(texture).asInstanceOf[TextureInfo]
  }
  
  implicit final def engineInfo(shader: Shader) :ObjectInfo = {
    getEngineInfo(shader).asInstanceOf[ObjectInfo]
  }
  
  implicit final def engineInfo(program: Technique) :ProgramInfo = {
    getEngineInfo(program).asInstanceOf[ProgramInfo]
  }
  
  implicit final def engineInfo(mesh: AbstractMesh) :MeshInfo = {
    getEngineInfo(mesh).asInstanceOf[MeshInfo]
  }
}
