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

import simplex3d.math.types._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.engine.bounding._
import simplex3d.engine.graphics._
import simplex3d.engine.scene._


object SubtextAccess {
  implicit def listenerSubtext(listener: StructuralChangeListener)
  :StructuralChangeListener#StructuralSubtext = listener.structuralSubtext
  
  implicit def changeListenerSubtext(listener: DataChangeListener)
  :DataChangeListener#DataSubtext = listener.dataSubtext

  implicit def accessDefinedProperty(property: DefinedProperty[_])
  = property.asInstanceOf[AccessibleDefinedProperty]
  
  implicit def accessShaderProperty(property: ShaderProperty[_])
  = property.asInstanceOf[AccessibleShaderProperty[_]]
  
  implicit def accessProperty(property: Property[_])
  = property.asInstanceOf[AccessibleProperty]
  
  implicit def accessValueProperty(property: ValueProperty[_])
  = property.asInstanceOf[AccessibleValueProperty[_]]
  
  implicit def accessEnvironmentalProperty(property: EnvironmentalProperty[_])
  = property.asInstanceOf[AccessibleEnvironmentalProperty[_]]
  
  implicit def accessSharedProperty(property: SharedProperty[_])
  = property.asInstanceOf[AccessibleSharedProperty]
  
  implicit def accessSharedAttributes(property: SharedAttributes[_, _])
  = property.asInstanceOf[AccessibleSharedAttributes[_, _]]
  
  implicit def attributeSharedSubtext(sharedState: AttributesSharedState)
  :AttributesSharedState#Subtext = sharedState.subtext
  
  implicit def textureSubtext(texture: Texture[_]) =
    texture.subtext.asInstanceOf[Texture[Accessor with AnyVec[Double]]#Subtext]
  
  implicit def meshSubtext(mesh: AbstractMesh)
  :AbstractMesh#MeshSubtext = mesh.meshSubtext
  
  implicit def sceneSubtext(scene: Scene[_])
  :Scene[GraphicsContext]#Subtext = scene.asInstanceOf[Scene[GraphicsContext]].subtext
}


trait EngineAccess {
  def getWorldMatrix(spatial: Spatial) :ReadMat3x4 = spatial.resolveWorldMatrix()
  def setWorldMatrixResolver(spatial: Spatial, resolver: () => ReadMat3x4) { spatial.resolveWorldMatrix = resolver }
  
  def getEngineInfo(info: EngineInfo) :Object = info.engineInfo
  def setEngineInfo(info: EngineInfo, data: Object) { info.engineInfo = data }
}
