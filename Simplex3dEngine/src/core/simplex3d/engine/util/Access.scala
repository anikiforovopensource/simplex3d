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
package util

import simplex3d.math.types._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.engine.bounding._
import simplex3d.engine.graphics._
import simplex3d.engine.scene._


object ClearChangesAccess {
  
  implicit def listenerSubtext(listener: StructuralChangeListener)
  :StructuralChangeListener#StructuralSubtext = listener.structuralSubtext
  
  implicit def changeListenerSubtext(listener: DataChangeListener)
  :DataChangeListener#DataSubtext = listener.dataSubtext

  
  implicit def accessOptional(property: Optional[_])
  = property.asInstanceOf[AccessibleOptional[_]]
  
  implicit def accessDefined(property: Defined[_])
  = property.asInstanceOf[AccessibleDefined[_]]
  
  implicit def accessSharedRef(property: SharedRef[_])
  = property.asInstanceOf[AccessibleSharedRef]
  
  implicit def accessSharedAttributes(property: SharedAttributes[_, _])
  = property.asInstanceOf[AccessibleSharedAttributes[_, _]]
  
  
  implicit def attributeSharedSubtext(sharedState: AttributesSharedState)
  :AttributesSharedState#Subtext = sharedState.subtext
  
  implicit def textureSubtext(texture: Texture[_]) =
    texture.subtext.asInstanceOf[Texture[Accessor with AnyVec[Double]]#Subtext]
}


object SceneAccess {
  implicit def meshSubtext(mesh: AbstractMesh)
  :AbstractMesh#MeshSubtext = mesh.meshSubtext
  
  implicit def sceneSubtext(scene: Scene[_])
  :Scene[GraphicsContext]#SceneSubtext = scene.asInstanceOf[Scene[GraphicsContext]].sceneSubtext
  
  implicit def managedSceneSubtext(managedScene: ManagedScene[_]) :ManagedScene[GraphicsContext]#ManagedSceneSubtext =
    managedScene.asInstanceOf[ManagedScene[GraphicsContext]].managedSceneSubtext
}


trait EngineAccess {
  def getWorldMatrix(spatial: Spatial) :ReadMat4x3 = spatial.resolveWorldMatrix()
  def setWorldMatrixResolver(spatial: Spatial, resolver: () => ReadMat4x3) { spatial.resolveWorldMatrix = resolver }
  
  def getEngineInfo(info: EngineInfo) :Object = info.engineInfo
  def setEngineInfo(info: EngineInfo, data: Object) { info.engineInfo = data }
}
