/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2013, Aleksey Nikiforov
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
package backend

import simplex3d.engine.bounding._
import simplex3d.engine.graphics._
import simplex3d.engine.scene._
import simplex3d.engine.transformation._
import simplex3d.engine.util._


object api {
  implicit class PropertyContextAc(val v: PropertyContext) {
    def hasStructuralChanges = v.hasStructuralChanges
    def signalStructuralChanges() = v.signalStructuralChanges()
    def clearStructuralChanges() = v.clearStructuralChanges()
  }
  
  implicit class DataChangeListenerAc(val v: DataChangeListener) {
    def hasDataChanges = v.hasDataChanges
    def signalDataChanges() = v.signalDataChanges()
    def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class PropertyAc(val v: Property[_]) {
    def hasDataChanges = v.hasDataChanges
    def signalDataChanges() = v.signalDataChanges()
    def clearDataChanges() = v.clearDataChanges()
  }

  implicit class ValueAc(val v: Value[_]) {
    def hasDataChanges = v.hasDataChanges
    def signalDataChanges() = v.signalDataChanges()
    def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class ReassignableAc(val v: Reassignable[_]) {
    def hasDataChanges = v.hasDataChanges
    def signalDataChanges() = v.signalDataChanges()
    def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class TransformationBindingAc(val v: TransformationBinding[_]) {
    def register(context: ControllerContext) = v.register(context)
    def hasDataChanges = v.hasDataChanges
    def signalDataChanges() = v.signalDataChanges()
    def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class SharedRefAc(val v: SharedRef[_]) {
    def hasRefChanges = v.hasRefChanges
    def clearRefChanges() = v.clearRefChanges()
  }
  
  implicit class AttributeBindingAc(val v: AttributeBinding[_, _]) {
    def hasDataChanges = v.hasDataChanges
    def signalDataChanges() = v.signalDataChanges()
    def clearDataChanges() = v.clearDataChanges()
    
    def hasRefChanges = v.hasRefChanges
    def clearRefChanges() = v.clearRefChanges()
    
    def hasChanges = (hasRefChanges || hasDataChanges)
  }
  
  implicit class AttributesSharedStateAc(val v: AttributesSharedState) {
    def updatedRegions = v.updatedRegions
    def hasDataChanges = v.hasDataChanges
    def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class TextureAc(val v: Texture[_]) {
    def hasDataChanges = v.hasDataChanges
    def clearDataChanges() = v.clearDataChanges()
    
    def hasParameterChanges = v.hasParameterChanges
    def clearParameterChanges() = v.clearParameterChanges()
  }
  
  implicit class AbstractMeshAc(val v: AbstractMesh) {
    def worldMatrix = v.ac_worldMatrix
    def debugBoundingVolume = v.ac_debugBoundingVolume
    def technique = v.technique
    def hasStructuralChanges = v.hasStructuralChanges
    def resolveElementRange(result: ElementRange) = v.resolveElementRange(result)
  }
  
  implicit class SceneAc(val v: Scene[_]) {
    def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) = v.ac_preload(context, frameTimer, timeSlice)
    def update(time: TimeStamp) = v.ac_update(time)
    def render(renderManager: RenderManager, time: TimeStamp) = v.ac_render(renderManager, time)
    def manage(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) = v.ac_manage(context, frameTimer, timeSlice)
    def cleanup(context: RenderContext) = v.ac_cleanup(context)
  }
  
  implicit class ManagedSceneAc(val v: ManagedScene[_]) {
    def camera = v.ac_camera
    def buildRenderArray(pass: Pass, time: TimeStamp, result: SortBuffer[AbstractMesh]) = v.ac_buildRenderArray(pass, time, result)
  }
  
  implicit class AppAc(val v: App) {
    //XXX
  }
}
