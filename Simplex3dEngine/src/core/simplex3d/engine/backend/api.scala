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
  implicit class PropertyContextAc(val v: PropertyContext) extends AnyVal {
    @inline def hasStructuralChanges = v.hasStructuralChanges
    @inline def signalStructuralChanges() = v.signalStructuralChanges()
    @inline def clearStructuralChanges() = v.clearStructuralChanges()
  }
  
  implicit class DataChangeListenerAc(val v: DataChangeListener) extends AnyVal {
    @inline def hasDataChanges = v.hasDataChanges
    @inline def signalDataChanges() = v.signalDataChanges()
    @inline def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class PropertyAc(val v: Property[_]) extends AnyVal {
    @inline def hasDataChanges = v.hasDataChanges
    @inline def signalDataChanges() = v.signalDataChanges()
    @inline def clearDataChanges() = v.clearDataChanges()
  }

  implicit class ValueAc(val v: Value[_]) extends AnyVal {
    @inline def hasDataChanges = v.hasDataChanges
    @inline def signalDataChanges() = v.signalDataChanges()
    @inline def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class ReassignableAc(val v: Reassignable[_]) extends AnyVal {
    @inline def hasDataChanges = v.hasDataChanges
    @inline def signalDataChanges() = v.signalDataChanges()
    @inline def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class TransformationBindingAc(val v: TransformationBinding[_]) extends AnyVal {
    @inline def register(context: ControllerContext) = v.register(context)
    @inline def hasDataChanges = v.hasDataChanges
    @inline def signalDataChanges() = v.signalDataChanges()
    @inline def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class SharedRefAc(val v: SharedRef[_]) extends AnyVal {
    @inline def hasRefChanges = v.hasRefChanges
    @inline def clearRefChanges() = v.clearRefChanges()
  }
  
  implicit class AttributeBindingAc(val v: AttributeBinding[_, _]) extends AnyVal {
    @inline def hasDataChanges = v.hasDataChanges
    @inline def signalDataChanges() = v.signalDataChanges()
    @inline def clearDataChanges() = v.clearDataChanges()
    
    @inline def hasRefChanges = v.hasRefChanges
    @inline def clearRefChanges() = v.clearRefChanges()
    
    @inline def hasChanges = (hasRefChanges || hasDataChanges)
  }
  
  implicit class AttributesSharedStateAc(val v: AttributesSharedState) extends AnyVal {
    @inline def updatedRegions = v.updatedRegions
    @inline def hasDataChanges = v.hasDataChanges
    @inline def clearDataChanges() = v.clearDataChanges()
  }
  
  implicit class TextureAc(val v: Texture[_]) extends AnyVal {
    @inline def hasDataChanges = v.hasDataChanges
    @inline def clearDataChanges() = v.clearDataChanges()
    
    @inline def hasParameterChanges = v.hasParameterChanges
    @inline def clearParameterChanges() = v.clearParameterChanges()
  }
  
  implicit class AbstractMeshAc(val v: AbstractMesh) extends AnyVal {
    @inline def worldMatrix = v.ac_worldMatrix
    @inline def debugBoundingVolume = v.ac_debugBoundingVolume
    @inline def technique = v.technique
    @inline def hasStructuralChanges = v.hasStructuralChanges
    @inline def resolveElementRange(result: ElementRange) = v.resolveElementRange(result)
  }
  
  implicit class SceneAc(val v: Scene[_]) extends AnyVal {
    @inline def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) = v.ac_preload(context, frameTimer, timeSlice)
    @inline def update(time: TimeStamp) = v.ac_update(time)
    @inline def render(renderManager: RenderManager, time: TimeStamp) = v.ac_render(renderManager, time)
    @inline def manage(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) = v.ac_manage(context, frameTimer, timeSlice)
    @inline def cleanup(context: RenderContext) = v.ac_cleanup(context)
  }
  
  implicit class ManagedSceneAc(val v: ManagedScene[_]) extends AnyVal {
    @inline def camera = v.ac_camera
    @inline def buildRenderArray(pass: Pass, time: TimeStamp, result: SortBuffer[AbstractMesh]) = v.ac_buildRenderArray(pass, time, result)
  }
  
  implicit class AppAc(val v: App) extends AnyVal {
    //XXX
  }
}
