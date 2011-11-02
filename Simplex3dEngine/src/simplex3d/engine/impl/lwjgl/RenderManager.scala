/*
 * Simplex3dEngine - LWJGL Module
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
package impl.lwjgl

import java.util.logging._
import java.nio._
import scala.annotation._
import scala.collection.mutable.ArrayBuffer
import org.lwjgl.opengl._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.impl.gl._


private[lwjgl] object RenderManager {
  private final val logger = Logger.getLogger(classOf[RenderManager].getName)
}


private[lwjgl] final class RenderManager(val renderContext: RenderContext)
extends graphics.RenderManager with GlUnsafeAccess
{
  import GL11._; import GL12._; import GL13._; import GL14._; import GL15._;
  import GL20._; import GL21._
  import RenderManager.logger._
  import SceneAccess._; import ClearChangesAccess._
  
  
  private val elementRange = new ElementRange()
  
  def render(camera: AbstractCamera, renderArray: SortBuffer[AbstractMesh]) {
    if (renderContext.requiresReset) renderContext.resetState()
    
    // XXX these should come from the path, and get activated via context
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    
    var i = 0; while (i < renderArray.size) {
      val mesh = renderArray(i)
      render(camera, mesh)
      
      i += 1
    }
  }

  
  private def render(camera: AbstractCamera, mesh: AbstractMesh) {
    
    val program = mesh.technique.defined
    var useDefaultProgram = false
    
    if (program == null) {
      useDefaultProgram = true
      log(Level.SEVERE, "Mesh '" + mesh.name + "' has an undefined technique. Default technique will be used.")
    }
    else {
      useDefaultProgram = !renderContext.bindProgram(program)
      
      if (useDefaultProgram) log(
        Level.SEVERE, "Unable to use technique provided for mesh '" + mesh.name +
        "'. Default technique will be used."
      )
    }
    
    if (useDefaultProgram) {
      mesh.technique.defineAs(renderContext.defaultProgram)
      render(camera, mesh)
      return
    }
    
    
    val programMapping = program.mapping
    
    if (mesh.technique.hasRefChanges) {
      renderContext.rebuildMeshMapping(mesh, programMapping)
      mesh.technique.clearRefChanges()
      
      def resolveUpdatableEffects() {
        var updatables = List[AnyRef]()
        val properties = mesh.worldEnvironment.properties
        var i = 0; while (i < properties.length) { val property = properties(i)
          if (property.isDefined && property.defined.isInstanceOf[UpdatableEnvironmentalEffect[_]]) {
            updatables ::= property.defined
          }
          
          i += 1
        }
        
        mesh.updatableEffects = new ReadArray(updatables.toArray).asInstanceOf[ReadArray[UncheckedUpdatableEffect]]
      }; resolveUpdatableEffects()
    }
    
    
    val transformation = getWorldMatrix(mesh)
    val geometry = mesh.geometry
    val material = mesh.material
    
    mesh.resolveElementRange(elementRange)
    renderContext.setFaceCulling(geometry.faceCulling.defined.toConst)
    
    
    val predefinedUniforms = mesh.predefinedUniforms
    predefinedUniforms.se_modelViewMatrix := Mat4(transformation concat camera.view)
    predefinedUniforms.se_modelViewProjectionMatrix := camera.projection*predefinedUniforms.se_modelViewMatrix
    predefinedUniforms.se_normalMatrix := transpose(inverse(Mat3(predefinedUniforms.se_modelViewMatrix)))
    
    // Update bindings using predefined uniforms.
    val effects = mesh.updatableEffects
    var i = 0; while (i < effects.length) { val property = effects(i)
      property.updateBinding(predefinedUniforms)
      
      i += 1
    }
    
    programMapping.bind(mesh.mapping)
    
//    val directionalLightPos = Vec3(-0.25, 0.5, 1)
//    val ecLightDir = -normalize(camera.view.transformVector(directionalLightPos))
//    program("ecLightDir") = ecLightDir
    
    val vertexMode = geometry.mode match {
      case p: Points => glPointSize(p.size.toFloat); GL_POINTS
      case l: Lines => glLineWidth(l.width.toFloat); GL_LINES
      case Triangles => GL_TRIANGLES
      case Quads => GL_QUADS
    }
      
    
    if (geometry.indices.isDefined) {
      renderContext.init(geometry.indices.defined)
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, geometry.indices.defined.managedFields.id)
      glDrawElements(vertexMode, elementRange.count, geometry.indices.defined.src.rawType, elementRange.first.toConst)
    }
    else {
      glDrawArrays(vertexMode, elementRange.first, elementRange.count)
    }
  }
  
  // XXX must be updated with new attributes
  private val comparator = new java.util.Comparator[AbstractMesh] with EngineAccess {
    
    def compare(a: AbstractMesh, b: AbstractMesh) :Int = {
      // XXX sort by textures: if (at0id < bt0id) -1 else if (at0id > bt0id)  1 else 0
      // XXX also sort by parent's environment
      
      val ainfo = getEngineInfo(a.technique.defined).asInstanceOf[ProgramInfo]
      val apid = if (ainfo != null) ainfo.managedFields.id else 0
      
      val binfo = getEngineInfo(b.technique.defined).asInstanceOf[ProgramInfo]
      val bpid = if (binfo != null) binfo.managedFields.id else 0
      
      if (apid < bpid) -1 else if (apid > bpid)  1 else 0
    }
  }
  
  def sortRenderArray(pass: Pass, renderArray: SortBuffer[AbstractMesh]) {
    renderArray.inplaceSort(comparator)
  }
}
