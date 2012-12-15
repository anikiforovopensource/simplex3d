/*
 * Simplex3dEngine - Renderer Module
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

package simplex3d.engine.renderer
package lighting

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine.graphics._
import simplex3d.engine._


sealed abstract class ReadLighting extends ReadUpdatableEnvironmentalEffect {
  type Read = ReadLighting
  type Mutable = Lighting
  final def readType = classOf[ReadLighting]
  
  //def directionalLights: List[ReadDirectionalLight]
  def pointLights: List[ReadPointLight]
  
  final override def equals(other: Any) :Boolean = {
    other match {
      case lighting: ReadLighting =>
        false //
      case _ => false
    }
  }
  
  final override def hashCode() = super.hashCode() //
}


final class Lighting extends ReadLighting with UpdatableEnvironmentalEffect {
  protected def mkMutable() = new Lighting
  
  //var directionalLights: List[DirectionalLight] = Nil
  var pointLights: List[PointLight] = Nil
  
  private def rebuildBinding() {
    if (unsafeBinding != null) { // This will limit binding updates to those in use.
      val bindingList = unsafeBinding.asInstanceOf[BindingList[PointLight]]
      bindingList := pointLights
    }
  }
 
  def :=(r: Read) {
    // Since we are using List instead of a BindingList, we have to signalStructuralChanges manually.
    if (pointLights.size != r.pointLights.size) signalStructuralChanges()
    pointLights = r.pointLights.asInstanceOf[List[PointLight]]
   
    rebuildBinding()
  }
 
  def propagate(parentVal: Read, result: Mutable) {
    val oldLights = result.pointLights
    result.pointLights = (parentVal.pointLights ++ pointLights).asInstanceOf[List[PointLight]]
    
    // signalStructuralChanges() will reset unsafeBinding to null and cause resolveBinding to be called.
    if (result.pointLights.size != oldLights.size) result.signalStructuralChanges()
    else result.rebuildBinding()
  }
 
  def resolveBinding = {
    val list = new BindingList[PointLight]
    list ++= pointLights
    list
  }
      
  def updateBinding(predefinedUniforms: ReadPredefinedUniforms) {
    val size = pointLights.size; var i = 0; while (i < size) {
      val tp = predefinedUniforms.se_viewMatrix.transformPoint(pointLights(i).position)
      pointLights(i).ecPosition := tp
      
      i += 1
    }
  }
}
