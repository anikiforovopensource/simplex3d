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

package simplex3d.backend.opengl

import scala.collection.mutable.HashSet
import scala.annotation._
import scala.ref._
import simplex3d.engine.graphics._


private[backend] final class GlResourceManager(
  val attributeManager: IdManager,
  val textureManager: IdManager,
  val shaderDeallocator: Int => Unit,
  val programDeallocator: Int => Unit
) {
  import AccessGl._
  
  private val managed = new HashSet[ManagedRef]
  private val deallocationQueue = new ReferenceQueue[ObjectInfo]
  
  
  def allocate(attributes: Attributes[_, _]) { allocate(attributeManager, engineInfo(attributes)) }
  def allocate(texture: Texture[_]) { allocate(textureManager, engineInfo(texture)) }
  
  private def allocate(idManager: IdManager, info: ObjectInfo) {
    val fields = info.managedFields
    
    if (fields.id == 0) {
      fields.id = idManager.nextId()
      info.managedRef = new ManagedRef(info, deallocationQueue, fields)
      managed.add(info.managedRef)
    }
  }
  
  def register(shader: Shader) { register(engineInfo(shader)) }
  def register(program: Technique) { register(engineInfo(program)) }
  
  private def register(info: ObjectInfo) {
    val fields = info.managedFields
    
    if (fields.id != 0) {
      info.managedRef = new ManagedRef(info, deallocationQueue, fields)
      managed.add(info.managedRef)
    }
  }
  
  def delete(info: ObjectInfo) {
    val fields = info.managedFields
    
    if (fields.id != 0) {
      deleteId(fields)

      managed.remove(info.managedRef)
      info.managedRef.clear()
      info.managedRef = null
    }
  }
  
  private[this] def deleteId(fields: ManagedFields) {
    (fields.objectType: @switch) match {
      case ManagedObjects.Attributes => attributeManager.release(fields.id)
      case ManagedObjects.Texture => textureManager.release(fields.id)
      case ManagedObjects.Shader => shaderDeallocator.apply(fields.id)
      case ManagedObjects.Program => programDeallocator.apply(fields.id)
    }

    fields.id = 0
  }
  
  def manage() {
    var last = deallocationQueue.poll; while (last.isDefined) {
      val ref = last.get.asInstanceOf[ManagedRef]
      
      if (ref.fields.id != 0) deleteId(ref.fields)
      managed.remove(ref)
      ref.clear()
      
      last = deallocationQueue.poll
    }
    
    attributeManager.releasePending()
    textureManager.releasePending()
  }
  
  def cleanup() {
    for (ref <- managed) {
      if (ref.fields.id != 0) deleteId(ref.fields)
      ref.clear()
    }
    managed.clear()
  }
}
