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
package graphics


abstract class GraphicsContext {
  
  type Geometry <: graphics.Geometry
  type Material <: graphics.Material
  type Environment <: graphics.Environment
  
  val mkGeometry: () => Geometry
  val mkMaterial: () => Material
  val mkEnvironment: () => Environment
  
  
  final def namespace: Set[String] = combinedNamespace
  private[this] final var combinedNamespace: Set[String] = null

  
  protected def initNamespace() {
    val geomNames = mkGeometry().attributeNames
    
    val predefNames = PredefinedUniforms.Names
    val matNames = mkMaterial().uniformNames
    val envNames = mkEnvironment().propertyNames
    
    val attributeNamespace = geomNames.toSet
    val uniformNamespace = predefNames.toSet ++ matNames ++ envNames
    combinedNamespace = attributeNamespace ++ uniformNamespace
    
    if (attributeNamespace.size < geomNames.size) {
      throw new Error(
        "Duplicate attribute names: " +
        (geomNames.filterNot(e => attributeNamespace.contains(e))).distinct.mkString(", ") +
        "."
      )
    }
    
    if (uniformNamespace.size < predefNames.size + matNames.size + envNames.size) {
      throw new Error(
        "Duplicate uniform names: " +
        (predefNames ++ matNames ++ envNames).filterNot(e => uniformNamespace.contains(e)).distinct.mkString(", ") +
        "."
      )
    }
    
    if (combinedNamespace.size < attributeNamespace.size + uniformNamespace.size) {
      throw new Error(
        "Name collision between attributes and uniforms: " +
        (attributeNamespace.toList ++ uniformNamespace).filterNot(e => combinedNamespace.contains(e)).
        distinct.mkString(", ") +
        "."
      )
    }
  }
}
