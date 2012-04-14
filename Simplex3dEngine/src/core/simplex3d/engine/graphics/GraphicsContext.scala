/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011-2012, Aleksey Nikiforov
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

import java.util.HashMap
import simplex3d.math.types._
import simplex3d.engine.util._


abstract class GraphicsContext {
  
  type Geometry <: graphics.Geometry
  type Material <: graphics.Material
  type Environment <: graphics.Environment
  
  def mkGeometry() :Geometry
  def mkMaterial() :Material
  def mkEnvironment() :Environment
  
  
  final def namespace: Set[String] = combinedNamespace
  private[this] final var combinedNamespace: Set[String] = null
  
  private[this] final val uniformMap: HashMap[String, (Int, Int)] = new HashMap[String, (Int, Int)]

  
  protected def init() {
    val material = mkMaterial()
    val environment = mkEnvironment()
    
    // Initialize namespace.
    val geomNames = mkGeometry().attributeNames
    
    val predefNames = PredefinedUniforms.Names
    val matNames = material.uniformNames
    val envNames = environment.propertyNames
    
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
    

    // Initialize uniform map.
    def registerUniforms(origin: Int, names: ReadArray[String]) {
      for (i <- 0 until names.size) {
        uniformMap.put(names(i), (origin, i))
      }
    }
    registerUniforms(UniformOrigin.Predefined, PredefinedUniforms.Names)
    registerUniforms(UniformOrigin.Material, material.uniformNames)
    registerUniforms(UniformOrigin.Environment, environment.propertyNames)
  }
  
  def resolveUniform(
    name: String,
    predefind: ReadPredefinedUniforms,
    material: Material,
    environment: Environment,
    technique: TechniqueUniforms
  ) :Binding = {
    val res = uniformMap.get(name)
    
    if (res != null) {
      val origin = res._1
      val id = res._2
      
//      val binding =
//      origin match {
//        case UniformOrigin.Predefined => def resolvePredefined() :AnyRef = {
//          if (isIndexValid(index, "", name)) {
//            val prop = material.uniforms(index)
//            if (prop.isDefined) prop.get else null
//          }
//          else null
//        }; resolvePredefined()
//      }
    }
    
    null
  }
}


object MinimalGraphicsContext extends GraphicsContext {
  type Geometry = graphics.Geometry
  type Material = graphics.Material
  type Environment = graphics.Environment
  
  def mkGeometry() = new Geometry with prototype.Geometry { init(this.getClass) }
  def mkMaterial() = new Material with prototype.Material { init(this.getClass) }
  def mkEnvironment() = new Environment with prototype.Environment { init(this.getClass) }
  
  init()
}
