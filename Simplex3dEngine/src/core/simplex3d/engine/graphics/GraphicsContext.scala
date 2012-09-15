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
import simplex3d.data._
import simplex3d.engine.util._
import simplex3d.engine.scene._


abstract class GraphicsContext {
  
  type Geometry <: graphics.Geometry
  type Material <: graphics.Material
  type Environment <: graphics.Environment
  
  def mkGeometry() :Geometry
  def mkMaterial(controllerContext: ControllerContext) :Material
  def mkEnvironment(controllerContext: ControllerContext) :Environment
  
  
  final def namespace: Set[String] = combinedNamespace
  private[this] final var combinedNamespace: Set[String] = null
  
  private[this] final val uniformMap = new HashMap[String, (Int, Int)]
  private[this] final val attributeMap = new HashMap[String, java.lang.Integer]

  
  protected def init() {
    val geometry = mkGeometry()
    val material = mkMaterial(null)
    val environment = mkEnvironment(null)
    
    // Initialize namespace.
    val geomNames = geometry.attributeNames
    
    val predefNames = PredefinedUniforms.BindingNames
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
    registerUniforms(UniformOrigin.Predefined, PredefinedUniforms.BindingNames)
    registerUniforms(UniformOrigin.Material, material.uniformNames)
    registerUniforms(UniformOrigin.Environment, environment.propertyNames)
    
    
    // Initialize attribute map.
    for (i <- 0 until geometry.attributeNames.length) {
      attributeMap.put(geometry.attributeNames(i), i)
    }
  }
  
  
  def resolveRootUniform(
    name: String,
    predefined: ReadPredefinedUniforms,
    material: graphics.Material,
    environment: graphics.Environment,
    programUniforms: Map[String, Property[UncheckedBinding]]
  ) :AnyRef = {
    val originAndId = uniformMap.get(name)
    
    if (originAndId == null) {
      val option = programUniforms.get(name)
      if (option.isDefined) option.get.get else null
    }
    else {
      val origin = originAndId._1
      val id = originAndId._2
      
      origin match {
        
        case UniformOrigin.Predefined =>
          predefined.bindings(id)
          
        case UniformOrigin.Material =>
          val prop = material.uniforms(id)
          if (prop.isDefined) prop.get else null
          
        case UniformOrigin.Environment =>
          val prop = environment.properties(id)
          if (prop.isDefined) prop.get.binding else null
      }
    }
  }
  
  def resolveUniformPath(
    path: String,
    predefined: ReadPredefinedUniforms,
    material: graphics.Material,
    environment: graphics.Environment,
    programUniforms: Map[String, Property[UncheckedBinding]]
  ) :AnyRef = {
    
    path match {
      case PathUtil.NameIndexRest(name, index, rest) =>
        val res = resolveRootUniform(name, predefined, material, environment, programUniforms)
        PathUtil.resolveAsList(index.toInt, rest, res)
        
      case PathUtil.NameRest(name, rest) =>
        val res = resolveRootUniform(name, predefined, material, environment, programUniforms)
        PathUtil.resolveAsValue(rest, res)
        
      case _ =>
        null
    }
  }
  
  def resolveAttributePath(
    path: String,
    geometry: graphics.Geometry
  ) :Attributes[Format with MathType, Raw] = {
    
    val id = attributeMap.get(path)
    
    if (id == null) null else {
      val binding = geometry.attributes(id)
      if (!binding.isDefined) null else binding.get
    }
  }
}


object MinimalGraphicsContext extends GraphicsContext {
  type Geometry = graphics.Geometry
  type Material = graphics.Material
  type Environment = graphics.Environment
  
  
  def mkGeometry() =
    new prototype.Geometry { init(this.getClass) }
  
  def mkMaterial(controllerContext: ControllerContext) =
    new prototype.Material(controllerContext) { init(this.getClass) }
  
  def mkEnvironment(controllerContext: ControllerContext) =
    new prototype.Environment(controllerContext) { init(this.getClass) }
  
  
  init()
}
