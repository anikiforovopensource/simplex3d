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
  
  private[this] var initialized = false
  protected def init() {
    if (initialized) throw new IllegalStateException("Already initialized.")
    initialized = true
    
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
  
  def getKeys(geometry: graphics.Geometry, material: graphics.Material, worldEnvironment: graphics.Environment)
  :(HashMap[ListNameKey, Integer], HashMap[String, Object]) =
  {
    
    val lists = new HashMap[ListNameKey, Integer]
    val enums = new HashMap[String, Object]
    
    var i = 0; while (i < material.uniforms.size) {
      val prop = material.uniforms(i)
      val name = material.uniformNames(i)
      
      if (prop.isDefined) prop.get match {
        case list: BindingList[_] => list.collectKeys(name, new ListNameKey("", name), lists, enums)
        case enum: EnumRef[_] => enum.collectKeys(name, enums) 
        case s: Struct => s.collectKeys(name, lists, enums)
        case _ => // do nothing
      }
      
      i += 1
    }
    
    i = 0; while (i < worldEnvironment.properties.size) {
      val prop = worldEnvironment.properties(i)
      val name = worldEnvironment.propertyNames(i)
      
      if (prop.isDefined) prop.get.binding match {
        case list: BindingList[_] => list.collectKeys(name, new ListNameKey("", name), lists, enums)
        case s: Struct => s.collectKeys(name, lists, enums)
        case _ => // do nothing
      }
      
      i += 1
    }
    
    geometry.primitive.get.mode.collectKeys("primitive.mode", enums)
    
    (lists, enums)
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
  
  
  override def resolveRootUniform(
    name: String,
    predefined: ReadPredefinedUniforms,
    material: graphics.Material,
    environment: graphics.Environment,
    programUniforms: Map[String, Property[UncheckedBinding]]
  ) :AnyRef = {
    
    var id = PathUtil.find(predefined.bindingNames, name)
    if (id >= 0) return predefined.bindings(id)
    
    id = PathUtil.find(material.uniformNames, name)
    if (id >= 0) {
      val prop = material.uniforms(id)
      return (if (prop.isDefined) prop.get else null)
    }
    
    id = PathUtil.find(environment.propertyNames, name)
    if (id >= 0) {
      val prop = environment.properties(id)
      return (if (prop.isDefined) prop.get.binding else null)
    }
    
    val option = programUniforms.get(name)
    return (if (option.isDefined) option.get.get else null)
  }
  
  override def resolveAttributePath(
    path: String,
    geometry: graphics.Geometry
  ) :Attributes[Format with MathType, Raw] = {
    
    val id = PathUtil.find(geometry.attributeNames, path)
    
    if (id < 0) null
    else {
      val binding = geometry.attributes(id)
      if (!binding.isDefined) null else binding.get
    }
  }
}
