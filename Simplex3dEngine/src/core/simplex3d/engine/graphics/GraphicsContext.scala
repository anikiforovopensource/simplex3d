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
  }
  
  
  def resolveRootUniform(
    name: String,
    predefined: ReadPredefinedUniforms,
    material: graphics.Material,
    environment: graphics.Environment,
    programUniforms: Map[String, Defined[UncheckedBinding]]
  ) :Binding = {
    val originAndId = uniformMap.get(name)
    
    if (originAndId == null) {
      val option = programUniforms.get(name)
      if (option.isDefined) option.get.get else null
    }
    else {
      val origin = originAndId._1
      val id = originAndId._2
      
      val binding: Binding = origin match {
        
        case UniformOrigin.Predefined =>
          predefined.bindings(id)
          
        case UniformOrigin.Material =>
          val prop = material.uniforms(id)
          if (prop.isDefined) prop.get else null
          
        case UniformOrigin.Environment =>
          val prop = environment.properties(id)
          if (prop.isDefined) prop.get.binding else null
      }
      
      binding
    }
  }
  
  def resolveUniformPath(
    path: String,
    predefined: ReadPredefinedUniforms,
    material: graphics.Material,
    environment: graphics.Environment,
    programUniforms: Map[String, Defined[UncheckedBinding]]
  ) :Object = {
    
    val bindingFromName = (name: String) => resolveRootUniform(name, predefined, material, environment, programUniforms)
    PathUtil.resolve(path, bindingFromName)
  }
  
  /* XXX redo comments for loader remapping.
   * Uniform values can be mapped to an object path on material or environment. This is useful when
   * a vertex shader consumes a part of a struct and a fragment consumes the rest.
   * Example: declare[Vec2]("offset").remap("textureUnit.offset")
   * 
   * Arrays can be remapped using a wildcard array declaration [*]. Only one wildcard declaration
   * is allowed per path, other array instances must specify the exact index.
   * Example: declare[BindingList[Vec3]]("colors").remap("colorMaps[0].color[*]")
   * 
   * Value path remapping can only be used on subclasses of Struct and can only navigate the fields
   * declared in Struct.fields. As a special case, texture dimensions can be accessed this way as well.
   * Example: declare[Vec2i]("dimensions").remap("textureUnit.texture.dimensions")
   */
  def resolveRemappedUniformPath(//XXX rework this for model loader remapping.
    path: String,
    predefined: ReadPredefinedUniforms,
    material: graphics.Material,
    environment: graphics.Environment,
    programUniforms: Map[String, Defined[UncheckedBinding]]
  ) :Object = {
    
    val bindingFromName = (name: String) => resolveRootUniform(name, predefined, material, environment, programUniforms)
    val remapped = PathUtil.remap(path, null)//XXX uniformRemapping
    
    if (remapped.contains("[*]")) {
      try {
        val (rootListPath, remappedListPath) = PathUtil.remappedList(remapped)
        val rootList = resolveUniformPath(rootListPath, predefined, material, environment, programUniforms)
        rootList match {
          
          case list: BindingList[_] =>
            RemappedList.unchecked(list.asInstanceOf[BindingList[UncheckedBinding]], remappedListPath)
            
          case _ =>
            null
        }
      }
      catch {
        case e: IllegalArgumentException => null
      }
    }
    else {
      PathUtil.resolve(remapped, bindingFromName)
    }
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
