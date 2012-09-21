/*
 * Simplex3dEngine - Renderer Module
 * Copyright (C) 2012, Aleksey Nikiforov
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
package graphics.pluggable

import scala.collection._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


final class Declaration(
  val qualifiers: String,
  val manifest: ClassManifest[_ <: Binding],
  val name: String,
  val arraySizeExpression: String
) {
  import scala.collection.mutable.HashMap//XXX
  import scala.collection.mutable.ArrayBuffer//XXX
  
  val isPredefined = name.startsWith("se_")
  val isReserved = name.startsWith("gl_")
  val isArray: Boolean = classOf[BindingList[_]].isAssignableFrom(manifest.erasure)
  
  
  private[pluggable] def getType(squareMatrices: Boolean) :String = {
    extractManifestTypeInfo(squareMatrices, 0, "", manifest, name, null)
  }
  private[pluggable] def getStructs(
    squareMatrices: Boolean,
    structBlocks: HashMap[String, StructSignature]
  ) {
    extractManifestTypeInfo(squareMatrices, 0, "", manifest, name, structBlocks)
  }
  
  private def resolveMathType(squareMatrices: Boolean, erasure: Class[_]) :String = {
    val className = ClassUtil.simpleName(erasure).toLowerCase()
    val unprefixedName = 
      if (className.startsWith("const")) className.drop(5)
      else if (className.startsWith("read")) className.drop(4)
      else className
    
    if (unprefixedName.endsWith("ref")) {
      val shorterName = unprefixedName.dropRight(3)
      if (shorterName == "double") "float" else shorterName
    }
    else if (unprefixedName.startsWith("vec") && unprefixedName.endsWith("i")) {
      "i" + unprefixedName.dropRight(1)
    }
    else if (unprefixedName.startsWith("mat") && squareMatrices) {
      unprefixedName.dropRight(1) match {
        case "mat2" => "mat2"
        case "mat2x3" | "mat3x2" | "mat3" => "mat3"
        case _ => "mat4"
      }
    }
    else {
      unprefixedName.dropRight(1)
    }
  }
  private def resolveTextureType(
    className: String,
    level: Int, dependenciesResult: HashMap[String, StructSignature]
  ) :String = {
    
    className match {
      case "Texture2d" =>
        val glslType = "Se_Texture2d"
          
        val sdec = new StructSignature(
          level, classOf[Texture2d[_]], glslType,
          new ReadSeq(ArrayBuffer(
            ("sampler2D", "sampler"),
            ("ivec2", "dimensions"))),
          true
        )
        
        dependenciesResult.put(glslType, sdec)
        glslType
        
      case _ => throw new RuntimeException
    }
  }
  
  private def processStruct(
    squareMatrices: Boolean,
    level: Int,
    instance: Struct,
    dependenciesResult: HashMap[String, StructSignature]
  ) :String =
  {
    val glslType = ClassUtil.simpleName(instance.getClass)
    if (dependenciesResult == null) return glslType

    val subStructs = new HashMap[String, StructSignature]
    val entries = new ArrayBuffer[(String, String)]
    var containsSamplers = false
    
    for (i <- 0 until instance.fieldNames.length) {
      val fieldName = instance.fieldNames(i)
      val field = instance.fields(i)
      
      val (fieldType, arraySizeExpression) = extractInstanceTypeInfo(
        squareMatrices, level + 1, glslType, field, fieldName, subStructs
      )
      if (classOf[TextureBinding[_]].isAssignableFrom(field.getClass)) containsSamplers = true
      val append = if (arraySizeExpression != "") "[" + arraySizeExpression + "]" else ""
      entries += ((fieldType, fieldName + append))
    }
    
    if (subStructs.values.find(_.containsSamplers).isDefined) containsSamplers = true
    dependenciesResult ++= subStructs//XXX use list, then check all at the very end.
    
    val existing = dependenciesResult.get(glslType)
    if (existing.isDefined) {
      val existingClass = existing.get.erasure
      if (instance.getClass != existingClass) throw new RuntimeException(
        "Both structs '" + instance.getClass.getName + "' and '" +
        existingClass.getName + "' map to the same glsl type."
      )
    }
    
    if (!existing.isDefined || existing.get.level < level) {
      val dec = new StructSignature(level, instance.getClass, glslType, new ReadSeq(entries), containsSamplers)
      dependenciesResult.put(glslType, dec)
    }
    
    glslType
  }
  
  private def extractManifestTypeInfo(
    squareMatrices: Boolean,
    level: Int,
    parentType: String,
    manifest: ClassManifest[_], name: String,
    dependenciesResult: HashMap[String, StructSignature]
  ) :String = { // glslType
    
    if (classOf[MathType].isAssignableFrom(manifest.erasure)) {
      resolveMathType(squareMatrices, manifest.erasure)
    }
    else if (classOf[TextureBinding[_]].isAssignableFrom(manifest.erasure)) {
      try {
        val erasure = manifest.typeArguments.head.asInstanceOf[ClassManifest[_]].erasure
        resolveTextureType(ClassUtil.simpleName(erasure), level, dependenciesResult) 
      }
      catch {
        case e: Exception => throw new RuntimeException(
          "Undefined or unsupported texture binding type for declaration '" + name + "'."
        )
      }
    }
    else if (classOf[BindingList[_]].isAssignableFrom(manifest.erasure)) {
      val listManifest = try {
        manifest.typeArguments.head.asInstanceOf[ClassManifest[_]]
      }
      catch {
        case e: Exception => throw new RuntimeException(
          "Undefined or unsupported array type for declaration '" + name + "'."
        )
      }
      
      val glslType = extractManifestTypeInfo(squareMatrices, level, parentType, listManifest, name, dependenciesResult)
      glslType
    }
    else if (classOf[Struct].isAssignableFrom(manifest.erasure)) {
      val instance = try {
        manifest.erasure.newInstance().asInstanceOf[Struct]
      }
      catch {
        case e: Exception => throw new RuntimeException(
          "Unable to create an instance of '" + manifest.erasure.getName +
          "'. All Struct subclasses must define a no-argument constructor."
        )
      }
      
      processStruct(squareMatrices, level, instance, dependenciesResult)
    }
    else {
      throw new RuntimeException(
        "Unsupported type '" + ClassUtil.simpleName(manifest.erasure) + "' for declaration '" + name + "'."
      )
    }
  }
  
  private def extractInstanceTypeInfo(
    squareMatrices: Boolean,
    level: Int,
    parentType: String,
    instance: Object, name: String,
    dependenciesResult: HashMap[String, StructSignature]
  ) :(String, String) = {//(glslType, sizeExpression)
    
    if (classOf[MathType].isAssignableFrom(instance.getClass)) {
      (resolveMathType(squareMatrices, instance.getClass), "")
    }
    else if (classOf[TextureBinding[_]].isAssignableFrom(instance.getClass)) {
      val erasure = instance.asInstanceOf[TextureBinding[_]].bindingManifest.erasure
      (resolveTextureType(ClassUtil.simpleName(erasure), level, dependenciesResult), "")
    }
    else if (classOf[BindingList[_]].isAssignableFrom(instance.getClass)) {
      val manifest = instance.asInstanceOf[BindingList[_]].elementManifest
      val glslType = extractManifestTypeInfo(squareMatrices, level, parentType, manifest, name, dependenciesResult)
      
      val sizeExpression =
        if (!arraySizeExpression.isEmpty) arraySizeExpression
        else ShaderPrototype.arraySizeId(parentType, name)
      
      (glslType, sizeExpression)
    }
    else if (classOf[Struct].isAssignableFrom(instance.getClass)) {
      (processStruct(squareMatrices, level, instance.asInstanceOf[Struct], dependenciesResult), "")
    }
    else {
      throw new RuntimeException(
        "Unsupported type '" + ClassUtil.simpleName(instance.getClass) + "' for declaration '" + name + "'."
      )
    }
  }
  
  override def toString() :String = {
    def extractTypeString(m: ClassManifest[_]) :String = {
      ClassUtil.simpleName(m.erasure) + {
        m.typeArguments match {
          case List(t: ClassManifest[_]) => "[" + extractTypeString(t) + "]"
          case _ => ""
        }
      }
    }
    "Declaraion(" + qualifiers + " " + extractTypeString(manifest) + " " + name + ")"
  }
  
  override def equals(o: Any) :Boolean = {
    if (this eq o.asInstanceOf[AnyRef]) {
      true
    }
    else o match {
      
      case d: Declaration =>
        manifest == d.manifest &&
        name == d.name &&
        qualifiers == d.qualifiers &&
        arraySizeExpression == d.arraySizeExpression
        
      case _ =>
        false
    }
  }
  
  override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + manifest.hashCode
        ) + name.hashCode
      ) + qualifiers.hashCode
    ) + arraySizeExpression.hashCode
  }
}
