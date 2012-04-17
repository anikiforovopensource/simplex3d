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

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


final class DeclarationBlock(val name: String, val declarations: ReadSeq[Declaration]) {
  override def toString() :String = {
    "DeclarationBlock('" + name + "')(\n" + declarations.mkString("  ", "\n", "") + "\n)"
  }
}

final case class StructDeclaration(
  level: Int,
  erasure: Class[_],
  glslType: String,
  entries: ReadSeq[(String, String)],// (glslType, name) pairs
  containsSamplers: Boolean
) {
  def isNested = (level != 0)
}

final class Declaration(
  val manifest: ClassManifest[_ <: TechniqueBinding],
  val name: String
) {
  val isPredefined = name.startsWith("se_")
  val isReserved = name.startsWith("gl_")
  val isArray: Boolean = classOf[BindingList[_]].isAssignableFrom(manifest.erasure)
  
  private[this] var _qualifiers = ""
  def qualifiers = _qualifiers
  
  
  private[pluggable] var arraySizeExpression: String = null
  
  /** The simple way to link input/output array sizes is using a size of a uniform array: a.length(); 
   */
  def size(arraySizeExpression: String) :this.type = {
    if (!classOf[BindingList[_]].isAssignableFrom(manifest.erasure)) throw new RuntimeException(
      "Only arrays can be sized."
    )
    if (this.arraySizeExpression != null) throw new RuntimeException(
      "This array is already sized."
    )
    
    this.arraySizeExpression = arraySizeExpression
    this
  }
  
  def qualify(qualifiers: String) :this.type = {
    _qualifiers = (_qualifiers + " " + qualifiers).trim
    this
  }
  
  private[pluggable] def extractManifestTypeInfo(squareMatrices: Boolean) :String = {
    extractManifestTypeInfo(squareMatrices, null)
  }
  private[pluggable] def extractManifestTypeInfo(
    squareMatrices: Boolean,
    structBlocks: HashMap[String, StructDeclaration]
  ) :String = {
    extractManifestTypeInfo(squareMatrices, 0, "", manifest, name, structBlocks)
  }
  
  private def resolveMathType(squareMatrices: Boolean, erasure: Class[_]) :String = {
    val className = ClassUtil.simpleName(erasure).toLowerCase()
    val unprefixedName = 
      if (className.startsWith("Const")) className.drop(5)
      else if (className.startsWith("Read")) className.drop(4)
      else className
    
    if (unprefixedName.endsWith("ref")) {
      val shorterName = unprefixedName.dropRight(3)
      if (shorterName == "double") "float" else shorterName
    }
    else if (squareMatrices && unprefixedName.startsWith("mat")) {
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
  private def resolveTextureType(className: String) :String = {
    className match {
      case "Texture2d" => "sampler2D"
      case _ => throw new RuntimeException
    }
  }
  
  private def processStruct(
    squareMatrices: Boolean,
    level: Int,
    instance: Struct[_],
    structBlocks: HashMap[String, StructDeclaration]
  ) :String =
  {
    val glslType = ClassUtil.simpleName(instance.getClass)
    if (structBlocks == null) return glslType

    val subStructs = new HashMap[String, StructDeclaration]
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
    structBlocks ++= subStructs
    
    val existing = structBlocks.get(glslType)
    if (existing.isDefined) {
      val existingClass = existing.get.erasure
      if (instance.getClass != existingClass) throw new RuntimeException(
        "Both structs '" + instance.getClass.getName + "' and '" +
        existingClass.getName + "' map to the same glsl type."
      )
    }
    
    if (!existing.isDefined || existing.get.level < level) {
      val dec = new StructDeclaration(level, instance.getClass, glslType, new ReadSeq(entries), containsSamplers)
      structBlocks.put(glslType, dec)
    }
    
    glslType
  }
  
  private def extractManifestTypeInfo(
    squareMatrices: Boolean,
    level: Int,
    parentType: String,
    m: ClassManifest[_], name: String,
    structBlocks: HashMap[String, StructDeclaration]
  ) :String = {
    
    if (classOf[MathType].isAssignableFrom(m.erasure)) {
      resolveMathType(squareMatrices, m.erasure)
    }
    else if (classOf[TextureBinding[_]].isAssignableFrom(m.erasure)) {
      try {
        val erasure = m.typeArguments.head.asInstanceOf[ClassManifest[_]].erasure
        resolveTextureType(ClassUtil.simpleName(erasure)) 
      }
      catch {
        case e: Exception => throw new RuntimeException(
          "Undefined or unsupported texture binding type for declaration '" + name + "'."
        )
      }
    }
    else if (classOf[BindingList[_]].isAssignableFrom(m.erasure)) {
      val manifest = try {
        m.typeArguments.head.asInstanceOf[ClassManifest[_]]
      }
      catch {
        case e: Exception => throw new RuntimeException(
          "Undefined or unsupported array type for declaration '" + name + "'."
        )
      }
      
      val glslType = extractManifestTypeInfo(squareMatrices, level, parentType, manifest, name, structBlocks)
      glslType
    }
    else if (classOf[Struct[_]].isAssignableFrom(m.erasure)) {
      val instance = try {
        m.erasure.newInstance().asInstanceOf[Struct[_]]
      }
      catch {
        case e: Exception => throw new RuntimeException(
          "Unable to create an instance of '" + m.erasure.getName +
          "'. All Struct subclasses must define a no-argument constructor."
        )
      }
      
      processStruct(squareMatrices, level, instance, structBlocks)
    }
    else {
      throw new RuntimeException(
        "Unsupported type '" + ClassUtil.simpleName(m.erasure) + "' for declaration '" + name + "'."
      )
    }
  }
  
  private def extractInstanceTypeInfo(
    squareMatrices: Boolean,
    level: Int,
    parentType: String,
    i: Object, name: String,
    structBlocks: HashMap[String, StructDeclaration]
  ) :(String, String) = {
    
    if (classOf[MathType].isAssignableFrom(i.getClass)) {
      (resolveMathType(squareMatrices, i.getClass), "")
    }
    else if (classOf[TextureBinding[_]].isAssignableFrom(i.getClass)) {
      val erasure = i.asInstanceOf[TextureBinding[_]].bindingManifest.erasure
      (resolveTextureType(ClassUtil.simpleName(erasure)), "")
    }
    else if (classOf[BindingList[_]].isAssignableFrom(i.getClass)) {
      val manifest = i.asInstanceOf[BindingList[_]].elementManifest
      val glslType = extractManifestTypeInfo(squareMatrices, level, parentType, manifest, name, structBlocks)
      
      val sizeExpression =
        if (arraySizeExpression != null) arraySizeExpression
        else ShaderPrototype.arraySizeId(parentType, name)
      
      (glslType, sizeExpression)
    }
    else if (classOf[Struct[_]].isAssignableFrom(i.getClass)) {
      (processStruct(squareMatrices, level, i.asInstanceOf[Struct[_]], structBlocks), "")
    }
    else {
      throw new RuntimeException(
        "Unsupported type '" + ClassUtil.simpleName(i.getClass) + "' for declaration '" + name + "'."
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
}
