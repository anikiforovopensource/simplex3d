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

import scala.reflect._
import scala.collection._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.data._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


final class Declaration(
  val qualifiers: Option[String],
  val tag: ClassTag[_ <: Binding],
  val glslType: String,
  val name: String,
  val arraySizeExpression: Option[String],
  val structSignatures: ReadArray[StructSignature],
  val nestedSamplers: ReadArray[NestedSampler]
) {
  val isPredefined = name.startsWith("se_")
  val isReserved = name.startsWith("gl_")
  val isArray: Boolean = classOf[BindingSeq[_]].isAssignableFrom(tag.runtimeClass)
  
  private val syntheticName = {
    name match {
      case "gl_Position" => "se_Position_FragCoord"
      case "gl_FragCoord" => "se_Position_FragCoord"
      case _ => name
    }
  }
  
  val attributeTag: ClassTag[_] = {
    tag match {
      case DoubleRef.Tag => PrimitiveFormat.RDouble
      case _ => tag
    }
  }
  
  val uniformTag: ClassTag[_] = {
    if (classOf[BindingSeq[_]].isAssignableFrom(tag.runtimeClass)) {
      try {
        val ct: ClassTag[_] = tag.typeArguments.head.asInstanceOf[ClassManifest[_ <: Binding]]
        ct
      }
      catch {
        case e: Exception => throw new RuntimeException(
          "Undefined or unsupported array type for declaration '" + name + "'."
        )
      }
    }
    else tag
  }
  
  override def toString() :String = {
    "Declaraion(" + qualifiers + " " + glslType + " " + name + ")"
  }
  
  override def equals(o: Any) :Boolean = {
    if (this eq o.asInstanceOf[AnyRef]) {
      true
    }
    else o match {
      
      case d: Declaration =>
        qualifiers == d.qualifiers &&
        tag == d.tag &&
        syntheticName == d.syntheticName &&
        arraySizeExpression == d.arraySizeExpression
        
      case _ =>
        false
    }
  }
  
  override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + qualifiers.hashCode
        ) + tag.hashCode
      ) + name.hashCode
    ) + arraySizeExpression.hashCode
  }
}
