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

import java.util.logging._
import scala.collection._
import scala.collection.mutable.ArrayBuilder
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


final class ShaderPrototype private[pluggable] (
  val logging: ShaderLogging,
  val shaderType: Shader.type#Value,
  val version: String,
  val squareMatrices: Boolean,
  val export: Option[String],
  val entryPoint: Option[String],
  val conditions: ReadArray[(String, AnyRef => Boolean)],
  val uniformBlock: ReadArray[Declaration],
  val boundUniforms: immutable.Map[String, Property[UncheckedBinding]],
  val unsizedArrayKeys: ReadArray[ListNameKey],
  val attributeBlock: ReadArray[Declaration],
  val inputBlocks: ReadArray[DeclarationBlock],
  val outputBlock: Option[DeclarationBlock],
  val functionDependencies: ReadArray[String],
  val sources: ReadArray[String]
) {
  def isVertexShader = (shaderType == Shader.Vertex)
  val structs = StructSignature.organizeDependencies(uniformBlock.flatMap(_.structSignatures))
}


object ShaderPrototype {
  
  private[this] def format(qualifiers: Option[String]) :String = {
    if (qualifiers.isDefined) qualifiers.get + " " else ""
  }
  
  private[this] def formatBlock(block: String) :String = {
    if (block.isEmpty) "" else block + "\n\n\n"
  }
  
  private[pluggable] def arraySizeId(key: ListNameKey) :String = {
    "se_sizeOf_" + (if (key.parentType == "") "" else key.parentType + "_") + key.name
  }
  
  
  def sizeHeader(arrayDeclarations: Iterable[ListSizeKey]) :String = {
    (for (d <- arrayDeclarations) yield {
      "const int " + arraySizeId(d.nameKey) + " = " + d.size + ";"
    }).mkString("\n")
  }
  
  
  def functionHeader(functions: Iterable[String]) :String = {
    if (functions.isEmpty) "" else functions.mkString("", ";\n", ";")
  }
  
  
  object Glsl2 {
    
    def structHeader(structs: Iterable[StructSignature]) :String = {
      (for (struct <- structs) yield {
        "struct " + struct.glslType + " {\n" +
        (for ((fieldType, fieldName) <- struct.entries) yield {
          "  " + fieldType + " " + fieldName + ";"
        }).mkString("\n") +
        (if (struct.containsSamplers) "\n  float nvidiaBugWorkaround;" else "") +
        "\n};"
      }).mkString("\n\n")
    }
    
    
    def attributeHeader(attributeBlock: Iterable[Declaration]) :String =  {
      if (attributeBlock.isEmpty) return ""
      
      (for (declaration <- attributeBlock; if !declaration.isReserved) yield {
        format(declaration.qualifiers) + "attribute " + declaration.glslType + " " +
        {
          if (declaration.isArray) declaration.name + "[" + declaration.arraySizeExpression.get + "]"
          else declaration.name
        } + ";"
      }).mkString("\n")
    }
    
    def uniformHeader(uniformBlock: Iterable[Declaration]) :String = {
      if (uniformBlock.isEmpty) return ""
      
      (for (declaration <- uniformBlock; if !declaration.isReserved) yield {
        format(declaration.qualifiers) + "uniform " + declaration.glslType + " " +
        {
          if (declaration.isArray) {
            val sizeId = ShaderPrototype.arraySizeId(new ListNameKey("", declaration.name))
            declaration.name + "[" + sizeId + "]"
          }
          else declaration.name
        } + ";"
      }).mkString("\n")
    }
    
    private[this] def mkRemapping(parent: String, name: String) :(String, String) = {// (from, to)
      val remapped = if (name.startsWith("gl_")) name else "tm_" + parent + "_" + name
      (parent + "." + name, remapped)
    }
    
    def interfaceHeader(blocks: Iterable[DeclarationBlock])
    :(Seq[(String, String)], String) = //(remapping, varDeclaration)
    {
      val remappings = ArrayBuilder.make[(String, String)]
      
      val src =
        (for (block <- blocks; if !block.declarations.isEmpty) yield {
          (for (declaration <- block.declarations) yield {
            val remapping = mkRemapping(block.name, declaration.name)
            remappings += remapping
            val remapped = remapping._2
            
            if (declaration.isReserved) ""
            else {
              format(declaration.qualifiers) + "varying " + declaration.glslType + " " +
              {
                if (declaration.isArray) remapped + "[" + declaration.arraySizeExpression.get + "]"
                else remapped
              } +  ";"
            }
          }).filter(!_.isEmpty).mkString("\n")
        }).mkString("\n\n")
      
      (remappings.result, src)
    }
    
    def shaderBody(remapping: Seq[(String, String)], sources: Iterable[String]) :String = {
      def remapSrc(remapping: Seq[(String, String)], src: String) :String = {
        var res = src
        for (rm <- remapping) {
          res = res.replaceAll("\\b" + rm._1 + "\\b", rm._2)
        }
        res
      }
      
      def unindent(src: String) :String = {
        val codeLines = src.split("\n")
    
        val lines = codeLines.map(_.replace("\t", "  "))
        val Spaces = """^(\s*).*""".r
        
        var min = scala.Int.MaxValue
        for (line <- lines) {
          val Spaces(s) = line
          if (s != null && !line.trim.isEmpty && s.length < min) min = s.length
        }
    
        lines.map(_.drop(min)).mkString("\n")
      }
      
      (for (srcChunk <- sources) yield {
        unindent(remapSrc(remapping, srcChunk)).trim
      }).mkString("\n")
    }
    
    def shaderSrc(shader: ShaderPrototype, arrayDeclarations: IndexedSeq[ListSizeKey]) :String = {
      val (remapping, interfaceDeclarations) = interfaceHeader(shader.inputBlocks ++ shader.outputBlock)
      
      formatBlock("#version " + shader.version) +
      formatBlock(sizeHeader(arrayDeclarations)) +
      formatBlock(structHeader(shader.structs)) +
      formatBlock(attributeHeader(shader.attributeBlock)) +
      formatBlock(uniformHeader(shader.uniformBlock)) +
      formatBlock(interfaceDeclarations) +
      formatBlock(functionHeader(shader.functionDependencies)) +
      shaderBody(remapping, shader.sources)
    }
  }
}
