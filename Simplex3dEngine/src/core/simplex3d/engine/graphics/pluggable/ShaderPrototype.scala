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
import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


final class ShaderPrototype(
  val name: String,
  val logAccepted: Boolean,
  val logRejected: Boolean,
  val shaderType: Shader.type#Value,
  val version: String,
  val export: Option[String],
  val entryPoint: Option[String],
  val conditions: ReadArray[(String, AnyRef => Boolean)],
  val uniformBlock: ReadArray[Declaration],
  val boundUniforms: immutable.Map[String, Property[UncheckedBinding]],
  val unsizedArrayKeys: ReadArray[(String, String)],//(structName, arrayName)
  val attributeBlock: ReadArray[Declaration],
  val inputBlocks: ReadArray[DeclarationBlock],
  val outputBlocks: ReadArray[DeclarationBlock],
  val functionDependencies: ReadArray[String],
  val sources: ReadArray[String]
) {
  def isVertexShader = (shaderType == Shader.Vertex)
  def squareMatrices = (version == "120")
  
  val structs = {
    val res = new scala.collection.mutable.HashMap[String, StructSignature]//XXX
    for (declaration <- uniformBlock) {
      declaration.getStructs(squareMatrices, res)
    }
    new ReadArray(res.values.toArray.sortBy(_.level).reverse)
  }
}


object ShaderPrototype {
  
  private[this] def format(qualifiers: String) :String = {
    if (qualifiers.isEmpty) "" else qualifiers + " "
  }
  
  private[pluggable] def arraySizeId(parentType: String, name: String) :String = {
    "se_sizeOf_" + (if (parentType == "") "" else parentType + "_") + name
  }
  
  def genSizeDelarationHeader(arrayDeclarations: Iterable[ListDeclarationKey]) :String = {
    var src = ""
      
    for (d <- arrayDeclarations) {
      src += "const int " + arraySizeId(d.parentType, d.name) + " = " + d.size + ";\n"
    }
    
    if (src.isEmpty) src else src + "\n"
  }
  
  def genStructSignatureHeader_Glsl120(structs: Iterable[StructSignature]) :String = {
    var src = ""
      
    for (struct <- structs) {
      src += "struct " + struct.glslType + " {\n"
      
      for ((fieldType, fieldName) <- struct.entries) {
        src += "  " + fieldType + " " + fieldName + ";\n"
      }
      
      if (struct.containsSamplers) src += "  float nvidiaBugWorkaround;\n"
      src += "};\n\n"
    }
    
    src
  }
  
  def genGlobalDeclarationHeader_Glsl120(
    isVertexShader: Boolean,
    squareMatrices: Boolean,
    attributeBlock: Iterable[Declaration],
    uniformBlock: Iterable[Declaration],
    inputBlocks: Iterable[DeclarationBlock],
    outputBlocks: Iterable[DeclarationBlock]
  ) :(Seq[(String, String)], String) = {
    val inputQualifier = if (isVertexShader) "attribute" else "varying"
    
    val remappings = new ArrayBuffer[(String, String)]
    def mkRemapping(parent: String, name: String) :(String, String) = {
      val remapped = if (name.startsWith("gl_")) name else "tm_" + parent + "_" + name
      (parent + "." + name, remapped)
    }
    
    
    var src = ""
    
    
    if (!uniformBlock.isEmpty) {
      for (declaration <- uniformBlock; if !declaration.isReserved) {
        val glslType = declaration.getType(squareMatrices)
        src += format(declaration.qualifiers) + "uniform " + glslType + " " +
        {
          if (declaration.isArray) declaration.name + "[" + ShaderPrototype.arraySizeId("", declaration.name) + "]"
          else declaration.name
        } +
        ";\n"
      }
      
      src += "\n"
    }
    
    if (!attributeBlock.isEmpty) {
      var appended = false
      for (declaration <- attributeBlock; if !declaration.isReserved) {
        val glslType = declaration.getType(squareMatrices)
        src += format(declaration.qualifiers) + inputQualifier + " " + glslType + " " +
        {
          if (declaration.isArray) declaration.name + "[" + declaration.arraySizeExpression + "]"
          else declaration.name
        } +
        ";\n"
        appended = true
      }
      if (appended) src += "\n"
    }
    
    
    for (block <- inputBlocks; if !block.declarations.isEmpty) {
      var appended = false
      for (declaration <- block.declarations) {
        val remapping = mkRemapping(block.name, declaration.name)
        remappings += remapping
        val remapped = remapping._2
        
        if (!declaration.isReserved) {
          val glslType = declaration.getType(squareMatrices)
          src += format(declaration.qualifiers) + inputQualifier + " " + glslType + " " +
          {
            if (declaration.isArray) remapped + "[" + declaration.arraySizeExpression + "]"
            else remapped
          } +
          ";\n"
          appended = true
        }
      }
      if (appended) src += "\n"
    }
    
    
    for (block <- outputBlocks; if !block.declarations.isEmpty) {
      var appended = false
      for (declaration <- block.declarations) {
        val remapping = mkRemapping(block.name, declaration.name)
        remappings += remapping
        if (!declaration.isReserved && isVertexShader) {
          val glslType = declaration.getType(squareMatrices)
          src += format(declaration.qualifiers) + "varying " + glslType + " " +
          {
            if (declaration.isArray) remapping._2 + "[" + declaration.arraySizeExpression + "]"
            else remapping._2
          } +
          ";\n"
          appended = true
        }
      }
      if (appended) src += "\n"
    }
    
    (remappings, src)
  }
  
  def genFunctionDeclarationHeader(functions: Iterable[String]) :String = {
    if (functions.isEmpty) "" else functions.mkString("", ";\n", ";\n")
  }
  
  private[this] def remap(remapping: Seq[(String, String)], src: String) :String = {
    var res = src
    for (rm <- remapping) {
      res = res.replaceAll("\\b" + rm._1 + "\\b", rm._2)
    }
    res
  }
  
  def genBody(remapping: Seq[(String, String)], sources: Iterable[String]) :String = {
    var src = ""
      
    for (srcChunk <- sources) {
      src += unindent(remap(remapping, srcChunk)) + "\n"
    }
    
    src
  }
  
  private[this] def unindent(src: String) :String = {
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
  
  def genSrc_Glsl120(shader: ShaderPrototype, arrayDeclarations: IndexedSeq[ListDeclarationKey]) :String = {
    val (remapping, globalDeclarations) = genGlobalDeclarationHeader_Glsl120(
      shader.isVertexShader, shader.squareMatrices,
      shader.attributeBlock, shader.uniformBlock, shader.inputBlocks, shader.outputBlocks
    )
    
    "#version " + shader.version + "\n\n" +
    genSizeDelarationHeader(arrayDeclarations) +
    genStructSignatureHeader_Glsl120(shader.structs) +
    globalDeclarations +
    genFunctionDeclarationHeader(shader.functionDependencies) +
    genBody(remapping, shader.sources)
  }
}
