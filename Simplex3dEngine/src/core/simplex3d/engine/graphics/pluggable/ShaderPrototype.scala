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

import java.lang.Integer
import java.util.logging._
import java.util.HashMap
import scala.collection._
import scala.collection.mutable.ArrayBuilder
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


//XXX add (val pack, val name) // log as {val name = pack.chainKey + "." + pack.name + "." + name}
final class ShaderPrototype private[pluggable] (
  val debugging: ShaderDebugging,
  val shaderType: Shader.type#Value,
  val version: String,
  val squareMatrices: Boolean,
  val functionSignature: Option[String],
  val mainLabel: Option[String],
  val mainInputs: ReadArray[DeclarationBlock],
  val mainOutput: Option[DeclarationBlock],
  val body: String,
  val conditions: ReadArray[(String, AnyRef => Boolean)],
  val uniformBlock: ReadArray[Declaration],
  val boundUniforms: immutable.Map[String, Property[UncheckedBinding]],
  val unsizedArrayKeys: ReadArray[ListNameKey],
  val sizedArraykeys: ReadArray[ListSizeKey],
  val attributeBlock: ReadArray[Declaration],
  val inputBlocks: ReadArray[DeclarationBlock],
  val outputBlock: Option[DeclarationBlock],
  val functionDependencies: ReadArray[String],
  val sources: List[String]
) {
  
  val name: String = {
    
    val shaderTypeString = shaderType match {
      case Shader.Fragment => "Frag"
      case Shader.Vertex => "Vert"
    }
    
    val sigType = if (functionSignature.isDefined) "Function" else "Main"
    
    val sig = {
      if (functionSignature.isDefined) functionSignature.get
      else {
        "void " + mainLabel.get + "(" +
        {
          (for (block <- mainInputs) yield "in " + block.name) ++
          (if (mainOutput.isDefined) "out " + mainOutput.get.name else "")
        }.mkString(", ") +
        ")"
      }
    }
    
    val inInterface = if (inputBlocks.size > 0) {
      "in { " +
      (for (block <- inputBlocks) yield block.name).mkString(", ") +
      " }"
    }
    else ""
      
    val outInterface = if (outputBlock.isDefined) {
      "out { " + outputBlock.get.name +  " }"
    }
    else ""
    
    shaderTypeString + sigType + "{ " + sig +
    (if (inInterface.isEmpty) "" else "; " + inInterface) +
    (if (outInterface.isEmpty) "" else "; " + outInterface) +
    " }"
  }
  
  def isVertexShader = (shaderType == Shader.Vertex)
  val structs = StructSignature.organizeDependencies(uniformBlock.flatMap(_.structSignatures))
  
  val nestedSamplers = {
    import ShaderPrototype.logger._
    
    val map = new HashMap[String, NestedSampler]
    
    var i = 0; while (i < uniformBlock.size) {
      val declaration = uniformBlock(i)
      
      val samplers = declaration.nestedSamplers
      var j = 0; while (j < samplers.size) {
        val sampler = samplers(j)
        
        val existing = map.put(sampler.name, sampler)
        if (existing != null && (existing.parentErasure ne sampler.parentErasure)) log(Level.WARNING,
          "Nested sampler remapping '" + sampler.name + "' is ambiguous, it resolves to values from '" +
          existing.parentErasure.getName + "' and '" + sampler.parentErasure.getName + "'."
        )
        
        j += 1
      }
      
      i += 1
    }
    
    val result = map.values().toArray(new Array[NestedSampler](0))
    new ReadArray(result)
  }
  
  def shaderKey(sizeMap: HashMap[ListNameKey, Integer]): (ShaderPrototype, IndexedSeq[ListSizeKey]) = {
    val result = new Array[ListSizeKey](unsizedArrayKeys.size)
    
    var i = 0; while (i < unsizedArrayKeys.size) {
      val key = unsizedArrayKeys(i)
      
      val size = sizeMap.get(key)
      assert(size != null)
      result(i) = new ListSizeKey(key, size)
      
      i += 1
    }
    
    Tuple2(this, new ReadArray(result))
  }
}


object ShaderPrototype {
  
  private final val logger = Logger.getLogger(classOf[ShaderPrototype].getName)

  
  private[this] def formatOption(option: Option[String]) :String = {
    if (option.isDefined) option.get + " " else ""
  }
  
  private[this] def formatBlock(block: String) :String = {
    if (block.isEmpty) "" else block + "\n\n\n"
  }
  
  private[this] def unindent(src: String) :Array[String] = {
    val codeLines = src.split("\n")

    val lines = codeLines.map(_.replace("\t", "  "))
    val Spaces = """^(\s*).*""".r
    
    var min = scala.Int.MaxValue
    for (line <- lines) {
      val Spaces(s) = line
      if (!line.trim.isEmpty && s.length < min) min = s.length
    }

    lines.map(_.drop(min))
  }
  
  def mkRemapping(parent: String, name: String) :(String, String) = {// (from, to)
    val remapped = if (name.startsWith("gl_")) name else "tm_" + parent + "_" + name
    (parent + "." + name, remapped)
  }
  
  private[this] def remapSrc(remapping: Seq[(String, String)], src: String) :String = {
    var res = src
    for (rm <- remapping) {
      res = res.replaceAll("\\b" + rm._1 + "\\b", rm._2)
    }
    res
  }
  
  private[pluggable] def arraySizeId(key: ListNameKey) :String = {
    "se_sizeOf_" + (if (key.parentType == "") "" else key.parentType + "_") + key.name
  }
  
  
  private[pluggable] def sizeDeclaration(arrayDeclarations: Iterable[ListSizeKey]) :String = {
    (for (d <- arrayDeclarations) yield {
      "const int " + arraySizeId(d.nameKey) + " = " + d.size + ";"
    }).mkString("\n")
  }
  
  
  private[pluggable] def functionDeclaration(functions: Iterable[String]) :String = {
    if (functions.isEmpty) "" else functions.mkString("", ";\n", ";")
  }
  
  private[pluggable] def structDeclaration(structs: Iterable[StructSignature]) :String = {
    (for (struct <- structs) yield {
      "struct " + struct.glslType + " {\n" +
      (for ((fieldType, fieldName) <- struct.entries) yield {
        "  " + fieldType + " " + fieldName + ";"
      }).mkString("\n") +
      "\n};"
    }).mkString("\n\n")
  }
  
  def varDeclaration(blockQualifier: Option[String], declarations: Iterable[Declaration]) :String =  {
    if (declarations.isEmpty) return ""
    
    (for (declaration <- declarations; if !declaration.isReserved) yield {
      formatOption(declaration.qualifiers) + formatOption(blockQualifier) + declaration.glslType + " " +
      {
        if (declaration.isArray) {
          if (declaration.arraySizeExpression.isDefined) {
            declaration.name + "[" + declaration.arraySizeExpression.get + "]"
          }
          else {
            val sizeExpression = arraySizeId(new ListNameKey("", declaration.name))
            declaration.name + "[" + sizeExpression + "]"
          }
        }
        else declaration.name
      } + ";"
    }).mkString("\n")
  }
  
  def nestedSamplerDeclaration(nestedSamplers: Iterable[NestedSampler]) :String =  {
    if (nestedSamplers.isEmpty) return ""
    
    (for (declaration <- nestedSamplers) yield {
      "uniform " + declaration.qualifiedType + " " + declaration.name +
      (if (declaration.arraySizeExpression.isDefined) "[" + declaration.arraySizeExpression.get + "]" else "") +
      ";"
    }).mkString("\n")
  }
  
  def bodySignature(shader: ShaderPrototype)
  :(Seq[(String, String)], String) = //(remapping, varDeclaration)
  {
    def process(qualifier: String, blocks: Iterable[DeclarationBlock], remappings: ArrayBuilder[(String, String)]) = {
      val entries =
        for (block <- blocks; declaration <- block.declarations) yield {
          val remapping = mkRemapping(block.name, declaration.name)
          remappings += remapping
          
          val argument = formatOption(declaration.qualifiers) + qualifier + " " + declaration.glslType + " " + {
            if (declaration.isArray) remapping._2 + "[" + declaration.arraySizeExpression + "]"
            else remapping._2
          }
          
          (remapping._2, argument)
        }
      
      entries.toList.sortBy(_._1).map(_._2)
    }

    if (shader.functionSignature.isDefined) (Nil, shader.functionSignature.get)
    else {
      val remappings = ArrayBuilder.make[(String, String)]
      
      val inputs = process("in", shader.mainInputs, remappings)
      val outputs = process("out", shader.mainOutput, remappings)
      
      val sig = "void " + shader.mainLabel.get + "(" + inputs.mkString(", ") + outputs.mkString(", ") + ")"
      (remappings.result, sig)
    }
  }
  
  def callSignature(shader: ShaderPrototype) :String = {
    def process(blocks: Iterable[DeclarationBlock]) = {
      val entries = for (block <- blocks; declaration <- block.declarations) yield {
        val remapping = mkRemapping(block.name, declaration.name)
        
        remapping._2
      }
      
      entries.toList.sorted
    }

    if (shader.functionSignature.isDefined) ""
    else {
      val inputs = process(shader.mainInputs)
      val outputs = process(shader.mainOutput)
      
      shader.mainLabel.get + "(" + inputs.mkString(", ") + outputs.mkString(", ") + ")"
    }
  }
  
  def extraSources(remapping: Seq[(String, String)], sources: List[String]) :String = {
    (for (srcChunk <- sources) yield {
      unindent(remapSrc(remapping, srcChunk)).mkString("\n").trim
    }).mkString("\n")
  }
  
  def bodySource(remapping: Seq[(String, String)], signature: String, body: String) :String = {
    val lines = unindent(remapSrc(remapping, body))
    
    signature + " {\n" +
    "  " + lines.map("  " + _).mkString("\n").trim +
    "\n}"
  }
  
  
  object Glsl2 {
    
    def interfaceDeclaration(blocks: Iterable[DeclarationBlock])
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
              formatOption(declaration.qualifiers) + "varying " + declaration.glslType + " " +
              {
                if (declaration.isArray) remapped + "[" + declaration.arraySizeExpression.get + "]"
                else remapped
              } +  ";"
            }
          }).filter(!_.isEmpty).mkString("\n")
        }).mkString("\n\n")
      
      (remappings.result, src)
    }
    
    
    def shaderSrc(shader: ShaderPrototype, arrayDeclarations: IndexedSeq[ListSizeKey]) :String = {
      val (varyingRemapping, interfaceDeclarations) = interfaceDeclaration(shader.inputBlocks ++ shader.outputBlock)
      val (mainVarRemapping, signature) = bodySignature(shader)
      
      val remapping = varyingRemapping ++ mainVarRemapping
      
      formatBlock("#version " + shader.version) +
      formatBlock(sizeDeclaration(shader.sizedArraykeys)) +
      formatBlock(sizeDeclaration(arrayDeclarations)) +
      formatBlock(structDeclaration(shader.structs)) +
      formatBlock(varDeclaration(Some("attribute"), shader.attributeBlock)) +
      formatBlock(varDeclaration(Some("uniform"), shader.uniformBlock)) +
      formatBlock(nestedSamplerDeclaration(shader.nestedSamplers)) +
      formatBlock(interfaceDeclarations) +
      formatBlock(functionDeclaration(shader.functionDependencies)) +
      formatBlock(extraSources(varyingRemapping, shader.sources)) +
      bodySource(remapping, signature, shader.body)
    }
  }
}
