/*
 * Simplex3dEngine - Renderer Module
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
package graphics.pluggable

import java.util.HashMap
import scala.collection._
import scala.collection.mutable.ArrayBuffer
import simplex3d.engine.util._
import simplex3d.engine.graphics._


class TechniqueManager[G <: GraphicsContext](implicit val graphicsContext: G)
extends graphics.TechniqueManager[G]
{
  val passManager = new PassManager[G]
  
  
  protected class Stage(val name: String) {
    val internal = new ArrayBuffer[ShaderPrototype]
    val output = new ArrayBuffer[ShaderPrototype]
    
    def register(shader: ShaderPrototype) {
      if (shader.entryPoint.isDefined) output.insert(0, shader)
      else internal.insert(0, shader)
    }
  }
  
  protected val stages = new Array[Stage](2)
  stages(0) = new Stage("Fragment")
  stages(1) = new Stage("Vertex")
  
  
  def register(shader: ShaderPrototype) {
    //XXX verify unique in/out block name has the same declarations
    //XXX verify that declarations match material and env property types.
    //XXX special treatment for gl_Position => gl_FragCoord
    shader match {
      case _: FragmentShader => stages(0).register(shader)
      case _: VertexShader => stages(1).register(shader)
    }
    shader.register()
  }
  
  
  private def toNameIdMap(array: ReadArray[String]) :HashMap[String, java.lang.Integer] = {
    val map = new HashMap[String, java.lang.Integer]
    for (i <- 0 until array.length) {
      map.put(array(i), i)
    }
    map
  }
  private val geometryMap = toNameIdMap(graphicsContext.mkGeometry().attributeNames)
  private val materialMap = toNameIdMap(graphicsContext.mkMaterial().uniformNames)
  private val environmentMap = toNameIdMap(graphicsContext.mkEnvironment().propertyNames)
  
  
  def resolveTechnique(//XXX handle shader config.
    meshName: String,
    geometry: G#Geometry, material: G#Material, worldEnvironment: G#Environment
  )
  :Technique =
  {
    
    def resolveShaderChain(stageId: Int, shader: ShaderPrototype) :ArrayBuffer[ShaderPrototype] = {
      val stage = stages(stageId)
      val chain = new ArrayBuffer[ShaderPrototype]
      chain += shader
      
      // Match uniforms.
      var uniformsPassed = true
      if (shader.uniformBlock.isDefined) { def checkUniform() {
        val declarations = shader.uniformBlock.get.declarations
        
        var i = 0; while (uniformsPassed && i < declarations.size) {
          val declaration = declarations(i)
          uniformsPassed = false
          
          if (declaration.isPredefined) {
            if (declaration.name == "se_pointSize") {
              uniformsPassed = (geometry.mode == PointSprites)
            }
            else {
              uniformsPassed = true
            }
          }
          else {
            val materialId = materialMap.get(declaration.name)
            if (materialId != null) {
              val prop = material.uniforms(materialId)
              uniformsPassed = prop.isDefined
            }
            else {
              val environmentId = environmentMap.get(declaration.name)
              if (environmentId != null) {
                val prop = worldEnvironment.properties(environmentId)
                uniformsPassed = prop.isDefined
              }
            }
          }
          
          i += 1
        }
      }; checkUniform() }
      
      var functionsPassed = uniformsPassed
      if (functionsPassed && !shader.functionDependencies.isEmpty) { def checkFunctions() {
        val requiredFunctions = shader.functionDependencies
        
        var i = 0; while (functionsPassed && i < requiredFunctions.size) {
          val requiredFunction = requiredFunctions(i)
          functionsPassed = false
          
          var j = 0; while (!functionsPassed && j < stage.internal.size) {
            val functionProvider = stage.internal(j)
            
            if (requiredFunction == functionProvider.export.get) {
              val subChain = resolveShaderChain(stageId, functionProvider)
              if (subChain != null) {
                chain ++= subChain
                functionsPassed = true
              }
            }
            
            j += 1
          }
          
          i += 1
        }
      }; checkFunctions() }
      
      var inputPassed = functionsPassed
      if (inputPassed && !shader.inputBlocks.isEmpty) { def checkInput() {
        val inputBlocks = shader.inputBlocks
        val previousStage = if (stageId + 1 == stages.size) null else stages(stageId + 1)
        
        if (previousStage == null) { def checkAttributes() {
          var i = 0; while (inputPassed && i < inputBlocks.size) {
            val inputBlock = inputBlocks(i)
            
            var j = 0; while (inputPassed && j < inputBlock.declarations.size) {
              val declaration = inputBlock.declarations(j)
              
              val geometryId = geometryMap.get(declaration.name)
              if (geometryId != null) {
                val attrib = geometry.attributes(geometryId)
                inputPassed = attrib.isDefined
              }
              
              j += 1
            }
            
            i += 1
          }
        }; checkAttributes() }
        
        else { def checkParentOutput() {
          var i = 0; while (inputPassed && i < inputBlocks.size) {
            val inputBlock = inputBlocks(i)
            
            inputPassed = false
            var j = 0; while (!inputPassed && j < previousStage.output.size) {
              val outputShader = previousStage.output(j)
              val outputBlocks = outputShader.outputBlocks
              
              var found = false
              var k = 0; while (!found && k < outputBlocks.size) {
                found = (inputBlock.name == outputBlocks(k).name)
                
                k += 1
              }
              
              if (found) {
                if (!chain.contains(outputShader)) {
                  val subChain = resolveShaderChain(stageId + 1, outputShader)
                  if (subChain != null) {
                    chain ++= subChain
                    inputPassed = true
                  }
                }
                else inputPassed = true
              }
              
              j += 1
            }
            
            i += 1
          }
        }; checkParentOutput() }
        
      }; checkInput() }
      
      if (uniformsPassed && functionsPassed && inputPassed) chain
      else null
    }
  
    val fragmentStage = stages(0)
    var chain: ArrayBuffer[ShaderPrototype] = null
    var i = 0; while (chain == null && i < fragmentStage.output.size) {
      val outputShader = fragmentStage.output(i)
      chain = resolveShaderChain(0, outputShader)
      i += 1
    }
    
    if (chain != null) {
      
      // Generate array declaration map.
      val arrayDeclarationMap = new HashMap[(String, String), ArrayDeclaration]
      def processValue(value: AnyRef, name: String) {
        value match {
          
          case a: BindingArray[_] =>
            val dec = new ArrayDeclaration("", name, a.size)
            val prev = arrayDeclarationMap.put(dec.key, dec)
            if (prev != null && prev.size != dec.size) println("XXX log")
            
          case s: Struct[_] =>
            var i = 0; while (i < s.arrayDeclarations.size) {
              val dec = s.arrayDeclarations(i)
              val prev = arrayDeclarationMap.put(dec.key, dec)
              if (prev != null && prev.size != dec.size) println("XXX log")
              
              i += 1
            }
            
          case _ =>
            // ignore
        }
      }
      
      var i = 0; while (i < material.uniforms.size) {
        val prop = material.uniforms(i)
        if (prop.isDefined) processValue(prop.defined, material.uniformNames(i))
        
        i += 1
      }
      
      i = 0; while (i < worldEnvironment.properties.size) {
        val prop = worldEnvironment.properties(i)
        if (prop.isDefined) processValue(prop.defined.resolveBinding(), worldEnvironment.propertyNames(i))
        
        i += 1
      }
      
      
      // Generate shader and technique keys. 
      val techniqueKey = new Array[(ShaderPrototype, Seq[ArrayDeclaration])](chain.size)
      
      i = 0; while (i < chain.size) {
        val shader = chain(i)
        val keys = shader.arrayDeclarationKeys
        
        val resolved = new Array[ArrayDeclaration](keys.size)
        var j = 0; while (j < keys.size) {
          val key = keys(j)
          val arrayDeclaration = arrayDeclarationMap.get(key)
          assert(arrayDeclaration != null)
          resolved(i) = arrayDeclaration
          
          j += 1
        }
        
        techniqueKey(i) = Tuple2(shader, new ReadArray(resolved))
        
        i += 1
      }
      
      
      // Load cached technique or make a new one.
      val readTechniqueKey = new ReadArray(techniqueKey)
      var technique = techniqueCache.get(readTechniqueKey)
      
      if (technique == null) {
        val shaders = new ArrayBuffer[Shader]
        
        var i = 0; while (i < techniqueKey.size) {
          val shaderKey = techniqueKey(i)
          
          var shader = shaderCache.get(shaderKey)
          if (shader == null) {
            val proto = chain(i)
            shader = new Shader(proto.shaderType, proto.src_Gl21)
            shaderCache.put(shaderKey, shader)
          }
          
          shaders += shader
          
          i += 1
        }
        
        shaders += genMain(Shader.Fragment, chain)
        shaders += genMain(Shader.Vertex, chain)
        
        technique = new Technique(graphicsContext, shaders.toSet)
        techniqueCache.put(readTechniqueKey, technique)
      }
      
      technique
    }
    else {
      null
    }
  }

  
  private[this] def genMain(shaderType: Shader.type#Value, chain: ArrayBuffer[ShaderPrototype]) :Shader = {
    var body = "void main() {\n"
    var declarations = ""
    
    var i = 0; while (i < chain.size) {
      val sp = chain(i)
      if (sp.shaderType == shaderType && sp.entryPoint.isDefined) {
        declarations += "void " + sp.entryPoint.get + "();\n"
        body += "  " + sp.entryPoint.get + "();\n"
      }
      
      i += 1
    }
    
    body += "}\n"
    
    new Shader(shaderType, declarations + "\n" +  body)
  }
  
  
  private val shaderCache = new HashMap[(ShaderPrototype, Seq[ArrayDeclaration]), Shader]
  private val techniqueCache = new HashMap[Seq[(ShaderPrototype, Seq[ArrayDeclaration])], Technique]
}
