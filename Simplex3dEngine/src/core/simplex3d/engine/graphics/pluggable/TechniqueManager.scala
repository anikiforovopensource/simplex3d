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

import java.util.logging._
import java.util.HashMap
import scala.collection._
import scala.collection.mutable.ArrayBuffer
import simplex3d.engine.util._
import simplex3d.engine.graphics._


class TechniqueManager[G <: GraphicsContext](implicit val graphicsContext: G)
extends graphics.TechniqueManager[G]
{
  import TechniqueManager.logger._
  
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
    //XXX verify that declarations match material types, verify env key is present
    //XXX special treatment for gl_Position => gl_FragCoord
    shader match {
      case _: FragmentShader => stages(0).register(shader)
      case _: VertexShader => stages(1).register(shader)
    }
    shader.init()
  }
  
  
  private[this] val dummyPredefined = new PredefinedUniforms()
  
  
  def resolveTechnique(//XXX handle shader uniforms config.
    meshName: String,
    geometry: G#Geometry, material: G#Material, worldEnvironment: G#Environment
  )
  :Technique =
  {
    
    def resolveShaderChain(stageId: Int, shader: ShaderPrototype) :ArrayBuffer[ShaderPrototype] = {
      val stage = stages(stageId)
      val chain = new ArrayBuffer[ShaderPrototype]
      chain += shader
      

      var uniformsPassed = true
      if (!shader.uniformBlock.isEmpty) { def checkUniform() {
        val declarations = shader.uniformBlock
        
        var i = 0; while (uniformsPassed && i < declarations.size) {
          uniformsPassed = false
          val declaration = declarations(i)
          
          if (declaration.isPredefined && declaration.name == "se_pointSize") {
            uniformsPassed = (geometry.mode == PointSprites)
          }
          else {
            val resolved = graphicsContext.resolveUniformPath(
              declaration.name, dummyPredefined, material, worldEnvironment, shader.shaderUniforms)
            
            uniformsPassed = (resolved != null)//XXX check the type at runtime
            
            if (shader.logRejected && !uniformsPassed) log(
              Level.INFO, "Shader '" + shader.name + "' was rejected because the uniform '" +
              declaration.name + "' is not defined."
            )
          }
          
          i += 1
        }
      }; checkUniform() }
      
      
      var attributesPassed = uniformsPassed
      if (attributesPassed && !shader.attributeBlock.isEmpty) { def checkAttributes() {
        val declarations = shader.attributeBlock
        var i = 0; while (attributesPassed && i < declarations.size) {
          attributesPassed = false
          val declaration = declarations(i)
          
          val attrib = graphicsContext.resolveAttributePath(declaration.name, geometry)
          attributesPassed = (attrib != null)//XXX check the type at runtime
          
          if (shader.logRejected && !attributesPassed) log(
            Level.INFO, "Shader '" + shader.name + "' was rejected because the attribute '" +
            declaration.name + "' is not defined."
          )
          
          i += 1
        }
      }; checkAttributes() }
      
      
      var functionsPassed = attributesPassed
      if (functionsPassed && !shader.functionDependencies.isEmpty) { def checkFunctions() {
        val requiredFunctions = shader.functionDependencies
        
        var i = 0; while (functionsPassed && i < requiredFunctions.size) {
          val requiredFunction = requiredFunctions(i)
          functionsPassed = false
          
          var j = 0; while (!functionsPassed && j < stage.internal.size) {
            val functionProvider = stage.internal(j)
            
            if (requiredFunction == functionProvider.export.get) {
              // Do not use set, use an ordered list, because the order is important.
              if (!chain.contains(functionProvider)) {
                val subChain = resolveShaderChain(stageId, functionProvider)
                if (subChain != null) {
                  chain ++= subChain
                  functionsPassed = true
                }
              }
              else functionsPassed = true
            }
            
            j += 1
          }
          
          if (shader.logRejected && !functionsPassed) log(
            Level.INFO, "Shader '" + shader.name + "' was rejected because the required function '" +
            requiredFunction + "' could not be resolved."
          )
          
          i += 1
        }
      }; checkFunctions() }
      
      
      var inputPassed = functionsPassed
      if (inputPassed && !shader.inputBlocks.isEmpty) { def checkInput() {
        val previousStage = if (stageId + 1 == stages.size) null else stages(stageId + 1)
        val inputBlocks = shader.inputBlocks
        
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
              if (!chain.contains(outputShader)) {// Do not use set, the order is important.
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
          
          if (shader.logRejected && !inputPassed) log(
            Level.INFO, "Shader '" + shader.name + "' was rejected because the input block '" +
            inputBlock.name + "' could not be resolved."
          )
          
          i += 1
        }
      }; checkInput() }
      
      if (uniformsPassed && functionsPassed && inputPassed) {
        if (shader.logAccepted) log(
          Level.INFO, "Shader '" + shader.name + "' was accepted."
        )
        
        chain
      }
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
      
      // Generate declaration map.
      val listDeclarationMap = new HashMap[(String, String), ListDeclarationSizeKey]
      def processValue(value: AnyRef, name: String) {
        value match {
          
          case a: BindingList[_] =>
            val dec = new ListDeclarationSizeKey("", name, a.size)
            val prev = listDeclarationMap.put(dec.nameKey, dec)
            if (prev != null && prev.size != dec.size) println("XXX log")
            
          case s: Struct[_] =>
            var i = 0; while (i < s.listDeclarations.size) {
              val dec = s.listDeclarations(i)
              val sizeKey = dec.sizeKey()
              val prev = listDeclarationMap.put(dec.nameKey, sizeKey)
              if (prev != null && prev.size != sizeKey.size) println("XXX log")
              
              i += 1
            }
            
          case _ =>
            // ignore
        }
      }
      
      var i = 0; while (i < material.uniforms.size) {
        val prop = material.uniforms(i)
        if (prop.isDefined) processValue(prop.get, material.uniformNames(i))
        
        i += 1
      }
      
      i = 0; while (i < worldEnvironment.properties.size) {
        val prop = worldEnvironment.properties(i)
        if (prop.isDefined) processValue(prop.get.binding, worldEnvironment.propertyNames(i))
        
        i += 1
      }
      
      
      // Generate shader and technique keys. 
      val techniqueKey = new Array[(ShaderPrototype, IndexedSeq[ListDeclarationSizeKey])](chain.size)
      
      i = 0; while (i < chain.size) {
        val shader = chain(i)
        val remappedKeys = shader.listDeclarationNameKeys
        
        val resolved = new Array[ListDeclarationSizeKey](remappedKeys.size)
        var j = 0; while (j < remappedKeys.size) {
          val key = remappedKeys(j)
          
          val listDeclaration = listDeclarationMap.get(key)
          assert(listDeclaration != null)
          resolved(j) = listDeclaration
          
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
            
            val listDeclarations = shaderKey._2
            val src = ShaderPrototype.genSrc_Glsl120(proto, listDeclarations)
            
            shader = new Shader(proto.shaderType, src, proto.shaderUniforms)
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
      
      //println("XXX\n" + technique.shaders.map(_.src).mkString("\n\n*****************************************\n\n"))
      
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
  
  
  private val shaderCache = new HashMap[(ShaderPrototype, IndexedSeq[ListDeclarationSizeKey]), Shader]
  private val techniqueCache = new HashMap[IndexedSeq[(ShaderPrototype, IndexedSeq[ListDeclarationSizeKey])], Technique]
}


object TechniqueManager {
  val logger = Logger.getLogger(this.getClass.getName)
}
