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

import java.lang.Integer
import java.util.logging._
import java.util.HashMap
import java.util.HashSet
import java.util.Stack
import scala.collection._
import scala.collection.mutable.ArrayBuffer
import simplex3d.math.double._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


class TechniqueManager[G <: GraphicsContext](implicit val graphicsContext: G)
extends graphics.TechniqueManager[G]
{
  import TechniqueManager.logger._
 
  val passManager = new PassManager[G]//XXX stub
  
  
  protected class Stage(val name: String, val id: Int) {
    val function = new HashMap[String, ArrayBuffer[ShaderPrototype]]
    
    // The key is out{} block name.
    val main = new HashMap[String, ArrayBuffer[ShaderPrototype]]
    
    // The key is out{} block name, "" for no-output (the root shader).
    val interface = new HashMap[String, ArrayBuffer[ShaderPrototype]]
    
    
    def push(shader: ShaderPrototype) {
      def insert(map: HashMap[String, ArrayBuffer[ShaderPrototype]], key: String, shader: ShaderPrototype) {
        
        val pending = new HashMap[String, DeclarationBlock]
        val register = registerNamedBlock(pending)_
        
        shader.inputBlocks.foreach(register)
        shader.outputBlock.foreach(register)
        shader.mainInputs.foreach(register)
        shader.mainOutput.foreach(register)
        
        namedBlocks.putAll(pending)
        
        
        var list = map.get(key)
        if (list == null) {
          list = new ArrayBuffer[ShaderPrototype]
          map.put(key, list)
        }
        list.insert(0, shader)
      }
      
      if (shader.mainLabel.isDefined) {
        if (shader.mainOutput.isDefined) {
          insert(main, shader.mainOutput.get.name, shader)
        }
        else {
          val key = if (shader.shaderType != Shader.Fragment && shader.outputBlock.isDefined) {
            shader.outputBlock.get.name
          }
          else ""
          
          insert(interface, key, shader)
        }
      }
      else {
        insert(function, shader.functionSignature.get, shader)
      }
    }
  }
  
  
  protected val namedBlocks = new HashMap[String, DeclarationBlock]
  protected def registerNamedBlock(pending: HashMap[String, DeclarationBlock])(block: DeclarationBlock) {
    val existing = namedBlocks.get(block.name)
    if (existing == null) pending.put(block.name, block)
    else {
      if (existing != block) throw new RuntimeException(
          "Block '" + block.name + "' is already defined with different declarations.")
    }
  }
  
  protected val stages = new Array[Stage](2)
  stages(0) = new Stage("Fragment", 0)
  stages(1) = new Stage("Vertex", 1)
  
  
  def push(shader: ShaderPrototype) {
    shader.shaderType match {
      case Shader.Fragment => stages(0).push(shader)
      case Shader.Vertex => stages(1).push(shader)
    }
  }
  
  
  private[this] val dummyPredefined = new PredefinedUniforms()
  
  private val arrowsDown = "\n↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓\n"
  private val arrowsUp =   "\n↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑\n"
  private val divider =  "\n\n*******************************************************************************\n\n"
  
  
  def resolveTechnique(
      meshName: String, shaderDebugging: ShaderDebugging,
      geometry: G#Geometry, material: G#Material, worldEnvironment: G#Environment)
  :Technique =
  {
    def logRejected(shader: ShaderPrototype) :Boolean = shaderDebugging.logRejected || shader.debugging.logRejected
    def logAccepted(shader: ShaderPrototype) :Boolean = shaderDebugging.logAccepted || shader.debugging.logAccepted
    
    val quickKey = graphicsContext.collectKeys(geometry, material, worldEnvironment)
    
    val quickLookup = quickCache.get(quickKey)
    if (quickLookup != null) return quickLookup
    
    val listMap = quickKey._1
    val enumMap = quickKey._2
    
    
    def resolveShaderChain(
        stageId: Int, dependencyKey: String, shader: ShaderPrototype,
        resolving: HashSet[ShaderPrototype],
        acceptable: Array[HashMap[String, IndexedSeq[ShaderPrototype]]],
        rejected: HashSet[ShaderPrototype])
    :IndexedSeq[ShaderPrototype] =
    {
      if (rejected.contains(shader)) return IndexedSeq.empty[ShaderPrototype]
      
      if (resolving.contains(shader)) {
        log(Level.INFO,
          "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
          "' because of cyclic dependency."
        )
        
        rejected.add(shader)
        return IndexedSeq.empty[ShaderPrototype]
      }
      
      
      resolving.add(shader)
      val stage = stages(stageId)
      
      
      def checkConditions() :Boolean = {
        val conditions = shader.conditions
        
        var passed = true
        var i = 0; while (passed && i < conditions.size) {
          passed = false
          val (path, condition) = conditions(i)
          
          val resolved = enumMap.get(path)
          if (resolved != null) passed = condition(resolved)
          
          if (!passed && logRejected(shader)) log(Level.INFO,
            "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
            "' because the condition with path '" + path + "' was not satisfied."
          )
          
          i += 1
        }
        
        passed
      }
      
      def checkAttributes() :Boolean = {
        val declarations = shader.attributeBlock
        
        var passed = true;
        var i = 0; while (passed && i < declarations.size) {
          passed = false
          val declaration = declarations(i)
          
          val attrib = graphicsContext.resolveAttributePath(declaration.name, geometry)
          passed = (attrib != null)
          
          if (attrib != null) {
            if (declaration.attributeClass == attrib.src.accessorTag.runtimeClass) {
              passed = true
            }
            else if (logRejected(shader)) log(Level.INFO,
              "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
              "' because the attribute '" + declaration.name + "' is declared as '" +
              ClassUtil.simpleName(declaration.attributeClass) + "' but resolves to a sequence of '" +
              ClassUtil.simpleName(attrib.src.accessorTag.runtimeClass) + "'."
            )
          }
          else if (logRejected(shader)) log(Level.INFO,
            "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
            "' because the attribute '" + declaration.name + "' is not defined."
          )
          
          i += 1
        }
        
        passed
      }
      
      def checkUniform() :Boolean = {
        val declarations = shader.uniformBlock
        
        var passed = true
        var i = 0; while (passed && i < declarations.size) {
          passed = false
          val declaration = declarations(i)
          
          
          val resolved =
            if (declaration.name == "se_pointSpriteSize" && geometry.primitive.get.mode != VertexMode.PointSprites) {
              null
            }
            else {
              graphicsContext.resolveUniformPath(
                  declaration.name, dummyPredefined, material, worldEnvironment, shader.boundUniforms)
            }
          
          
          if (resolved != null) {
            
            import scala.language.existentials
            def adjustedErasure = {
              if (resolved.getClass == Mat2x3.Tag.runtimeClass) Mat2.Tag.runtimeClass
              else if (resolved.getClass == Mat2x4.Tag.runtimeClass) Mat2.Tag.runtimeClass
              else if (resolved.getClass == Mat3x2.Tag.runtimeClass) Mat3.Tag.runtimeClass
              else if (resolved.getClass == Mat3x4.Tag.runtimeClass) Mat3.Tag.runtimeClass
              else if (resolved.getClass == Mat4x2.Tag.runtimeClass) Mat4.Tag.runtimeClass
              else if (resolved.getClass == Mat4x3.Tag.runtimeClass) Mat4.Tag.runtimeClass
              else resolved.getClass
            }
            
            if (
              declaration.uniformClass == resolved.getClass ||
              (shader.squareMatrices && declaration.uniformClass == adjustedErasure)
            ) {
              passed = true
            }
            else if (logRejected(shader)) log(Level.INFO,
              "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
              "' because the uniform '" + declaration.name + "' is declared as '" +
              ClassUtil.simpleName(declaration.uniformClass) + "' but resolves to an instance of '" +
              ClassUtil.simpleName(resolved.getClass) + "'."
            )
          }
          else if (logRejected(shader)) log(Level.INFO,
            "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
            "' because the uniform '" + declaration.name + "' is not defined."
          )
          
          
          i += 1
        }
        
        passed
      }
      
      def resolveFunctions() :Option[IndexedSeq[ShaderPrototype]] = {
        val requiredFunctions = shader.functionDependencies
        
        val chain = new ArrayBuffer[ShaderPrototype]
        var passed = true
        var i = 0; while (passed && i < requiredFunctions.size) {
          val requiredFunction = requiredFunctions(i)
          passed = false
          
          val existing = acceptable(stageId).get(requiredFunction)
          if (existing != null) {
            chain ++= existing
            passed = true
          }
          else {
            val matchingProviders = stage.function.get(requiredFunction)
            var j = 0; while (matchingProviders != null && !passed && j < matchingProviders.size) {
              val functionProvider = matchingProviders(j)
              
              if (requiredFunction == functionProvider.functionSignature.get) {
                val subChain = resolveShaderChain(stageId, requiredFunction, functionProvider, resolving, acceptable, rejected)
                chain ++= subChain
                passed = !subChain.isEmpty
              }
              
              j += 1
            }
          }
          
          if (!passed && logRejected(shader)) log(Level.INFO,
            "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
            "' because the required function '" + requiredFunction + "' could not be resolved."
          )
          
          i += 1
        }
        
        if (passed) Some(chain) else None
      }
      
      def resolveMainArgs() :Option[IndexedSeq[ShaderPrototype]] = {
        val inputBlocks = shader.mainInputs
        
        val chain = new ArrayBuffer[ShaderPrototype]
        var passed = true
        var i = 0; while (passed && i < inputBlocks.size) {
          val inputBlock = inputBlocks(i)
          passed = false
          
          val existing = acceptable(stageId).get(inputBlock.name)
          if (existing != null) {
            chain ++= existing
            passed = true
          }
          else {
            val matchingProviders = stage.main.get(inputBlock.name)
            var j = 0; while (matchingProviders != null && !passed && j < matchingProviders.size) {
              val outputShader = matchingProviders(j)

              val outBlock = outputShader.mainOutput
              val found = outBlock.isDefined && (outBlock.get.name == inputBlock.name)
              
              if (found) {
                val subChain = resolveShaderChain(stageId, inputBlock.name, outputShader, resolving, acceptable, rejected)
                chain ++= subChain
                passed = !subChain.isEmpty
              }
              
              j += 1
            }
          }
          
          if (!passed && logRejected(shader)) log(Level.INFO,
            "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
            "' because because the main input '" + inputBlock.name + "' could not be resolved."
          )
          
          i += 1
        }
        
        if (passed) Some(chain) else None
      }
      
      def resolveInputs() :Option[IndexedSeq[ShaderPrototype]] = {
        val inputBlocks = shader.inputBlocks
        val prevStageId = stageId + 1
        
        if (inputBlocks.isEmpty) return Some(IndexedSeq.empty[ShaderPrototype])
        if (prevStageId >= stages.size) None
        
        
        val prevStage = stages(prevStageId)
        
        
        val chain = new ArrayBuffer[ShaderPrototype]
        var passed = true
        var i = 0; while (passed && i < inputBlocks.size) {
          val inputBlock = inputBlocks(i)
          passed = false
          
          val existing = acceptable(prevStageId).get(inputBlock.name)
          if (existing != null) {
            chain ++= existing
            passed = true
          }
          else {
            val matchingProviders = prevStage.interface.get(inputBlock.name)
            var j = 0; while (matchingProviders != null && !passed && j < matchingProviders.size) {
              val outputShader = matchingProviders(j)
              
              val outBlock = outputShader.outputBlock
              val found = outBlock.isDefined && (outBlock.get.name == inputBlock.name)
              
              if (found) {
                val subChain = resolveShaderChain(stageId + 1, inputBlock.name, outputShader, resolving, acceptable, rejected)
                chain ++= subChain
                passed = !subChain.isEmpty
              }
              
              j += 1
            }
          }
          
          if (!passed && logRejected(shader)) log(Level.INFO,
            "Shader '" + shader.name + "' was rejected for mesh '" + meshName +
            "' because the input block '" + inputBlock.name + "' could not be resolved."
          )
          
          i += 1
        }
        
        if (passed) Some(chain) else None
      }
      
      // Chain order is important for dependency management, duplicate entries are allowed.
      val chain = new ArrayBuffer[ShaderPrototype]
      var passed = checkConditions() && checkAttributes() && checkUniform()
      
      if (passed) {
        passed = false
        
        val functionChain = resolveFunctions()
        if (functionChain.isDefined) {
          
          val mainVarChain = resolveMainArgs()
          if (mainVarChain.isDefined) {
            
            val inputChain = resolveInputs()
            if (inputChain.isDefined) {
              
              chain ++= functionChain.get
              chain ++= mainVarChain.get
              chain ++= inputChain.get
              
              passed = true
            }
          }
        }
      }
      
      
      if (passed) {
        chain += shader
        acceptable(stageId).put(dependencyKey, chain)
        
        if (logAccepted(shader)) log(
          Level.INFO, "Shader '" + shader.name + "' was accepted for mesh '" + meshName + "'."
        )
        
        resolving.remove(shader)
        return chain
      }
      else {
        rejected.add(shader)
        resolving.remove(shader)
        return IndexedSeq.empty[ShaderPrototype]
      }
    }
  
    
    val fragmentStage = stages(0)
    val rootShaders = fragmentStage.interface.get("")
        
    var rawChain = IndexedSeq.empty[ShaderPrototype]
    var i = 0; while (rawChain.isEmpty && rootShaders != null && i < rootShaders.size) {
      val outputShader = rootShaders(i)
      
      rawChain = resolveShaderChain(
          0, "", outputShader,
          new HashSet[ShaderPrototype](),
          Array.fill(2)(new HashMap[String, IndexedSeq[ShaderPrototype]]),
          new HashSet[ShaderPrototype]())
          
      i += 1
    }
    
    
    if (rawChain.isEmpty) return null
    
    val chain = rawChain.distinct
    
    
    // Generate shader and technique keys.
    val techniqueKeyArray = new Array[ShaderKey](chain.size)
    i = 0; while (i < chain.size) {
      techniqueKeyArray(i) = chain(i).shaderKey(listMap)
      
      i += 1
    }
    val techniqueKey = new ReadArray(techniqueKeyArray)
    
    
    var technique = techniqueCache.get(techniqueKey)
    
    if (technique == null) {
      val shaders = new ArrayBuffer[Shader]
      
      var i = 0; while (i < techniqueKeyArray.size) {
        val shaderKey = techniqueKeyArray(i)
        
        var shader = shaderCache.get(shaderKey)
        if (shader == null) {
          val proto = chain(i)
          
          val listSizeKeys = shaderKey._2
          val src = ShaderPrototype.Glsl2.shaderSrc(proto, listSizeKeys)
          
          if (proto.debugging.logGeneratedSource) log(Level.INFO,
            "Shader source for '" + proto.name + "':\n" +
            arrowsDown + 
            src +
            arrowsUp
          )
          
          shader = new Shader(proto.shaderType, src, proto.boundUniforms)
          shaderCache.put(shaderKey, shader)
        }
        
        shaders += shader
        
        i += 1
      }
      
      shaders += genMain(Shader.Fragment, chain)
      shaders += genMain(Shader.Vertex, chain)
      
      technique = new Technique(graphicsContext, shaders.toSet)
      techniqueCache.put(techniqueKey, technique)
    }

    if (shaderDebugging.logGeneratedSource) log(Level.INFO,
      "Complete program source for mesh '" + meshName + "':\n" +
      arrowsDown +
      technique.shaders.map(_.src).mkString(divider) +
      arrowsUp
    )
    
    quickCache.put(quickKey, technique)
    technique
  }

  
  private[this] def genMain(shaderType: Shader.type#Value, chain: IndexedSeq[ShaderPrototype]) :Shader = {
    var body = ""
    var declarations = ""
    val mainVarBlocks = new mutable.HashSet[DeclarationBlock]
    
    var i = 0; while (i < chain.size) {
      val shaderPrototype = chain(i)
      
      if (shaderPrototype.shaderType == shaderType && shaderPrototype.mainLabel.isDefined) {
        mainVarBlocks ++= shaderPrototype.mainInputs
        mainVarBlocks ++= shaderPrototype.mainOutput
        
        declarations += ShaderPrototype.bodySignature(shaderPrototype)._2 + ";\n"//XXX cache bodySig
        body += "  " + ShaderPrototype.callSignature(shaderPrototype) + ";\n"
      }
      
      i += 1
    }
    
    
    val mainVars = for {
      declarationBlock <- mainVarBlocks
      declaration <- declarationBlock.declarations
    } yield {
      val varName = ShaderPrototype.mkRemapping(declarationBlock.name, declaration.name)._2
      (varName, "  " + declaration.glslType + " " + varName + ";")
    }
    
    val mainVarDeclarations = mainVars.toList.sortBy(_._1).map(_._2).mkString("\n")
      
    val shaderSrc =
      declarations + "\n" +
      "void main() {\n" +
        (if(mainVarDeclarations.isEmpty) "" else mainVarDeclarations + "\n\n") +
        body +
      "}\n"
    
    new Shader(shaderType, shaderSrc)
  }
  
  
  // XXX Technique caching must not prevent GC.
  private type ShaderKey = (ShaderPrototype, IndexedSeq[ListSizeKey])
  private val shaderCache = new HashMap[ShaderKey, Shader]
  
  private type TechniqueKey = IndexedSeq[ShaderKey]
  private val techniqueCache = new HashMap[TechniqueKey, Technique]
  
  private type QuickKey = (HashMap[ListNameKey, Integer], HashMap[String, Object], IndexedSeq[Boolean], IndexedSeq[Class[Object]])
  private val quickCache = new HashMap[QuickKey, Technique]
}


object TechniqueManager {
  val logger = Logger.getLogger(this.getClass.getName)
}
