/*
 * Simplex3dEngine - Renderer Module
 * Copyright (C) 2011, Aleksey Nikiforov
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
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.types._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


sealed abstract class Shader {
  
  final class Declaration(
    val manifest: ClassManifest[_ <: TechniqueBinding],
    val qualifiers: String, val name: String
  ) {
    def extractTypeString(m: ClassManifest[_]) :String = {
      m.erasure.getSimpleName() + {
        m.typeArguments match {
          case List(t: ClassManifest[_]) => "[" + extractTypeString(t) + "]"
          case _ => ""
        }
      }
    }
    override def toString() :String = {
      "Declaraion(" + qualifiers + " " + extractTypeString(manifest) + " " + name + ")"
    }
  }
  
  final class Block(val name: String, val declarations: ReadSeq[Declaration]) {
    override def toString() :String = {
      "Block('" + name + "')(\n" + declarations.mkString("  ", "\n", "") + "\n)"
    }
  }
  
  
  private[this] val _functionDependencies = new ArrayBuffer[String]
  private[this] val _uniformBlocks = new ArrayBuffer[Block]
  private[this] val _inputBlocks = new ArrayBuffer[Block]
  private[this] val _outputBlocks = new ArrayBuffer[Block]
  private[this] val _sources = new ArrayBuffer[String]
  private[this] var _export: Option[String] = None
  
  val functionDependencies = new ReadSeq(_functionDependencies)
  val uniformBlocks = new ReadSeq(_uniformBlocks)
  val inputBlocks = new ReadSeq(_inputBlocks)
  val outputBlocks = new ReadSeq(_outputBlocks)
  val sources = new ReadSeq(_sources)
  def export = _export

  
  private[this] var isRegistered = false
  private[this] def checkState() {
    if (isRegistered) throw new IllegalStateException("Modifying shader after it has been registered.")
  }
  def register() { isRegistered = true }
  
    
  private[this] var declarations: ArrayBuffer[Declaration] = null
  private[this] def processBlock(name: String, blocks: ArrayBuffer[Block], blockBody: => Unit) {
    declarations = new ArrayBuffer[Declaration]
    blockBody
    blocks += new Block(name, new ReadSeq(declarations))
    declarations = null
  }
  
  protected final def uses(functionSignature: String) {
    checkState()
    _functionDependencies += functionSignature
  }
  
  protected final def declare[B <: TechniqueBinding : ClassManifest](name: String) {
    declare("", name)
  }
  protected final def declare[B <: TechniqueBinding : ClassManifest](qualifiers: String, name: String) {
    checkState()
    if (declarations == null) throw new IllegalStateException("declare() must be called inside a block.")
    declarations += new Declaration(implicitly[ClassManifest[B]], qualifiers, name)
  }
  
  /** Anonymous uniform block must contain top-level bindings.
   */
  protected final def uniform(block: => Unit) {
    uniform(null: String)(block)
  }
  
  /** The name of the uniform block must resolve to an instance of NestedBinding.
   */
  protected final def uniform(name: String)(block: => Unit) {
    checkState()
    processBlock(name, _uniformBlocks, block)
  }
  
  /** Vertex shader: if the name of the inBlock cannot be resolved then attributes will be used as input.
   */
  protected final def in(name: String)(block: => Unit) {
    checkState()
    processBlock(name, _inputBlocks, block)
  }
  
  /** A shader must export one function or define one output block (but not both).
   * Fragment shader: the name of the outBlock will be ignored if there are no further shader stages.
   */
  protected final def out(name: String)(block: => Unit) {
    checkState()
    processBlock(name, _outputBlocks, block)
  }
  
  /** A shader must export one function or define one output block (but not both).
   */
  protected final def exports(functionSignature: String) {
    checkState()
    if (export != None) throw new IllegalStateException("Export is already defined.")
    _export = Some(functionSignature)
  }
  
  protected final def src(src: String) {
    checkState()
    _sources += src
  }
  
  
  def generateFullSource() :String = {
    var src = "\n"
      
    for (dep <- functionDependencies) { src += dep + ";\n" }
    src += "\n"
      
    
    src
  }
}

abstract class VertexShader extends Shader
abstract class FragmentShader extends Shader
