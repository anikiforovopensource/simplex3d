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
  
  final class UniformDeclaraion(val manifest: ClassManifest[_ <: TechniqueBinding], val name: String) {
    override def toString() :String = {
      def extractTypeString(m: ClassManifest[_]) :String = {
        m.erasure.getSimpleName() + {
          m.typeArguments match {
            case List(t: ClassManifest[_]) => "[" + extractTypeString(t) + "]"
            case _ => ""
          }
        }
      }
      "UniformDeclaraion(" + extractTypeString(manifest) + " " + name + ")"
    }
  }
  final class VaryingDeclaraion(
    val manifest: ClassManifest[_ <: TechniqueBinding], val qualifiers: String, val name: String
  ) {
    override def toString() :String = {
      "VaryingDeclaraion(" + qualifiers + " " + manifest.erasure.getName + " " + name + ")"
    }
  }
  final class AttributeDeclaraion(
    val manifest: ClassManifest[_ <: VectorLike], val qualifiers: String, val name: String
  ) {
    override def toString() :String = {
      "AttributeDeclaraion(" + qualifiers + " " + manifest.erasure.getName + " " + name + ")"
    }
  }
  
  abstract class VaryingBlock private[pluggable] (val name: String, registry: ArrayBuffer[VaryingBlock]) {
    checkState()
    registry += this
    
    protected final def varying[B <: TechniqueBinding : ClassManifest](qualifiers: String, name: String) {
      checkState()
      _declarations += new VaryingDeclaraion(implicitly[ClassManifest[B]], qualifiers, name)
    }
    protected final def varying[B <: TechniqueBinding : ClassManifest](name: String) {
      varying("", name)
    }
    
    private[pluggable] var _declarations = new ArrayBuffer[VaryingDeclaraion]
    val declarations = new ReadSeq(_declarations)
  }
  
  sealed trait ShaderInterface
  
  final class Function(val signature: String)(val src: String) extends ShaderInterface {
    checkState()
    _functions += this
  }
  
  
  protected final def uses(functionSignature: String) {
    checkState()
    _functionDependencies += functionSignature
  }
  
  protected final def uniform[B <: TechniqueBinding : ClassManifest](name: String) {
    checkState()
    _uniformsDependencies += new UniformDeclaraion(implicitly[ClassManifest[B]], name)
  }
  
  protected val interface: ShaderInterface
  
  
  private var _functionDependencies = new ArrayBuffer[String]
  private var _uniformsDependencies = new ArrayBuffer[UniformDeclaraion]
  private var _functions = new ArrayBuffer[Function]
  
  private[pluggable] var isRegistered = false
  private[pluggable] def checkState() {
    if (isRegistered) throw new IllegalStateException("Modifying shader after it has been registered.")
  }
  
  
  val functionDependencies = new ReadSeq(_functionDependencies)
  val uniformsDependencies = new ReadSeq(_uniformsDependencies)
  val functions = new ReadSeq(_functions)
}


abstract class VertexShader extends Shader {
  protected final def attribute[B <: VectorLike : ClassManifest](qualifiers: String, name: String) {
    checkState()
    _attributeDependencies += new AttributeDeclaraion(implicitly[ClassManifest[B]], qualifiers, name)
  }
  protected final def attribute[B <: VectorLike : ClassManifest](name: String) {
    attribute("", name)
  }
  
  protected abstract class OutputBlock(name: String) extends VaryingBlock(
    name, new ArrayBuffer[VaryingBlock]
  ) with ShaderInterface
  
  
  private var _attributeDependencies = new ArrayBuffer[AttributeDeclaraion]
  val attributeDependencies = new ReadSeq(_attributeDependencies)
}


abstract class FragmentShader extends Shader {
  protected abstract class InputBlock(name: String) extends VaryingBlock(name, inputRegistry)
  
  private val inputRegistry = new ArrayBuffer[VaryingBlock]
  val inputBlocks = new ReadSeq(inputRegistry)
}
