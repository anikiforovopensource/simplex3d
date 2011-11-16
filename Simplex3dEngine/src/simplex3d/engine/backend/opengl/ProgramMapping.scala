/*
 * Simplex3dEngine - GL Module
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
package backend.opengl

import simplex3d.data._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


private[backend] abstract class ProgramMapping(
  uniformsSeq: Seq[UniformBinding],
  attributesSeq: Seq[AttributeBinding]
) {
  
  final val uniformVectors = new ReadArray(uniformsSeq.filter(b => EngineBindingTypes.isVector(b.dataType)).toArray)
  final val uniformMatrices = new ReadArray(uniformsSeq.filter(b => EngineBindingTypes.isMatrix(b.dataType)).toArray)
  final val uniformTextures = new ReadArray(uniformsSeq.filter(b => EngineBindingTypes.isTexture(b.dataType)).toArray)
  final val attributes = new ReadArray(attributesSeq.toArray)
  
  def bind(meshMapping: MeshMapping) :Unit
  
  
  override def toString() :String = {
    "ProgramMapping(\n" +
    uniformVectors.map("  " + _.toString() + "\n").mkString("") +
    uniformMatrices.map("  " + _.toString() + "\n").mkString("") +
    uniformTextures.map("  " + _.toString() + "\n").mkString("") +
    attributes.map("  " + _.toString() + "\n").mkString("") +
    ")"
  }
}
