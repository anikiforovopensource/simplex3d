/*
 * Simplex3dEngine - LWJGL Module
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
package backend.lwjgl

import scala.annotation._
import org.lwjgl.opengl._
import ArbEquivalents.{ GL15 ,GL20 }
import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.util._
import simplex3d.engine.graphics._
import simplex3d.engine.backend.opengl._


private[backend] final class ProgramMapping(val program: Technique, val context: RenderContext)(
  uniformsSeq: Seq[ActiveUniform],
  attributesSeq: Seq[ActiveAttribute]
) extends backend.opengl.ProgramMapping(uniformsSeq, attributesSeq) {
  
  //println("XXX " + program + "\n\n" + uniformsSeq.mkString("\n") + "\n\n" + attributesSeq.mkString("\n"))
  
  // Fast access.
  private[this] val uniformVectorLocations = uniformVectors.map(_.location).toArray
  private[this] val uniformVectorTypes = uniformVectors.map(_.dataType).toArray
  
  private[this] val uniformMatrixLocations = uniformMatrices.map(_.location).toArray
  private[this] val uniformMatrixTypes = uniformMatrices.map(_.dataType).toArray
  
  private[this] val uniformTextureLocations = uniformTextures.map(_.location).toArray
  private[this] val uniformTextureTypes = uniformTextures.map(_.dataType).toArray
  private[this] val uniformTextureUnits = uniformTextures.map(_.asInstanceOf[ActiveTexture].textureUnit).toArray
  
  private[this] val attributeLocations = attributes.map(_.location).toArray
  private[this] val attributeColumnsRows = attributes.map {
    def zip(columns: Int, rows: Int) :Int = (columns << 4) | rows
    _.dataType match {
        case EngineBindingTypes.Float => zip(1, 1)
        case EngineBindingTypes.Vec2 => zip(1, 2)
        case EngineBindingTypes.Vec3 => zip(1, 3)
        case EngineBindingTypes.Vec4 => zip(1, 4)
        case EngineBindingTypes.Int => zip(1, 1)
        case EngineBindingTypes.Vec2i => zip(1, 2)
        case EngineBindingTypes.Vec3i => zip(1, 3)
        case EngineBindingTypes.Vec4i => zip(1, 4)
        case EngineBindingTypes.Mat2x2 => zip(2, 2)
        case EngineBindingTypes.Mat2x3 => zip(2, 3)
        case EngineBindingTypes.Mat2x4 => zip(2, 4)
        case EngineBindingTypes.Mat3x2 => zip(3, 2)
        case EngineBindingTypes.Mat3x3 => zip(3, 3)
        case EngineBindingTypes.Mat3x4 => zip(3, 4)
        case EngineBindingTypes.Mat4x2 => zip(4, 2)
        case EngineBindingTypes.Mat4x3 => zip(4, 3)
        case EngineBindingTypes.Mat4x4 => zip(4, 4)
    }
  }.toArray
  
  
  private[this] def bindUniformVectors(uniforms: ReadArray[VectorLike]) {
    var i = 0; while (i < uniformVectors.length) {
      setUniformVector(uniformVectorLocations(i), uniformVectorTypes(i), uniforms(i))
      i += 1
    }
  }
  
  private[this] def bindUniformMatrices(uniforms: ReadArray[AnyMat[_]]) {
    var i = 0; while (i < uniformMatrices.length) {
      setUniformMatrix(uniformMatrixLocations(i), uniformMatrixTypes(i), uniforms(i))
      i += 1
    }
  }
  
  private[this] def bindUniformTextures(uniforms: ReadArray[ReadTextureBinding[_]]) {
    var i = 0; while (i < uniformTextures.length) {
      setUniformTexture(
        uniformTextureLocations(i), uniformTextureUnits(i),
        uniformTextureTypes(i), uniforms(i)
      )
      i += 1
    }
  }
  
  private[this] def bindAttributes(
    attributeBindings: ReadArray[ActiveAttribute],
    attributes: ReadArray[Attributes[_, _]]
  ) {
    var i = 0; while (i < attributeBindings.length) {
      val columnsRows = attributeColumnsRows(i)
      val columns = (columnsRows >> 4)
      val rows = (columnsRows & 0x00000007)
      setAttributes(attributeLocations(i), columns, rows, attributes(i))
      
      i += 1
    }
  }
  
  
  def bind(meshMapping: MeshMapping) {
    bindUniformVectors(meshMapping.uniformVectors)
    bindUniformMatrices(meshMapping.uniformMatrices)
    bindUniformTextures(meshMapping.uniformTextures)
    bindAttributes(attributes, meshMapping.attributes)
  }
  
  
  // *** State ********************************************************************************************************
  
  import GL20._; import GL21._;
  
  
  def setAttributes(location: Int, columns: Int, rows: Int, attributes: Attributes[_, _]) {
    if (attributes != null) {
      context.bind(location, columns, rows, attributes.asInstanceOf[Attributes[_ <: Format, Raw]])
    }
  }
  
  def setUniformVector(location: Int, dataType: Int, value: VectorLike) {
    if (value != null) {
      (dataType: @switch) match {
        case EngineBindingTypes.Float => uniform(location, value.asInstanceOf[ReadDoubleRef])
        case EngineBindingTypes.Vec2 => uniform(location, value.asInstanceOf[ReadVec2])
        case EngineBindingTypes.Vec3 => uniform(location, value.asInstanceOf[ReadVec3])
        case EngineBindingTypes.Vec4 => {
          if (value.isInstanceOf[ReadQuat4]) uniform(location, value.asInstanceOf[ReadQuat4])
          else uniform(location, value.asInstanceOf[ReadVec4])
        }
        case EngineBindingTypes.Int => uniform(location, value.asInstanceOf[ReadIntRef])
        case EngineBindingTypes.Vec2i => uniform(location, value.asInstanceOf[ReadVec2i])
        case EngineBindingTypes.Vec3i => uniform(location, value.asInstanceOf[ReadVec3i])
        case EngineBindingTypes.Vec4i => uniform(location, value.asInstanceOf[ReadVec4i])
        case EngineBindingTypes.Boolean => uniform(location, value.asInstanceOf[ReadBooleanRef])
        case EngineBindingTypes.Vec2b => uniform(location, value.asInstanceOf[ReadVec2b])
        case EngineBindingTypes.Vec3b => uniform(location, value.asInstanceOf[ReadVec3b])
        case EngineBindingTypes.Vec4b => uniform(location, value.asInstanceOf[ReadVec4b])
      }
    }
  }
  
  def setUniformMatrix(location: Int, dataType: Int, value: AnyMat[_]) {
    if (value != null) {
      (dataType: @switch) match {
        case EngineBindingTypes.Mat2x2 => uniformMat2(location, value)
        case EngineBindingTypes.Mat2x3 => uniform(location, value.asInstanceOf[ReadMat2x3])
        case EngineBindingTypes.Mat2x4 => uniform(location, value.asInstanceOf[ReadMat2x4])
        case EngineBindingTypes.Mat3x2 => uniform(location, value.asInstanceOf[ReadMat3x2])
        case EngineBindingTypes.Mat3x3 => uniformMat3(location, value)
        case EngineBindingTypes.Mat3x4 => uniform(location, value.asInstanceOf[ReadMat3x4])
        case EngineBindingTypes.Mat4x2 => uniform(location, value.asInstanceOf[ReadMat4x2])
        case EngineBindingTypes.Mat4x3 => uniform(location, value.asInstanceOf[ReadMat4x3])
        case EngineBindingTypes.Mat4x4 => uniformMat4(location, value)
      }
    }
  }
  
  def setUniformTexture(location: Int, textureUnit: Int, dataType: Int, value: ReadTextureBinding[_]) {
    if (value != null) {
      (dataType: @switch) match {
        case EngineBindingTypes.Texture1d => //XXX
        case EngineBindingTypes.Texture2d => sampler2d(location, textureUnit, dataType, value.asInstanceOf[ReadTextureBinding[Texture2d[_]]])
        case EngineBindingTypes.Texture3d =>
        case EngineBindingTypes.CubeTexture =>
        case EngineBindingTypes.ShadowTexture1d =>
        case EngineBindingTypes.ShadowTexture2d =>
      }
    }
  }
  
  
  private def uniform(location: Int, r: ReadDoubleRef) { glUniform1f(location, r.toConst.toFloat) }
  private def uniform(location: Int, u: inVec2) { glUniform2f(location, u.x.toFloat, u.y.toFloat) }
  private def uniform(location: Int, u: inVec3) { glUniform3f(location, u.x.toFloat, u.y.toFloat, u.z.toFloat) }
  private def uniform(location: Int, u: inVec4) { glUniform4f(location, u.x.toFloat, u.y.toFloat, u.z.toFloat, u.w.toFloat) }
  private def uniform(location: Int, q: inQuat4) { glUniform4f(location, q.b.toFloat, q.c.toFloat, q.d.toFloat, q.a.toFloat) }
  
  private def uniform(location: Int, r: ReadIntRef) { glUniform1i(location, r.toConst) }
  private def uniform(location: Int, u: inVec2i) { glUniform2i(location, u.x, u.y) }
  private def uniform(location: Int, u: inVec3i) { glUniform3i(location, u.x, u.y, u.z) }
  private def uniform(location: Int, u: inVec4i) { glUniform4i(location, u.x, u.y, u.z, u.w) }
  
  private def uniform(location: Int, r: ReadBooleanRef) { glUniform1i(location, toInt(r.toConst)) }
  private def uniform(location: Int, u: inVec2b) { glUniform2i(location, toInt(u.x), toInt(u.y)) }
  private def uniform(location: Int, u: inVec3b) { glUniform3i(location, toInt(u.x), toInt(u.y), toInt(u.z)) }
  private def uniform(location: Int, u: inVec4b) { glUniform4i(location, toInt(u.x), toInt(u.y), toInt(u.z), toInt(u.w)) }
  
  
  private def uniformMat2(location: Int, m: AnyMat[_]) { glUniformMatrix2(location, false, context.mat2x2ToBuffer(m)) }
  private def uniform(location: Int, m: inMat2x3) { glUniformMatrix2x3(location, false, context.mat2x3ToBuffer(m)) }
  private def uniform(location: Int, m: inMat2x4) { glUniformMatrix2x4(location, false, context.mat2x4ToBuffer(m)) }
  private def uniform(location: Int, m: inMat3x2) { glUniformMatrix3x2(location, false, context.mat3x2ToBuffer(m)) }
  private def uniformMat3(location: Int, m: AnyMat[_]) { glUniformMatrix3(location, false, context.mat3x3ToBuffer(m)) }
  private def uniform(location: Int, m: inMat3x4) { glUniformMatrix3x4(location, false, context.mat3x4ToBuffer(m)) }
  private def uniform(location: Int, m: inMat4x2) { glUniformMatrix4x2(location, false, context.mat4x2ToBuffer(m)) }
  private def uniform(location: Int, m: inMat4x3) { glUniformMatrix4x3(location, false, context.mat4x3ToBuffer(m)) }
  private def uniformMat4(location: Int, m: AnyMat[_]) { glUniformMatrix4(location, false, context.mat4x4ToBuffer(m)) }
  
  
  private def sampler2d(location: Int, textureUnit: Int, textureType: Int, texture: ReadTextureBinding[Texture2d[_]]) {
    context.bindTex2d(location, textureUnit, texture)
  }
}
