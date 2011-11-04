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
package backend.gl

import scala.annotation._


private[gl] object GlBindingTypes {
  final val Float = 5126
  final val Vec2 = 35664
  final val Vec3 = 35665
  final val Vec4 = 35666
  final val Int = 5124
  final val Vec2i = 35667
  final val Vec3i = 35668
  final val Vec4i = 35669
  final val Boolean = 35670
  final val Vec2b = 35671
  final val Vec3b = 35672
  final val Vec4b = 35673
  final val Mat2x2 = 35674
  final val Mat2x3 = 35685
  final val Mat2x4 = 35686
  final val Mat3x3 = 35675
  final val Mat3x2 = 35687
  final val Mat3x4 = 35688
  final val Mat4x4 = 35676
  final val Mat4x2 = 35689
  final val Mat4x3 = 35690
  final val Texture1d = 35677
  final val Texture2d = 35678
  final val Texture3d = 35679
  final val CubeTexture = 35680
  final val ShadowTexture1d = 35681
  final val ShadowTexture2d = 35682
}

object EngineBindingTypes {
  final val Float = 0
  final val Vec2 = 1
  final val Vec3 = 2
  final val Vec4 = 3
  final val Int = 4
  final val Vec2i = 5
  final val Vec3i = 6
  final val Vec4i = 7
  final val Boolean = 8
  final val Vec2b = 9
  final val Vec3b = 10
  final val Vec4b = 11
  final val Mat2x2 = 12
  final val Mat2x3 = 13
  final val Mat2x4 = 14
  final val Mat3x3 = 15
  final val Mat3x2 = 16
  final val Mat3x4 = 17
  final val Mat4x4 = 18
  final val Mat4x2 = 19
  final val Mat4x3 = 20
  final val Texture1d = 21
  final val Texture2d = 22
  final val Texture3d = 23
  final val CubeTexture = 24
  final val ShadowTexture1d = 25
  final val ShadowTexture2d = 26
  
  def isVector(t: Int) :Boolean = {
    (t: @switch) match {
      case Float | Vec2 | Vec3 | Vec4 | Int | Vec2i | Vec3i | Vec4i | Boolean | Vec2b | Vec3b | Vec4b => true
      case _ => false
    }
  }
  
  def isMatrix(t: Int) :Boolean = {
    (t: @switch) match {
      case Mat2x2 | Mat2x3 | Mat2x4 | Mat3x2 | Mat3x3 | Mat3x4 | Mat4x2 | Mat4x3 | Mat4x4 => true
      case _ => false
    }
  }
  
  def isTexture(t: Int) :Boolean = {
    (t: @switch) match {
      case Texture1d | Texture2d | Texture3d | CubeTexture | ShadowTexture1d | ShadowTexture2d => true
      case _ => false
    }
  }
  
  def fromGlType(t: Int) :Int = {
    (t: @switch) match {
      case GlBindingTypes.Float => Float
      case GlBindingTypes.Vec2 => Vec2
      case GlBindingTypes.Vec3 => Vec3
      case GlBindingTypes.Vec4 => Vec4
      case GlBindingTypes.Int => Int
      case GlBindingTypes.Vec2i => Vec2i
      case GlBindingTypes.Vec3i => Vec3i
      case GlBindingTypes.Vec4i => Vec4i
      case GlBindingTypes.Boolean => Boolean
      case GlBindingTypes.Vec2b => Vec2b
      case GlBindingTypes.Vec3b => Vec3b
      case GlBindingTypes.Vec4b => Vec4b
      case GlBindingTypes.Mat2x2 => Mat2x2
      case GlBindingTypes.Mat2x3 => Mat2x3
      case GlBindingTypes.Mat2x4 => Mat2x4
      case GlBindingTypes.Mat3x2 => Mat3x2
      case GlBindingTypes.Mat3x3 => Mat3x3
      case GlBindingTypes.Mat3x4 => Mat3x4
      case GlBindingTypes.Mat4x2 => Mat4x2
      case GlBindingTypes.Mat4x3 => Mat4x3
      case GlBindingTypes.Mat4x4 => Mat4x4
      case GlBindingTypes.Texture1d => Texture1d
      case GlBindingTypes.Texture2d => Texture2d
      case GlBindingTypes.Texture3d => Texture3d
      case GlBindingTypes.CubeTexture => CubeTexture
      case GlBindingTypes.ShadowTexture1d => ShadowTexture1d
      case GlBindingTypes.ShadowTexture2d => ShadowTexture2d
    }
  }
  
  def toString(t: Int) :String = {
    (t: @switch) match {
      case Float => "Float"
      case Vec2 => "Vec2"
      case Vec3 => "Vec3"
      case Vec4 => "Vec4"
      case Int => "Int"
      case Vec2i => "Vec2i"
      case Vec3i => "Vec3i"
      case Vec4i => "Vec4i"
      case Boolean => "Boolean"
      case Vec2b => "Vec2b"
      case Vec3b => "Vec3b"
      case Vec4b => "Vec4b"
      case Mat2x2 => "Mat2x2"
      case Mat2x3 => "Mat2x3"
      case Mat2x4 => "Mat2x4"
      case Mat3x2 => "Mat3x2"
      case Mat3x3 => "Mat3x3"
      case Mat3x4 => "Mat3x4"
      case Mat4x2 => "Mat4x2"
      case Mat4x3 => "Mat4x3"
      case Mat4x4 => "Mat4x4"
      case Texture1d => "Texture1d"
      case Texture2d => "Texture2d"
      case Texture3d => "Texture3d"
      case CubeTexture => "CubeTexture"
      case ShadowTexture1d => "ShadowTexture1d"
      case ShadowTexture2d => "ShadowTexture2d"
      case _ => "UndefinedEnum_" + t.toString
    }
  }
}
