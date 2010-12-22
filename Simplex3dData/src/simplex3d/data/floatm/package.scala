/*
 * Simplex3d, FloatBuffer module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer

import java.nio._
import scala.reflect._
import simplex3d.math.floatm._
import simplex3d.buffer.floatm._
import simplex3d.buffer.floatm.impl._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

  private[this] final def primitiveFactory[R <: DefinedFloat](f: PrimitiveFactory[RFloat, R]) = f
  private[this] final def factory[E <: Meta](f: CompositionFactory[E, DefinedFloat]) = f
  private[this] final val default = new ArrayRFloatRFloat

  // RFloat
  implicit final val FactoryRFloatSByte = primitiveFactory[SByte](new ArrayRFloatSByte)
  implicit final val FactoryRFloatUByte = primitiveFactory[UByte](new ArrayRFloatUByte)
  implicit final val FactoryRFloatSShort = primitiveFactory[SShort](new ArrayRFloatSShort)
  implicit final val FactoryRFloatUShort = primitiveFactory[UShort](new ArrayRFloatUShort)
  implicit final val FactoryRFloatSInt = primitiveFactory[SInt](new ArrayRFloatSInt)
  implicit final val FactoryRFloatUInt = primitiveFactory[UInt](new ArrayRFloatUInt)
  implicit final val FactoryRFloatHFloat = primitiveFactory[HFloat](new ArrayRFloatHFloat)
  implicit final val FactoryRFloatRFloat = primitiveFactory[RFloat](default)

  // Composition
  implicit final val FactoryRFloat = factory[RFloat](default)
  implicit final val FactoryVec2f = factory[Vec2f](new ArrayVec2f(default))
  implicit final val FactoryVec3f = factory[Vec3f](new ArrayVec3f(default))
  implicit final val FactoryVec4f = factory[Vec4f](new ArrayVec4f(default))


  private[this] final val matrixBound = Manifest.intersectionType[DefinedFloat with SystemFloatingPoint](
    Manifest.classType(classOf[DefinedFloat]),
    Manifest.classType(classOf[SystemFloatingPoint])
  )

  implicit object FactoryMat2x2f extends DataAdapter[Mat2x2f, DefinedFloat with SystemFloatingPoint](components = 4)(
    elemManifest = Mat2x2f.Manifest,
    readManifest = Mat2x2f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat2x2f#Component, Raw], j: Int) :Mat2x2f#Const = {
      Mat2x2f(
        backing(j),     backing(j + 1),
        backing(j + 2), backing(j + 3)
      )
    }
    def update(backing: outContiguous[Mat2x2f#Component, Raw], j: Int, value: Mat2x2f#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10
      backing(j + 2) = value.m01; backing(j + 3) = value.m11
    }
  }

  implicit object FactoryMat2x3f extends DataAdapter[Mat2x3f, DefinedFloat with SystemFloatingPoint](components = 6)(
    elemManifest = Mat2x3f.Manifest,
    readManifest = Mat2x3f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat2x3f#Component, Raw], j: Int) :Mat2x3f#Const = {
      Mat2x3f(
        backing(j),     backing(j + 1),
        backing(j + 2), backing(j + 3),
        backing(j + 4), backing(j + 5)
      )
    }
    def update(backing: outContiguous[Mat2x3f#Component, Raw], j: Int, value: Mat2x3f#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10
      backing(j + 2) = value.m01; backing(j + 3) = value.m11
      backing(j + 4) = value.m02; backing(j + 5) = value.m12
    }
  }

  implicit object FactoryMat2x4f extends DataAdapter[Mat2x4f, DefinedFloat with SystemFloatingPoint](components = 8)(
    elemManifest = Mat2x4f.Manifest,
    readManifest = Mat2x4f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat2x4f#Component, Raw], j: Int) :Mat2x4f#Const = {
      Mat2x4f(
        backing(j),     backing(j + 1),
        backing(j + 2), backing(j + 3),
        backing(j + 4), backing(j + 5),
        backing(j + 6), backing(j + 7)
      )
    }
    def update(backing: outContiguous[Mat2x4f#Component, Raw], j: Int, value: Mat2x4f#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10
      backing(j + 2) = value.m01; backing(j + 3) = value.m11
      backing(j + 4) = value.m02; backing(j + 5) = value.m12
      backing(j + 6) = value.m03; backing(j + 7) = value.m13
    }
  }

  implicit object FactoryMat3x2f extends DataAdapter[Mat3x2f, DefinedFloat with SystemFloatingPoint](components = 6)(
    elemManifest = Mat3x2f.Manifest,
    readManifest = Mat3x2f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat3x2f#Component, Raw], j: Int) :Mat3x2f#Const = {
      Mat3x2f(
        backing(j),     backing(j + 1), backing(j + 2),
        backing(j + 3), backing(j + 4), backing(j + 5)
      )
    }
    def update(backing: outContiguous[Mat3x2f#Component, Raw], j: Int, value: Mat3x2f#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10; backing(j + 2) = value.m20
      backing(j + 3) = value.m01; backing(j + 4) = value.m11; backing(j + 5) = value.m21
    }
  }

  implicit object FactoryMat3x3f extends DataAdapter[Mat3x3f, DefinedFloat with SystemFloatingPoint](components = 9)(
    elemManifest = Mat3x3f.Manifest,
    readManifest = Mat3x3f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat3x3f#Component, Raw], j: Int) :Mat3x3f#Const = {
      Mat3x3f(
        backing(j),     backing(j + 1), backing(j + 2),
        backing(j + 3), backing(j + 4), backing(j + 5),
        backing(j + 6), backing(j + 7), backing(j + 8)
      )
    }
    def update(backing: outContiguous[Mat3x3f#Component, Raw], j: Int, value: Mat3x3f#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10; backing(j + 2) = value.m20
      backing(j + 3) = value.m01; backing(j + 4) = value.m11; backing(j + 5) = value.m21
      backing(j + 6) = value.m02; backing(j + 7) = value.m12; backing(j + 8) = value.m22
    }
  }

  implicit object FactoryMat3x4f extends DataAdapter[Mat3x4f, DefinedFloat with SystemFloatingPoint](components = 12)(
    elemManifest = Mat3x4f.Manifest,
    readManifest = Mat3x4f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat3x4f#Component, Raw], j: Int) :Mat3x4f#Const = {
      Mat3x4f(
        backing(j),     backing(j + 1),  backing(j + 2),
        backing(j + 3), backing(j + 4),  backing(j + 5),
        backing(j + 6), backing(j + 7),  backing(j + 8),
        backing(j + 9), backing(j + 10), backing(j + 11)
      )
    }
    def update(backing: outContiguous[Mat3x4f#Component, Raw], j: Int, value: Mat3x4f#Read) {
      backing(j) =     value.m00; backing(j + 1) =  value.m10; backing(j + 2) =  value.m20
      backing(j + 3) = value.m01; backing(j + 4) =  value.m11; backing(j + 5) =  value.m21
      backing(j + 6) = value.m02; backing(j + 7) =  value.m12; backing(j + 8) =  value.m22
      backing(j + 9) = value.m03; backing(j + 10) = value.m13; backing(j + 11) = value.m23
    }
  }

  implicit object FactoryMat4x2f extends DataAdapter[Mat4x2f, DefinedFloat with SystemFloatingPoint](components = 8)(
    elemManifest = Mat4x2f.Manifest,
    readManifest = Mat4x2f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat4x2f#Component, Raw], j: Int) :Mat4x2f#Const = {
      Mat4x2f(
        backing(j),     backing(j + 1), backing(j + 2), backing(j + 3),
        backing(j + 4), backing(j + 5), backing(j + 6), backing(j + 7)
      )
    }
    def update(backing: outContiguous[Mat4x2f#Component, Raw], j: Int, value: Mat4x2f#Read) {
      backing(j) = value.m00;     backing(j + 1) = value.m10; backing(j + 2) = value.m20; backing(j + 3) = value.m30
      backing(j + 4) = value.m01; backing(j + 5) = value.m11; backing(j + 6) = value.m21; backing(j + 7) = value.m31
    }
  }

  implicit object FactoryMat4x3f extends DataAdapter[Mat4x3f, DefinedFloat with SystemFloatingPoint](components = 12)(
    elemManifest = Mat4x3f.Manifest,
    readManifest = Mat4x3f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat4x3f#Component, Raw], j: Int) :Mat4x3f#Const = {
      Mat4x3f(
        backing(j),     backing(j + 1), backing(j + 2),  backing(j + 3),
        backing(j + 4), backing(j + 5), backing(j + 6),  backing(j + 7),
        backing(j + 8), backing(j + 9), backing(j + 10), backing(j + 11)
      )
    }
    def update(backing: outContiguous[Mat4x3f#Component, Raw], j: Int, value: Mat4x3f#Read) {
      backing(j) = value.m00;     backing(j + 1) = value.m10; backing(j + 2) =  value.m20; backing(j + 3) =  value.m30
      backing(j + 4) = value.m01; backing(j + 5) = value.m11; backing(j + 6) =  value.m21; backing(j + 7) =  value.m31
      backing(j + 8) = value.m02; backing(j + 9) = value.m12; backing(j + 10) = value.m22; backing(j + 11) = value.m32
    }
  }

  implicit object FactoryMat4x4f extends DataAdapter[Mat4x4f, DefinedFloat with SystemFloatingPoint](components = 16)(
    elemManifest = Mat4x4f.Manifest,
    readManifest = Mat4x4f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat4x4f#Component, Raw], j: Int) :Mat4x4f#Const = {
      Mat4x4f(
        backing(j),      backing(j + 1),  backing(j + 2),  backing(j + 3),
        backing(j + 4),  backing(j + 5),  backing(j + 6),  backing(j + 7),
        backing(j + 8),  backing(j + 9),  backing(j + 10), backing(j + 11),
        backing(j + 12), backing(j + 13), backing(j + 14), backing(j + 15)
      )
    }
    def update(backing: outContiguous[Mat4x4f#Component, Raw], j: Int, value: Mat4x4f#Read) {
      backing(j) =      value.m00; backing(j + 1) =  value.m10; backing(j + 2) =  value.m20; backing(j + 3) =  value.m30
      backing(j + 4) =  value.m01; backing(j + 5) =  value.m11; backing(j + 6) =  value.m21; backing(j + 7) =  value.m31
      backing(j + 8) =  value.m02; backing(j + 9) =  value.m12; backing(j + 10) = value.m22; backing(j + 11) = value.m32
      backing(j + 12) = value.m03; backing(j + 13) = value.m13; backing(j + 14) = value.m23; backing(j + 15) = value.m33
    }
  }
}
