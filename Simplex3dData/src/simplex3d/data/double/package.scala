/*
 * Simplex3d, DoubleData module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data

import java.nio._
import scala.reflect._
import simplex3d.math.doublex._
import simplex3d.data.doublem._
import simplex3d.data.doublem.impl._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object doublem {

  private[this] final def primitiveFactory[R <: DefinedDouble](f: PrimitiveFactory[RDouble, R]) = f
  private[this] final def factory[E <: Meta](f: CompositionFactory[E, DefinedDouble]) = f
  private[this] final val default = new ArrayRDoubleRFloat

  // RDouble
  implicit final val FactoryRDoubleSByte = primitiveFactory[SByte](new ArrayRDoubleSByte)
  implicit final val FactoryRDoubleUByte = primitiveFactory[UByte](new ArrayRDoubleUByte)
  implicit final val FactoryRDoubleSShort = primitiveFactory[SShort](new ArrayRDoubleSShort)
  implicit final val FactoryRDoubleUShort = primitiveFactory[UShort](new ArrayRDoubleUShort)
  implicit final val FactoryRDoubleSInt = primitiveFactory[SInt](new ArrayRDoubleSInt)
  implicit final val FactoryRDoubleUInt = primitiveFactory[UInt](new ArrayRDoubleUInt)
  implicit final val FactoryRDoubleHFloat = primitiveFactory[HFloat](new ArrayRDoubleHFloat)
  implicit final val FactoryRDoubleRFloat = primitiveFactory[RFloat](default)
  implicit final val FactoryRDoubleRDouble = primitiveFactory[RDouble](new ArrayRDoubleRDouble)

  // Composition
  implicit final val FactoryRDouble = factory[RDouble](default)
  implicit final val FactoryVec2d = factory[Vec2d](new ArrayVec2d(default))
  implicit final val FactoryVec3d = factory[Vec3d](new ArrayVec3d(default))
  implicit final val FactoryVec4d = factory[Vec4d](new ArrayVec4d(default))


   private[this] final val matrixBound = Manifest.intersectionType[DefinedFloat with SystemFloatingPoint](
    Manifest.classType(classOf[DefinedFloat]),
    Manifest.classType(classOf[SystemFloatingPoint])
  )

  implicit object FactoryMat2x2d extends DataAdapter[Mat2x2d, DefinedFloat with SystemFloatingPoint](components = 4)(
    elemManifest = Mat2x2d.Manifest,
    readManifest = Mat2x2d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat2x2d#Component, Raw], j: Int) :Mat2x2d#Const = {
      Mat2x2d(
        backing(j),     backing(j + 1),
        backing(j + 2), backing(j + 3)
      )
    }
    def update(backing: outContiguous[Mat2x2d#Component, Raw], j: Int, value: Mat2x2d#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10
      backing(j + 2) = value.m01; backing(j + 3) = value.m11
    }
  }

  implicit object FactoryMat2x3d extends DataAdapter[Mat2x3d, DefinedFloat with SystemFloatingPoint](components = 6)(
    elemManifest = Mat2x3d.Manifest,
    readManifest = Mat2x3d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat2x3d#Component, Raw], j: Int) :Mat2x3d#Const = {
      Mat2x3d(
        backing(j),     backing(j + 1),
        backing(j + 2), backing(j + 3),
        backing(j + 4), backing(j + 5)
      )
    }
    def update(backing: outContiguous[Mat2x3d#Component, Raw], j: Int, value: Mat2x3d#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10
      backing(j + 2) = value.m01; backing(j + 3) = value.m11
      backing(j + 4) = value.m02; backing(j + 5) = value.m12
    }
  }

  implicit object FactoryMat2x4d extends DataAdapter[Mat2x4d, DefinedFloat with SystemFloatingPoint](components = 8)(
    elemManifest = Mat2x4d.Manifest,
    readManifest = Mat2x4d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat2x4d#Component, Raw], j: Int) :Mat2x4d#Const = {
      Mat2x4d(
        backing(j),     backing(j + 1),
        backing(j + 2), backing(j + 3),
        backing(j + 4), backing(j + 5),
        backing(j + 6), backing(j + 7)
      )
    }
    def update(backing: outContiguous[Mat2x4d#Component, Raw], j: Int, value: Mat2x4d#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10
      backing(j + 2) = value.m01; backing(j + 3) = value.m11
      backing(j + 4) = value.m02; backing(j + 5) = value.m12
      backing(j + 6) = value.m03; backing(j + 7) = value.m13
    }
  }

  implicit object FactoryMat3x2d extends DataAdapter[Mat3x2d, DefinedFloat with SystemFloatingPoint](components = 6)(
    elemManifest = Mat3x2d.Manifest,
    readManifest = Mat3x2d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat3x2d#Component, Raw], j: Int) :Mat3x2d#Const = {
      Mat3x2d(
        backing(j),     backing(j + 1), backing(j + 2),
        backing(j + 3), backing(j + 4), backing(j + 5)
      )
    }
    def update(backing: outContiguous[Mat3x2d#Component, Raw], j: Int, value: Mat3x2d#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10; backing(j + 2) = value.m20
      backing(j + 3) = value.m01; backing(j + 4) = value.m11; backing(j + 5) = value.m21
    }
  }

  implicit object FactoryMat3x3d extends DataAdapter[Mat3x3d, DefinedFloat with SystemFloatingPoint](components = 9)(
    elemManifest = Mat3x3d.Manifest,
    readManifest = Mat3x3d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat3x3d#Component, Raw], j: Int) :Mat3x3d#Const = {
      Mat3x3d(
        backing(j),     backing(j + 1), backing(j + 2),
        backing(j + 3), backing(j + 4), backing(j + 5),
        backing(j + 6), backing(j + 7), backing(j + 8)
      )
    }
    def update(backing: outContiguous[Mat3x3d#Component, Raw], j: Int, value: Mat3x3d#Read) {
      backing(j) =     value.m00; backing(j + 1) = value.m10; backing(j + 2) = value.m20
      backing(j + 3) = value.m01; backing(j + 4) = value.m11; backing(j + 5) = value.m21
      backing(j + 6) = value.m02; backing(j + 7) = value.m12; backing(j + 8) = value.m22
    }
  }

  implicit object FactoryMat3x4d extends DataAdapter[Mat3x4d, DefinedFloat with SystemFloatingPoint](components = 12)(
    elemManifest = Mat3x4d.Manifest,
    readManifest = Mat3x4d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat3x4d#Component, Raw], j: Int) :Mat3x4d#Const = {
      Mat3x4d(
        backing(j),     backing(j + 1),  backing(j + 2),
        backing(j + 3), backing(j + 4),  backing(j + 5),
        backing(j + 6), backing(j + 7),  backing(j + 8),
        backing(j + 9), backing(j + 10), backing(j + 11)
      )
    }
    def update(backing: outContiguous[Mat3x4d#Component, Raw], j: Int, value: Mat3x4d#Read) {
      backing(j) =     value.m00; backing(j + 1) =  value.m10; backing(j + 2) =  value.m20
      backing(j + 3) = value.m01; backing(j + 4) =  value.m11; backing(j + 5) =  value.m21
      backing(j + 6) = value.m02; backing(j + 7) =  value.m12; backing(j + 8) =  value.m22
      backing(j + 9) = value.m03; backing(j + 10) = value.m13; backing(j + 11) = value.m23
    }
  }

  implicit object FactoryMat4x2d extends DataAdapter[Mat4x2d, DefinedFloat with SystemFloatingPoint](components = 8)(
    elemManifest = Mat4x2d.Manifest,
    readManifest = Mat4x2d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat4x2d#Component, Raw], j: Int) :Mat4x2d#Const = {
      Mat4x2d(
        backing(j),     backing(j + 1), backing(j + 2), backing(j + 3),
        backing(j + 4), backing(j + 5), backing(j + 6), backing(j + 7)
      )
    }
    def update(backing: outContiguous[Mat4x2d#Component, Raw], j: Int, value: Mat4x2d#Read) {
      backing(j) = value.m00;     backing(j + 1) = value.m10; backing(j + 2) = value.m20; backing(j + 3) = value.m30
      backing(j + 4) = value.m01; backing(j + 5) = value.m11; backing(j + 6) = value.m21; backing(j + 7) = value.m31
    }
  }

  implicit object FactoryMat4x3d extends DataAdapter[Mat4x3d, DefinedFloat with SystemFloatingPoint](components = 12)(
    elemManifest = Mat4x3d.Manifest,
    readManifest = Mat4x3d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat4x3d#Component, Raw], j: Int) :Mat4x3d#Const = {
      Mat4x3d(
        backing(j),     backing(j + 1), backing(j + 2),  backing(j + 3),
        backing(j + 4), backing(j + 5), backing(j + 6),  backing(j + 7),
        backing(j + 8), backing(j + 9), backing(j + 10), backing(j + 11)
      )
    }
    def update(backing: outContiguous[Mat4x3d#Component, Raw], j: Int, value: Mat4x3d#Read) {
      backing(j) = value.m00;     backing(j + 1) = value.m10; backing(j + 2) =  value.m20; backing(j + 3) =  value.m30
      backing(j + 4) = value.m01; backing(j + 5) = value.m11; backing(j + 6) =  value.m21; backing(j + 7) =  value.m31
      backing(j + 8) = value.m02; backing(j + 9) = value.m12; backing(j + 10) = value.m22; backing(j + 11) = value.m32
    }
  }

  implicit object FactoryMat4x4d extends DataAdapter[Mat4x4d, DefinedFloat with SystemFloatingPoint](components = 16)(
    elemManifest = Mat4x4d.Manifest,
    readManifest = Mat4x4d.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(backing: inContiguous[Mat4x4d#Component, Raw], j: Int) :Mat4x4d#Const = {
      Mat4x4d(
        backing(j),      backing(j + 1),  backing(j + 2),  backing(j + 3),
        backing(j + 4),  backing(j + 5),  backing(j + 6),  backing(j + 7),
        backing(j + 8),  backing(j + 9),  backing(j + 10), backing(j + 11),
        backing(j + 12), backing(j + 13), backing(j + 14), backing(j + 15)
      )
    }
    def update(backing: outContiguous[Mat4x4d#Component, Raw], j: Int, value: Mat4x4d#Read) {
      backing(j) =      value.m00; backing(j + 1) =  value.m10; backing(j + 2) =  value.m20; backing(j + 3) =  value.m30
      backing(j + 4) =  value.m01; backing(j + 5) =  value.m11; backing(j + 6) =  value.m21; backing(j + 7) =  value.m31
      backing(j + 8) =  value.m02; backing(j + 9) =  value.m12; backing(j + 10) = value.m22; backing(j + 11) = value.m32
      backing(j + 12) = value.m03; backing(j + 13) = value.m13; backing(j + 14) = value.m23; backing(j + 15) = value.m33
    }
  }
}
