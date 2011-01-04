/*
 * Simplex3d, FloatData module
 * Copyright (C) 2010-2011, Simplex3d Team
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
import simplex3d.math.floatx._
import simplex3d.data.float._
import simplex3d.data.float.impl._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object float {

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
    def apply(p: inContiguous[Mat2x2f#Component, Raw], j: Int) :Mat2x2f#Const = {
      Mat2x2f(
        p(j),     p(j + 1),
        p(j + 2), p(j + 3)
      )
    }
    def update(p: outContiguous[Mat2x2f#Component, Raw], j: Int, v: Mat2x2f#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10
      p(j + 2) = v.m01; p(j + 3) = v.m11
    }
  }

  implicit object FactoryMat2x3f extends DataAdapter[Mat2x3f, DefinedFloat with SystemFloatingPoint](components = 6)(
    elemManifest = Mat2x3f.Manifest,
    readManifest = Mat2x3f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(p: inContiguous[Mat2x3f#Component, Raw], j: Int) :Mat2x3f#Const = {
      Mat2x3f(
        p(j),     p(j + 1),
        p(j + 2), p(j + 3),
        p(j + 4), p(j + 5)
      )
    }
    def update(p: outContiguous[Mat2x3f#Component, Raw], j: Int, v: Mat2x3f#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10
      p(j + 2) = v.m01; p(j + 3) = v.m11
      p(j + 4) = v.m02; p(j + 5) = v.m12
    }
  }

  implicit object FactoryMat2x4f extends DataAdapter[Mat2x4f, DefinedFloat with SystemFloatingPoint](components = 8)(
    elemManifest = Mat2x4f.Manifest,
    readManifest = Mat2x4f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(p: inContiguous[Mat2x4f#Component, Raw], j: Int) :Mat2x4f#Const = {
      Mat2x4f(
        p(j),     p(j + 1),
        p(j + 2), p(j + 3),
        p(j + 4), p(j + 5),
        p(j + 6), p(j + 7)
      )
    }
    def update(p: outContiguous[Mat2x4f#Component, Raw], j: Int, v: Mat2x4f#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10
      p(j + 2) = v.m01; p(j + 3) = v.m11
      p(j + 4) = v.m02; p(j + 5) = v.m12
      p(j + 6) = v.m03; p(j + 7) = v.m13
    }
  }

  implicit object FactoryMat3x2f extends DataAdapter[Mat3x2f, DefinedFloat with SystemFloatingPoint](components = 6)(
    elemManifest = Mat3x2f.Manifest,
    readManifest = Mat3x2f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(p: inContiguous[Mat3x2f#Component, Raw], j: Int) :Mat3x2f#Const = {
      Mat3x2f(
        p(j),     p(j + 1), p(j + 2),
        p(j + 3), p(j + 4), p(j + 5)
      )
    }
    def update(p: outContiguous[Mat3x2f#Component, Raw], j: Int, v: Mat3x2f#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10; p(j + 2) = v.m20
      p(j + 3) = v.m01; p(j + 4) = v.m11; p(j + 5) = v.m21
    }
  }

  implicit object FactoryMat3x3f extends DataAdapter[Mat3x3f, DefinedFloat with SystemFloatingPoint](components = 9)(
    elemManifest = Mat3x3f.Manifest,
    readManifest = Mat3x3f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(p: inContiguous[Mat3x3f#Component, Raw], j: Int) :Mat3x3f#Const = {
      Mat3x3f(
        p(j),     p(j + 1), p(j + 2),
        p(j + 3), p(j + 4), p(j + 5),
        p(j + 6), p(j + 7), p(j + 8)
      )
    }
    def update(p: outContiguous[Mat3x3f#Component, Raw], j: Int, v: Mat3x3f#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10; p(j + 2) = v.m20
      p(j + 3) = v.m01; p(j + 4) = v.m11; p(j + 5) = v.m21
      p(j + 6) = v.m02; p(j + 7) = v.m12; p(j + 8) = v.m22
    }
  }

  implicit object FactoryMat3x4f extends DataAdapter[Mat3x4f, DefinedFloat with SystemFloatingPoint](components = 12)(
    elemManifest = Mat3x4f.Manifest,
    readManifest = Mat3x4f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(p: inContiguous[Mat3x4f#Component, Raw], j: Int) :Mat3x4f#Const = {
      Mat3x4f(
        p(j),     p(j + 1),  p(j + 2),
        p(j + 3), p(j + 4),  p(j + 5),
        p(j + 6), p(j + 7),  p(j + 8),
        p(j + 9), p(j + 10), p(j + 11)
      )
    }
    def update(p: outContiguous[Mat3x4f#Component, Raw], j: Int, v: Mat3x4f#Read) {
      p(j) =     v.m00; p(j + 1) =  v.m10; p(j + 2) =  v.m20
      p(j + 3) = v.m01; p(j + 4) =  v.m11; p(j + 5) =  v.m21
      p(j + 6) = v.m02; p(j + 7) =  v.m12; p(j + 8) =  v.m22
      p(j + 9) = v.m03; p(j + 10) = v.m13; p(j + 11) = v.m23
    }
  }

  implicit object FactoryMat4x2f extends DataAdapter[Mat4x2f, DefinedFloat with SystemFloatingPoint](components = 8)(
    elemManifest = Mat4x2f.Manifest,
    readManifest = Mat4x2f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(p: inContiguous[Mat4x2f#Component, Raw], j: Int) :Mat4x2f#Const = {
      Mat4x2f(
        p(j),     p(j + 1), p(j + 2), p(j + 3),
        p(j + 4), p(j + 5), p(j + 6), p(j + 7)
      )
    }
    def update(p: outContiguous[Mat4x2f#Component, Raw], j: Int, v: Mat4x2f#Read) {
      p(j) = v.m00;     p(j + 1) = v.m10; p(j + 2) = v.m20; p(j + 3) = v.m30
      p(j + 4) = v.m01; p(j + 5) = v.m11; p(j + 6) = v.m21; p(j + 7) = v.m31
    }
  }

  implicit object FactoryMat4x3f extends DataAdapter[Mat4x3f, DefinedFloat with SystemFloatingPoint](components = 12)(
    elemManifest = Mat4x3f.Manifest,
    readManifest = Mat4x3f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(p: inContiguous[Mat4x3f#Component, Raw], j: Int) :Mat4x3f#Const = {
      Mat4x3f(
        p(j),     p(j + 1), p(j + 2),  p(j + 3),
        p(j + 4), p(j + 5), p(j + 6),  p(j + 7),
        p(j + 8), p(j + 9), p(j + 10), p(j + 11)
      )
    }
    def update(p: outContiguous[Mat4x3f#Component, Raw], j: Int, v: Mat4x3f#Read) {
      p(j) = v.m00;     p(j + 1) = v.m10; p(j + 2) =  v.m20; p(j + 3) =  v.m30
      p(j + 4) = v.m01; p(j + 5) = v.m11; p(j + 6) =  v.m21; p(j + 7) =  v.m31
      p(j + 8) = v.m02; p(j + 9) = v.m12; p(j + 10) = v.m22; p(j + 11) = v.m32
    }
  }

  implicit object FactoryMat4x4f extends DataAdapter[Mat4x4f, DefinedFloat with SystemFloatingPoint](components = 16)(
    elemManifest = Mat4x4f.Manifest,
    readManifest = Mat4x4f.ReadManifest,
    boundManifest = matrixBound
  ) {
    def apply(p: inContiguous[Mat4x4f#Component, Raw], j: Int) :Mat4x4f#Const = {
      Mat4x4f(
        p(j),      p(j + 1),  p(j + 2),  p(j + 3),
        p(j + 4),  p(j + 5),  p(j + 6),  p(j + 7),
        p(j + 8),  p(j + 9),  p(j + 10), p(j + 11),
        p(j + 12), p(j + 13), p(j + 14), p(j + 15)
      )
    }
    def update(p: outContiguous[Mat4x4f#Component, Raw], j: Int, v: Mat4x4f#Read) {
      p(j) =      v.m00; p(j + 1) =  v.m10; p(j + 2) =  v.m20; p(j + 3) =  v.m30
      p(j + 4) =  v.m01; p(j + 5) =  v.m11; p(j + 6) =  v.m21; p(j + 7) =  v.m31
      p(j + 8) =  v.m02; p(j + 9) =  v.m12; p(j + 10) = v.m22; p(j + 11) = v.m32
      p(j + 12) = v.m03; p(j + 13) = v.m13; p(j + 14) = v.m23; p(j + 15) = v.m33
    }
  }
}
