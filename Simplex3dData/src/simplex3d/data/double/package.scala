/*
 * Simplex3d, DoubleData module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
import simplex3d.data.double._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object double {

  private[this] final def primitiveFactory[R <: DefinedDouble](f: PrimitiveFactory[RDouble, R]) = f
  private[this] final def factory[F <: Format](f: CompositionFactory[F, DefinedDouble]) = f
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


   private[this] final val sysfpBound = Manifest.intersectionType[DefinedDouble with SysFP](
    Manifest.classType(classOf[DefinedDouble]),
    Manifest.classType(classOf[SysFP])
  )

  implicit object FactoryQuat4d extends DataAdapter[Quat4d, DefinedDouble with SysFP](components = 4)(
    formatManifest = Quat4d.Manifest,
    accessorManifest = Quat4d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Quat4d#Component, Raw], j: Int) :Quat4d#Const = {
      ConstQuat4d(p(j), p(j + 1), p(j + 2), p(j + 3))
    }
    def update(p: Contiguous[Quat4d#Component, Raw], j: Int, v: Quat4d#Read) {
      p(j) = v.a; p(j + 1) = v.b; p(j + 2) = v.c; p(j + 3) = v.d
    }
  }

  implicit object FactoryMat2x2d extends DataAdapter[Mat2x2d, DefinedDouble with SysFP](components = 4)(
    formatManifest = Mat2x2d.Manifest,
    accessorManifest = Mat2x2d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat2x2d#Component, Raw], j: Int) :Mat2x2d#Const = {
      ConstMat2x2d(
        p(j),     p(j + 1),
        p(j + 2), p(j + 3)
      )
    }
    def update(p: Contiguous[Mat2x2d#Component, Raw], j: Int, v: Mat2x2d#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10
      p(j + 2) = v.m01; p(j + 3) = v.m11
    }
  }

  implicit object FactoryMat2x3d extends DataAdapter[Mat2x3d, DefinedDouble with SysFP](components = 6)(
    formatManifest = Mat2x3d.Manifest,
    accessorManifest = Mat2x3d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat2x3d#Component, Raw], j: Int) :Mat2x3d#Const = {
      ConstMat2x3d(
        p(j),     p(j + 1),
        p(j + 2), p(j + 3),
        p(j + 4), p(j + 5)
      )
    }
    def update(p: Contiguous[Mat2x3d#Component, Raw], j: Int, v: Mat2x3d#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10
      p(j + 2) = v.m01; p(j + 3) = v.m11
      p(j + 4) = v.m02; p(j + 5) = v.m12
    }
  }

  implicit object FactoryMat2x4d extends DataAdapter[Mat2x4d, DefinedDouble with SysFP](components = 8)(
    formatManifest = Mat2x4d.Manifest,
    accessorManifest = Mat2x4d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat2x4d#Component, Raw], j: Int) :Mat2x4d#Const = {
      ConstMat2x4d(
        p(j),     p(j + 1),
        p(j + 2), p(j + 3),
        p(j + 4), p(j + 5),
        p(j + 6), p(j + 7)
      )
    }
    def update(p: Contiguous[Mat2x4d#Component, Raw], j: Int, v: Mat2x4d#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10
      p(j + 2) = v.m01; p(j + 3) = v.m11
      p(j + 4) = v.m02; p(j + 5) = v.m12
      p(j + 6) = v.m03; p(j + 7) = v.m13
    }
  }

  implicit object FactoryMat3x2d extends DataAdapter[Mat3x2d, DefinedDouble with SysFP](components = 6)(
    formatManifest = Mat3x2d.Manifest,
    accessorManifest = Mat3x2d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat3x2d#Component, Raw], j: Int) :Mat3x2d#Const = {
      ConstMat3x2d(
        p(j),     p(j + 1), p(j + 2),
        p(j + 3), p(j + 4), p(j + 5)
      )
    }
    def update(p: Contiguous[Mat3x2d#Component, Raw], j: Int, v: Mat3x2d#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10; p(j + 2) = v.m20
      p(j + 3) = v.m01; p(j + 4) = v.m11; p(j + 5) = v.m21
    }
  }

  implicit object FactoryMat3x3d extends DataAdapter[Mat3x3d, DefinedDouble with SysFP](components = 9)(
    formatManifest = Mat3x3d.Manifest,
    accessorManifest = Mat3x3d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat3x3d#Component, Raw], j: Int) :Mat3x3d#Const = {
      ConstMat3x3d(
        p(j),     p(j + 1), p(j + 2),
        p(j + 3), p(j + 4), p(j + 5),
        p(j + 6), p(j + 7), p(j + 8)
      )
    }
    def update(p: Contiguous[Mat3x3d#Component, Raw], j: Int, v: Mat3x3d#Read) {
      p(j) =     v.m00; p(j + 1) = v.m10; p(j + 2) = v.m20
      p(j + 3) = v.m01; p(j + 4) = v.m11; p(j + 5) = v.m21
      p(j + 6) = v.m02; p(j + 7) = v.m12; p(j + 8) = v.m22
    }
  }

  implicit object FactoryMat3x4d extends DataAdapter[Mat3x4d, DefinedDouble with SysFP](components = 12)(
    formatManifest = Mat3x4d.Manifest,
    accessorManifest = Mat3x4d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat3x4d#Component, Raw], j: Int) :Mat3x4d#Const = {
      ConstMat3x4d(
        p(j),     p(j + 1),  p(j + 2),
        p(j + 3), p(j + 4),  p(j + 5),
        p(j + 6), p(j + 7),  p(j + 8),
        p(j + 9), p(j + 10), p(j + 11)
      )
    }
    def update(p: Contiguous[Mat3x4d#Component, Raw], j: Int, v: Mat3x4d#Read) {
      p(j) =     v.m00; p(j + 1) =  v.m10; p(j + 2) =  v.m20
      p(j + 3) = v.m01; p(j + 4) =  v.m11; p(j + 5) =  v.m21
      p(j + 6) = v.m02; p(j + 7) =  v.m12; p(j + 8) =  v.m22
      p(j + 9) = v.m03; p(j + 10) = v.m13; p(j + 11) = v.m23
    }
  }

  implicit object FactoryMat4x2d extends DataAdapter[Mat4x2d, DefinedDouble with SysFP](components = 8)(
    formatManifest = Mat4x2d.Manifest,
    accessorManifest = Mat4x2d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat4x2d#Component, Raw], j: Int) :Mat4x2d#Const = {
      ConstMat4x2d(
        p(j),     p(j + 1), p(j + 2), p(j + 3),
        p(j + 4), p(j + 5), p(j + 6), p(j + 7)
      )
    }
    def update(p: Contiguous[Mat4x2d#Component, Raw], j: Int, v: Mat4x2d#Read) {
      p(j) = v.m00;     p(j + 1) = v.m10; p(j + 2) = v.m20; p(j + 3) = v.m30
      p(j + 4) = v.m01; p(j + 5) = v.m11; p(j + 6) = v.m21; p(j + 7) = v.m31
    }
  }

  implicit object FactoryMat4x3d extends DataAdapter[Mat4x3d, DefinedDouble with SysFP](components = 12)(
    formatManifest = Mat4x3d.Manifest,
    accessorManifest = Mat4x3d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat4x3d#Component, Raw], j: Int) :Mat4x3d#Const = {
      ConstMat4x3d(
        p(j),     p(j + 1), p(j + 2),  p(j + 3),
        p(j + 4), p(j + 5), p(j + 6),  p(j + 7),
        p(j + 8), p(j + 9), p(j + 10), p(j + 11)
      )
    }
    def update(p: Contiguous[Mat4x3d#Component, Raw], j: Int, v: Mat4x3d#Read) {
      p(j) = v.m00;     p(j + 1) = v.m10; p(j + 2) =  v.m20; p(j + 3) =  v.m30
      p(j + 4) = v.m01; p(j + 5) = v.m11; p(j + 6) =  v.m21; p(j + 7) =  v.m31
      p(j + 8) = v.m02; p(j + 9) = v.m12; p(j + 10) = v.m22; p(j + 11) = v.m32
    }
  }

  implicit object FactoryMat4x4d extends DataAdapter[Mat4x4d, DefinedDouble with SysFP](components = 16)(
    formatManifest = Mat4x4d.Manifest,
    accessorManifest = Mat4x4d.Manifest,
    boundManifest = sysfpBound
  ) {
    def apply(p: inContiguous[Mat4x4d#Component, Raw], j: Int) :Mat4x4d#Const = {
      ConstMat4x4d(
        p(j),      p(j + 1),  p(j + 2),  p(j + 3),
        p(j + 4),  p(j + 5),  p(j + 6),  p(j + 7),
        p(j + 8),  p(j + 9),  p(j + 10), p(j + 11),
        p(j + 12), p(j + 13), p(j + 14), p(j + 15)
      )
    }
    def update(p: Contiguous[Mat4x4d#Component, Raw], j: Int, v: Mat4x4d#Read) {
      p(j) =      v.m00; p(j + 1) =  v.m10; p(j + 2) =  v.m20; p(j + 3) =  v.m30
      p(j + 4) =  v.m01; p(j + 5) =  v.m11; p(j + 6) =  v.m21; p(j + 7) =  v.m31
      p(j + 8) =  v.m02; p(j + 9) =  v.m12; p(j + 10) = v.m22; p(j + 11) = v.m32
      p(j + 12) = v.m03; p(j + 13) = v.m13; p(j + 14) = v.m23; p(j + 15) = v.m33
    }
  }
}
