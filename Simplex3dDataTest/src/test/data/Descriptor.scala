/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.data

import scala.reflect._
import simplex3d.data.{MetaManifest => Man, _}
import simplex3d.data.RawType._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._


/**
 * @author Aleksey Nikiforov (lex)
 */
case class Descriptor[E <: Meta, +R <: Raw](
  metaManifest: ClassManifest[_],
  componentManifest: ClassManifest[_],
  readManifest: ClassManifest[_],
  components: Int,
  rawType: Int,
  normalized: Boolean
)

object Descriptors {
  implicit val SIntSByte = Descriptor[SInt, SByte](Man.SInt, Man.SInt, Manifest.Int, 1, SByte, false)
  implicit val SIntUByte = Descriptor[SInt, UByte](Man.SInt, Man.SInt, Manifest.Int, 1, UByte, false)
  implicit val SIntSShort = Descriptor[SInt, SShort](Man.SInt, Man.SInt, Manifest.Int, 1, SShort, false)
  implicit val SIntUShort = Descriptor[SInt, UShort](Man.SInt, Man.SInt, Manifest.Int, 1, UShort, false)
  implicit val SIntSInt = Descriptor[SInt, SInt](Man.SInt, Man.SInt, Manifest.Int, 1, SInt, false)
  implicit val SIntUInt = Descriptor[SInt, UInt](Man.SInt, Man.SInt, Manifest.Int, 1, UInt, false)
  
  implicit val Vec2iSByte = Descriptor[Vec2i, SByte](Vec2i.Manifest, Man.SInt, Vec2i.ReadManifest, 2, SByte, false)
  implicit val Vec2iUByte = Descriptor[Vec2i, UByte](Vec2i.Manifest, Man.SInt, Vec2i.ReadManifest, 2, UByte, false)
  implicit val Vec2iSShort = Descriptor[Vec2i, SShort](Vec2i.Manifest, Man.SInt, Vec2i.ReadManifest, 2, SShort, false)
  implicit val Vec2iUShort = Descriptor[Vec2i, UShort](Vec2i.Manifest, Man.SInt, Vec2i.ReadManifest, 2, UShort, false)
  implicit val Vec2iSInt = Descriptor[Vec2i, SInt](Vec2i.Manifest, Man.SInt, Vec2i.ReadManifest, 2, SInt, false)
  implicit val Vec2iUInt = Descriptor[Vec2i, UInt](Vec2i.Manifest, Man.SInt, Vec2i.ReadManifest, 2, UInt, false)

  implicit val Vec3iSByte = Descriptor[Vec3i, SByte](Vec3i.Manifest, Man.SInt, Vec3i.ReadManifest, 3, SByte, false)
  implicit val Vec3iUByte = Descriptor[Vec3i, UByte](Vec3i.Manifest, Man.SInt, Vec3i.ReadManifest, 3, UByte, false)
  implicit val Vec3iSShort = Descriptor[Vec3i, SShort](Vec3i.Manifest, Man.SInt, Vec3i.ReadManifest, 3, SShort, false)
  implicit val Vec3iUShort = Descriptor[Vec3i, UShort](Vec3i.Manifest, Man.SInt, Vec3i.ReadManifest, 3, UShort, false)
  implicit val Vec3iSInt = Descriptor[Vec3i, SInt](Vec3i.Manifest, Man.SInt, Vec3i.ReadManifest, 3, SInt, false)
  implicit val Vec3iUInt = Descriptor[Vec3i, UInt](Vec3i.Manifest, Man.SInt, Vec3i.ReadManifest, 3, UInt, false)

  implicit val Vec4iSByte = Descriptor[Vec4i, SByte](Vec4i.Manifest, Man.SInt, Vec4i.ReadManifest, 4, SByte, false)
  implicit val Vec4iUByte = Descriptor[Vec4i, UByte](Vec4i.Manifest, Man.SInt, Vec4i.ReadManifest, 4, UByte, false)
  implicit val Vec4iSShort = Descriptor[Vec4i, SShort](Vec4i.Manifest, Man.SInt, Vec4i.ReadManifest, 4, SShort, false)
  implicit val Vec4iUShort = Descriptor[Vec4i, UShort](Vec4i.Manifest, Man.SInt, Vec4i.ReadManifest, 4, UShort, false)
  implicit val Vec4iSInt = Descriptor[Vec4i, SInt](Vec4i.Manifest, Man.SInt, Vec4i.ReadManifest, 4, SInt, false)
  implicit val Vec4iUInt = Descriptor[Vec4i, UInt](Vec4i.Manifest, Man.SInt, Vec4i.ReadManifest, 4, UInt, false)


  implicit val RFloatSByte = Descriptor[RFloat, SByte](Man.RFloat, Man.RFloat, Manifest.Float, 1, SByte, true)
  implicit val RFloatUByte = Descriptor[RFloat, UByte](Man.RFloat, Man.RFloat, Manifest.Float, 1, UByte, true)
  implicit val RFloatSShort = Descriptor[RFloat, SShort](Man.RFloat, Man.RFloat, Manifest.Float, 1, SShort, true)
  implicit val RFloatUShort = Descriptor[RFloat, UShort](Man.RFloat, Man.RFloat, Manifest.Float, 1, UShort, true)
  implicit val RFloatSInt = Descriptor[RFloat, SInt](Man.RFloat, Man.RFloat, Manifest.Float, 1, SInt, true)
  implicit val RFloatUInt = Descriptor[RFloat, UInt](Man.RFloat, Man.RFloat, Manifest.Float, 1, UInt, true)
  implicit val RFloatHFloat = Descriptor[RFloat, HFloat](Man.RFloat, Man.RFloat, Manifest.Float, 1, HFloat, false)
  implicit val RFloatRFloat = Descriptor[RFloat, RFloat](Man.RFloat, Man.RFloat, Manifest.Float, 1, RFloat, false)

  implicit val Vec2fSByte = Descriptor[Vec2f, SByte](Vec2f.Manifest, Man.RFloat, Vec2f.ReadManifest, 2, SByte, true)
  implicit val Vec2fUByte = Descriptor[Vec2f, UByte](Vec2f.Manifest, Man.RFloat, Vec2f.ReadManifest, 2, UByte, true)
  implicit val Vec2fSShort = Descriptor[Vec2f, SShort](Vec2f.Manifest, Man.RFloat, Vec2f.ReadManifest, 2, SShort, true)
  implicit val Vec2fUShort = Descriptor[Vec2f, UShort](Vec2f.Manifest,Man. RFloat, Vec2f.ReadManifest, 2, UShort, true)
  implicit val Vec2fSInt = Descriptor[Vec2f, SInt](Vec2f.Manifest, Man.RFloat, Vec2f.ReadManifest, 2, SInt, true)
  implicit val Vec2fUInt = Descriptor[Vec2f, UInt](Vec2f.Manifest, Man.RFloat, Vec2f.ReadManifest, 2, UInt, true)
  implicit val Vec2fHFloat = Descriptor[Vec2f, HFloat](Vec2f.Manifest, Man.RFloat, Vec2f.ReadManifest, 2, HFloat, false)
  implicit val Vec2fRFloat = Descriptor[Vec2f, RFloat](Vec2f.Manifest, Man.RFloat, Vec2f.ReadManifest, 2, RFloat, false)

  implicit val Vec3fSByte = Descriptor[Vec3f, SByte](Vec3f.Manifest, Man.RFloat, Vec3f.ReadManifest, 3, SByte, true)
  implicit val Vec3fUByte = Descriptor[Vec3f, UByte](Vec3f.Manifest, Man.RFloat, Vec3f.ReadManifest, 3, UByte, true)
  implicit val Vec3fSShort = Descriptor[Vec3f, SShort](Vec3f.Manifest, Man.RFloat, Vec3f.ReadManifest, 3, SShort, true)
  implicit val Vec3fUShort = Descriptor[Vec3f, UShort](Vec3f.Manifest, Man.RFloat, Vec3f.ReadManifest, 3, UShort, true)
  implicit val Vec3fSInt = Descriptor[Vec3f, SInt](Vec3f.Manifest, Man.RFloat, Vec3f.ReadManifest, 3, SInt, true)
  implicit val Vec3fUInt = Descriptor[Vec3f, UInt](Vec3f.Manifest, Man.RFloat, Vec3f.ReadManifest, 3, UInt, true)
  implicit val Vec3fHFloat = Descriptor[Vec3f, HFloat](Vec3f.Manifest, Man.RFloat, Vec3f.ReadManifest, 3, HFloat, false)
  implicit val Vec3fRFloat = Descriptor[Vec3f, RFloat](Vec3f.Manifest, Man.RFloat, Vec3f.ReadManifest, 3, RFloat, false)

  implicit val Vec4fSByte = Descriptor[Vec4f, SByte](Vec4f.Manifest, Man.RFloat, Vec4f.ReadManifest, 4, SByte, true)
  implicit val Vec4fUByte = Descriptor[Vec4f, UByte](Vec4f.Manifest, Man.RFloat, Vec4f.ReadManifest, 4, UByte, true)
  implicit val Vec4fSShort = Descriptor[Vec4f, SShort](Vec4f.Manifest, Man.RFloat, Vec4f.ReadManifest, 4, SShort, true)
  implicit val Vec4fUShort = Descriptor[Vec4f, UShort](Vec4f.Manifest, Man.RFloat, Vec4f.ReadManifest, 4, UShort, true)
  implicit val Vec4fSInt = Descriptor[Vec4f, SInt](Vec4f.Manifest, Man.RFloat, Vec4f.ReadManifest, 4, SInt, true)
  implicit val Vec4fUInt = Descriptor[Vec4f, UInt](Vec4f.Manifest, Man.RFloat, Vec4f.ReadManifest, 4, UInt, true)
  implicit val Vec4fHFloat = Descriptor[Vec4f, HFloat](Vec4f.Manifest, Man.RFloat, Vec4f.ReadManifest, 4, HFloat, false)
  implicit val Vec4fRFloat = Descriptor[Vec4f, RFloat](Vec4f.Manifest, Man.RFloat, Vec4f.ReadManifest, 4, RFloat, false)

  implicit val Mat2x3fRFloat = Descriptor[Mat2x3f, RFloat](Mat2x3f.Manifest, Man.RFloat, Mat2x3f.ReadManifest, 6, RFloat, false)


  implicit val RDoubleSByte = Descriptor[RDouble, SByte](Man.RDouble, Man.RDouble, Manifest.Double, 1, SByte, true)
  implicit val RDoubleUByte = Descriptor[RDouble, UByte](Man.RDouble, Man.RDouble, Manifest.Double, 1, UByte, true)
  implicit val RDoubleSShort = Descriptor[RDouble, SShort](Man.RDouble, Man.RDouble, Manifest.Double, 1, SShort, true)
  implicit val RDoubleUShort = Descriptor[RDouble, UShort](Man.RDouble, Man.RDouble, Manifest.Double, 1, UShort, true)
  implicit val RDoubleSInt = Descriptor[RDouble, SInt](Man.RDouble, Man.RDouble, Manifest.Double, 1, SInt, true)
  implicit val RDoubleUInt = Descriptor[RDouble, UInt](Man.RDouble, Man.RDouble, Manifest.Double, 1, UInt, true)
  implicit val RDoubleHFloat = Descriptor[RDouble, HFloat](Man.RDouble, Man.RDouble, Manifest.Double, 1, HFloat, false)
  implicit val RDoubleRFloat = Descriptor[RDouble, RFloat](Man.RDouble, Man.RDouble, Manifest.Double, 1, RFloat, false)
  implicit val RDoubleRDouble = Descriptor[RDouble, RDouble](Man.RDouble, Man.RDouble, Manifest.Double, 1, RDouble, false)

  implicit val Vec2dSByte = Descriptor[Vec2d, SByte](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, SByte, true)
  implicit val Vec2dUByte = Descriptor[Vec2d, UByte](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, UByte, true)
  implicit val Vec2dSShort = Descriptor[Vec2d, SShort](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, SShort, true)
  implicit val Vec2dUShort = Descriptor[Vec2d, UShort](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, UShort, true)
  implicit val Vec2dSInt = Descriptor[Vec2d, SInt](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, SInt, true)
  implicit val Vec2dUInt = Descriptor[Vec2d, UInt](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, UInt, true)
  implicit val Vec2dHFloat = Descriptor[Vec2d, HFloat](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, HFloat, false)
  implicit val Vec2dRFloat = Descriptor[Vec2d, RFloat](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, RFloat, false)
  implicit val Vec2dRDouble = Descriptor[Vec2d, RDouble](Vec2d.Manifest, Man.RDouble, Vec2d.ReadManifest, 2, RDouble, false)

  implicit val Vec3dSByte = Descriptor[Vec3d, SByte](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, SByte, true)
  implicit val Vec3dUByte = Descriptor[Vec3d, UByte](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, UByte, true)
  implicit val Vec3dSShort = Descriptor[Vec3d, SShort](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, SShort, true)
  implicit val Vec3dUShort = Descriptor[Vec3d, UShort](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, UShort, true)
  implicit val Vec3dSInt = Descriptor[Vec3d, SInt](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, SInt, true)
  implicit val Vec3dUInt = Descriptor[Vec3d, UInt](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, UInt, true)
  implicit val Vec3dHFloat = Descriptor[Vec3d, HFloat](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, HFloat, false)
  implicit val Vec3dRFloat = Descriptor[Vec3d, RFloat](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, RFloat, false)
  implicit val Vec3dRDouble = Descriptor[Vec3d, RDouble](Vec3d.Manifest, Man.RDouble, Vec3d.ReadManifest, 3, RDouble, false)

  implicit val Vec4dSByte = Descriptor[Vec4d, SByte](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, SByte, true)
  implicit val Vec4dUByte = Descriptor[Vec4d, UByte](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, UByte, true)
  implicit val Vec4dSShort = Descriptor[Vec4d, SShort](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, SShort, true)
  implicit val Vec4dUShort = Descriptor[Vec4d, UShort](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, UShort, true)
  implicit val Vec4dSInt = Descriptor[Vec4d, SInt](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, SInt, true)
  implicit val Vec4dUInt = Descriptor[Vec4d, UInt](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, UInt, true)
  implicit val Vec4dHFloat = Descriptor[Vec4d, HFloat](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, HFloat, false)
  implicit val Vec4dRFloat = Descriptor[Vec4d, RFloat](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, RFloat, false)
  implicit val Vec4dRDouble = Descriptor[Vec4d, RDouble](Vec4d.Manifest, Man.RDouble, Vec4d.ReadManifest, 4, RDouble, false)

  implicit val Mat2x3dRFloat = Descriptor[Mat2x3d, RFloat](Mat2x3d.Manifest, Man.RDouble, Mat2x3d.ReadManifest, 6, RFloat, false)
  implicit val Mat2x3dRDouble = Descriptor[Mat2x3d, RDouble](Mat2x3d.Manifest, Man.RDouble, Mat2x3d.ReadManifest, 6, RDouble, false)
}
