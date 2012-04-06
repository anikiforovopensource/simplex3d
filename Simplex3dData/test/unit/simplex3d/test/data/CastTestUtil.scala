/*
 * Simplex3dData - Test Package
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

package simplex3d.test.data

import java.nio._
import scala.reflect._
import org.scalatest._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._
import simplex3d.data._
import simplex3d.data.RawType._
import simplex3d.data.float._
import simplex3d.data.double._

import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object CastTestUtil extends FunSuite {

  def testArrayCast[F <: Format, R <: Raw](
    factory: (R#Array) => DataArray[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    for (size <- 0 to 9) {
      val seq = factory(genRandomArray(size, descriptor))

      seq.rawType match {
        case SByte => testSByteArrayCast(seq.asInstanceOf[DataArray[_, SByte]])
        case UByte => testUByteArrayCast(seq.asInstanceOf[DataArray[_, UByte]])
        case SShort => testSShortArrayCast(seq.asInstanceOf[DataArray[_, SShort]])
        case UShort => testUShortArrayCast(seq.asInstanceOf[DataArray[_, UShort]])
        case SInt => testSIntArrayCast(seq.asInstanceOf[DataArray[_, SInt]])
        case UInt => testUIntArrayCast(seq.asInstanceOf[DataArray[_, UInt]])
        case HFloat => testHFloatArrayCast(seq.asInstanceOf[DataArray[_, HFloat]])
        case RFloat => testRFloatArrayCast(seq.asInstanceOf[DataArray[_, RFloat]])
        case RDouble => testRDoubleArrayCast(seq.asInstanceOf[DataArray[_, RDouble]])
      }
    }
  }

  private def testCastToArray[F <: Format, R <: Raw](
    original: ReadDataArray[_, _],
    cast: ReadDataArray[F, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[F, R]) {
    testArray(cast, readOnly, data)(descriptor)
    assert(original.sharesStorageWith(cast))
    assert(cast.sharesStorageWith(original))
  }

  private def testSByteArrayCast(da: DataArray[_, SByte]) {
    val data = da.buffer()

    testCastToArray(da, DataArray[SInt, SByte](da), false, data)(Descriptors.SIntSByte)
    testCastToArray(da, DataArray[Vec2i, SByte](da), false, data)(Descriptors.Vec2iSByte)
    testCastToArray(da, DataArray[Vec3i, SByte](da), false, data)(Descriptors.Vec3iSByte)
    testCastToArray(da, DataArray[Vec4i, SByte](da), false, data)(Descriptors.Vec4iSByte)

    testCastToArray(da, DataArray[RFloat, SByte](da), false, data)(Descriptors.RFloatSByte)
    testCastToArray(da, DataArray[Vec2f, SByte](da), false, data)(Descriptors.Vec2fSByte)
    testCastToArray(da, DataArray[Vec3f, SByte](da), false, data)(Descriptors.Vec3fSByte)
    testCastToArray(da, DataArray[Vec4f, SByte](da), false, data)(Descriptors.Vec4fSByte)
    
    testCastToArray(da, DataArray[RDouble, SByte](da), false, data)(Descriptors.RDoubleSByte)
    testCastToArray(da, DataArray[Vec2d, SByte](da), false, data)(Descriptors.Vec2dSByte)
    testCastToArray(da, DataArray[Vec3d, SByte](da), false, data)(Descriptors.Vec3dSByte)
    testCastToArray(da, DataArray[Vec4d, SByte](da), false, data)(Descriptors.Vec4dSByte)

    
    testCastToArray(da, ReadDataArray[SInt, SByte](da), false, data)(Descriptors.SIntSByte)
    testCastToArray(da, ReadDataArray[Vec2i, SByte](da), false, data)(Descriptors.Vec2iSByte)
    testCastToArray(da, ReadDataArray[Vec3i, SByte](da), false, data)(Descriptors.Vec3iSByte)
    testCastToArray(da, ReadDataArray[Vec4i, SByte](da), false, data)(Descriptors.Vec4iSByte)

    testCastToArray(da, ReadDataArray[RFloat, SByte](da), false, data)(Descriptors.RFloatSByte)
    testCastToArray(da, ReadDataArray[Vec2f, SByte](da), false, data)(Descriptors.Vec2fSByte)
    testCastToArray(da, ReadDataArray[Vec3f, SByte](da), false, data)(Descriptors.Vec3fSByte)
    testCastToArray(da, ReadDataArray[Vec4f, SByte](da), false, data)(Descriptors.Vec4fSByte)
    
    testCastToArray(da, ReadDataArray[RDouble, SByte](da), false, data)(Descriptors.RDoubleSByte)
    testCastToArray(da, ReadDataArray[Vec2d, SByte](da), false, data)(Descriptors.Vec2dSByte)
    testCastToArray(da, ReadDataArray[Vec3d, SByte](da), false, data)(Descriptors.Vec3dSByte)
    testCastToArray(da, ReadDataArray[Vec4d, SByte](da), false, data)(Descriptors.Vec4dSByte)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, SByte]]
    intercept[IllegalArgumentException] { DataArray[SInt, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, SByte](roCast) }
    
    intercept[IllegalArgumentException] { DataArray[RFloat, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, SByte](roCast) }
    
    intercept[IllegalArgumentException] { DataArray[RDouble, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, SByte](roCast) }
    
    
    testCastToArray(ro, ReadDataArray[SInt, SByte](ro), true, data)(Descriptors.SIntSByte)
    testCastToArray(ro, ReadDataArray[Vec2i, SByte](ro), true, data)(Descriptors.Vec2iSByte)
    testCastToArray(ro, ReadDataArray[Vec3i, SByte](ro), true, data)(Descriptors.Vec3iSByte)
    testCastToArray(ro, ReadDataArray[Vec4i, SByte](ro), true, data)(Descriptors.Vec4iSByte)

    testCastToArray(ro, ReadDataArray[RFloat, SByte](ro), true, data)(Descriptors.RFloatSByte)
    testCastToArray(ro, ReadDataArray[Vec2f, SByte](ro), true, data)(Descriptors.Vec2fSByte)
    testCastToArray(ro, ReadDataArray[Vec3f, SByte](ro), true, data)(Descriptors.Vec3fSByte)
    testCastToArray(ro, ReadDataArray[Vec4f, SByte](ro), true, data)(Descriptors.Vec4fSByte)
    
    testCastToArray(ro, ReadDataArray[RDouble, SByte](ro), true, data)(Descriptors.RDoubleSByte)
    testCastToArray(ro, ReadDataArray[Vec2d, SByte](ro), true, data)(Descriptors.Vec2dSByte)
    testCastToArray(ro, ReadDataArray[Vec3d, SByte](ro), true, data)(Descriptors.Vec3dSByte)
    testCastToArray(ro, ReadDataArray[Vec4d, SByte](ro), true, data)(Descriptors.Vec4dSByte)
    
    
    val contigTest = Contiguous[SInt, SByte](da).asInstanceOf[DataArray[SInt, SByte]]
    testCastToArray(da, contigTest, false, data)(Descriptors.SIntSByte)
    intercept[IllegalArgumentException] { Contiguous[SInt, SByte](roCast) }
    val readContigTest = ReadContiguous[SInt, SByte](ro).asInstanceOf[ReadDataArray[SInt, SByte]]
    testCastToArray(ro, readContigTest, true, data)(Descriptors.SIntSByte)
  }
  
  private def testUByteArrayCast(da: DataArray[_, UByte]) {
    val data = da.buffer()
    
    testCastToArray(da, DataArray[SInt, UByte](da), false, data)(Descriptors.SIntUByte)
    testCastToArray(da, DataArray[Vec2i, UByte](da), false, data)(Descriptors.Vec2iUByte)
    testCastToArray(da, DataArray[Vec3i, UByte](da), false, data)(Descriptors.Vec3iUByte)
    testCastToArray(da, DataArray[Vec4i, UByte](da), false, data)(Descriptors.Vec4iUByte)

    testCastToArray(da, DataArray[RFloat, UByte](da), false, data)(Descriptors.RFloatUByte)
    testCastToArray(da, DataArray[Vec2f, UByte](da), false, data)(Descriptors.Vec2fUByte)
    testCastToArray(da, DataArray[Vec3f, UByte](da), false, data)(Descriptors.Vec3fUByte)
    testCastToArray(da, DataArray[Vec4f, UByte](da), false, data)(Descriptors.Vec4fUByte)
    
    testCastToArray(da, DataArray[RDouble, UByte](da), false, data)(Descriptors.RDoubleUByte)
    testCastToArray(da, DataArray[Vec2d, UByte](da), false, data)(Descriptors.Vec2dUByte)
    testCastToArray(da, DataArray[Vec3d, UByte](da), false, data)(Descriptors.Vec3dUByte)
    testCastToArray(da, DataArray[Vec4d, UByte](da), false, data)(Descriptors.Vec4dUByte)

    
    testCastToArray(da, ReadDataArray[SInt, UByte](da), false, data)(Descriptors.SIntUByte)
    testCastToArray(da, ReadDataArray[Vec2i, UByte](da), false, data)(Descriptors.Vec2iUByte)
    testCastToArray(da, ReadDataArray[Vec3i, UByte](da), false, data)(Descriptors.Vec3iUByte)
    testCastToArray(da, ReadDataArray[Vec4i, UByte](da), false, data)(Descriptors.Vec4iUByte)

    testCastToArray(da, ReadDataArray[RFloat, UByte](da), false, data)(Descriptors.RFloatUByte)
    testCastToArray(da, ReadDataArray[Vec2f, UByte](da), false, data)(Descriptors.Vec2fUByte)
    testCastToArray(da, ReadDataArray[Vec3f, UByte](da), false, data)(Descriptors.Vec3fUByte)
    testCastToArray(da, ReadDataArray[Vec4f, UByte](da), false, data)(Descriptors.Vec4fUByte)
    
    testCastToArray(da, ReadDataArray[RDouble, UByte](da), false, data)(Descriptors.RDoubleUByte)
    testCastToArray(da, ReadDataArray[Vec2d, UByte](da), false, data)(Descriptors.Vec2dUByte)
    testCastToArray(da, ReadDataArray[Vec3d, UByte](da), false, data)(Descriptors.Vec3dUByte)
    testCastToArray(da, ReadDataArray[Vec4d, UByte](da), false, data)(Descriptors.Vec4dUByte)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, UByte]]
    intercept[IllegalArgumentException] { DataArray[SInt, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, UByte](roCast) }
    
    intercept[IllegalArgumentException] { DataArray[RFloat, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, UByte](roCast) }
    
    intercept[IllegalArgumentException] { DataArray[RDouble, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, UByte](roCast) }


    testCastToArray(ro, ReadDataArray[SInt, UByte](ro), true, data)(Descriptors.SIntUByte)
    testCastToArray(ro, ReadDataArray[Vec2i, UByte](ro), true, data)(Descriptors.Vec2iUByte)
    testCastToArray(ro, ReadDataArray[Vec3i, UByte](ro), true, data)(Descriptors.Vec3iUByte)
    testCastToArray(ro, ReadDataArray[Vec4i, UByte](ro), true, data)(Descriptors.Vec4iUByte)

    testCastToArray(ro, ReadDataArray[RFloat, UByte](ro), true, data)(Descriptors.RFloatUByte)
    testCastToArray(ro, ReadDataArray[Vec2f, UByte](ro), true, data)(Descriptors.Vec2fUByte)
    testCastToArray(ro, ReadDataArray[Vec3f, UByte](ro), true, data)(Descriptors.Vec3fUByte)
    testCastToArray(ro, ReadDataArray[Vec4f, UByte](ro), true, data)(Descriptors.Vec4fUByte)
    
    testCastToArray(ro, ReadDataArray[RDouble, UByte](ro), true, data)(Descriptors.RDoubleUByte)
    testCastToArray(ro, ReadDataArray[Vec2d, UByte](ro), true, data)(Descriptors.Vec2dUByte)
    testCastToArray(ro, ReadDataArray[Vec3d, UByte](ro), true, data)(Descriptors.Vec3dUByte)
    testCastToArray(ro, ReadDataArray[Vec4d, UByte](ro), true, data)(Descriptors.Vec4dUByte)
  }

  private def testSShortArrayCast(da: DataArray[_, SShort]) {
    val data = da.buffer()

    testCastToArray(da, DataArray[SInt, SShort](da), false, data)(Descriptors.SIntSShort)
    testCastToArray(da, DataArray[Vec2i, SShort](da), false, data)(Descriptors.Vec2iSShort)
    testCastToArray(da, DataArray[Vec3i, SShort](da), false, data)(Descriptors.Vec3iSShort)
    testCastToArray(da, DataArray[Vec4i, SShort](da), false, data)(Descriptors.Vec4iSShort)

    testCastToArray(da, DataArray[RFloat, SShort](da), false, data)(Descriptors.RFloatSShort)
    testCastToArray(da, DataArray[Vec2f, SShort](da), false, data)(Descriptors.Vec2fSShort)
    testCastToArray(da, DataArray[Vec3f, SShort](da), false, data)(Descriptors.Vec3fSShort)
    testCastToArray(da, DataArray[Vec4f, SShort](da), false, data)(Descriptors.Vec4fSShort)
    
    testCastToArray(da, DataArray[RDouble, SShort](da), false, data)(Descriptors.RDoubleSShort)
    testCastToArray(da, DataArray[Vec2d, SShort](da), false, data)(Descriptors.Vec2dSShort)
    testCastToArray(da, DataArray[Vec3d, SShort](da), false, data)(Descriptors.Vec3dSShort)
    testCastToArray(da, DataArray[Vec4d, SShort](da), false, data)(Descriptors.Vec4dSShort)

    
    testCastToArray(da, ReadDataArray[SInt, SShort](da), false, data)(Descriptors.SIntSShort)
    testCastToArray(da, ReadDataArray[Vec2i, SShort](da), false, data)(Descriptors.Vec2iSShort)
    testCastToArray(da, ReadDataArray[Vec3i, SShort](da), false, data)(Descriptors.Vec3iSShort)
    testCastToArray(da, ReadDataArray[Vec4i, SShort](da), false, data)(Descriptors.Vec4iSShort)

    testCastToArray(da, ReadDataArray[RFloat, SShort](da), false, data)(Descriptors.RFloatSShort)
    testCastToArray(da, ReadDataArray[Vec2f, SShort](da), false, data)(Descriptors.Vec2fSShort)
    testCastToArray(da, ReadDataArray[Vec3f, SShort](da), false, data)(Descriptors.Vec3fSShort)
    testCastToArray(da, ReadDataArray[Vec4f, SShort](da), false, data)(Descriptors.Vec4fSShort)
    
    testCastToArray(da, ReadDataArray[RDouble, SShort](da), false, data)(Descriptors.RDoubleSShort)
    testCastToArray(da, ReadDataArray[Vec2d, SShort](da), false, data)(Descriptors.Vec2dSShort)
    testCastToArray(da, ReadDataArray[Vec3d, SShort](da), false, data)(Descriptors.Vec3dSShort)
    testCastToArray(da, ReadDataArray[Vec4d, SShort](da), false, data)(Descriptors.Vec4dSShort)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, SShort]]
    intercept[IllegalArgumentException] { DataArray[SInt, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, SShort](roCast) }

    intercept[IllegalArgumentException] { DataArray[RFloat, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, SShort](roCast) }

    intercept[IllegalArgumentException] { DataArray[RDouble, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, SShort](roCast) }


    testCastToArray(ro, ReadDataArray[SInt, SShort](ro), true, data)(Descriptors.SIntSShort)
    testCastToArray(ro, ReadDataArray[Vec2i, SShort](ro), true, data)(Descriptors.Vec2iSShort)
    testCastToArray(ro, ReadDataArray[Vec3i, SShort](ro), true, data)(Descriptors.Vec3iSShort)
    testCastToArray(ro, ReadDataArray[Vec4i, SShort](ro), true, data)(Descriptors.Vec4iSShort)

    testCastToArray(ro, ReadDataArray[RFloat, SShort](ro), true, data)(Descriptors.RFloatSShort)
    testCastToArray(ro, ReadDataArray[Vec2f, SShort](ro), true, data)(Descriptors.Vec2fSShort)
    testCastToArray(ro, ReadDataArray[Vec3f, SShort](ro), true, data)(Descriptors.Vec3fSShort)
    testCastToArray(ro, ReadDataArray[Vec4f, SShort](ro), true, data)(Descriptors.Vec4fSShort)
    
    testCastToArray(ro, ReadDataArray[RDouble, SShort](ro), true, data)(Descriptors.RDoubleSShort)
    testCastToArray(ro, ReadDataArray[Vec2d, SShort](ro), true, data)(Descriptors.Vec2dSShort)
    testCastToArray(ro, ReadDataArray[Vec3d, SShort](ro), true, data)(Descriptors.Vec3dSShort)
    testCastToArray(ro, ReadDataArray[Vec4d, SShort](ro), true, data)(Descriptors.Vec4dSShort)
  }
  
  private def testUShortArrayCast(da: DataArray[_, UShort]) {
    val data = da.buffer()
    
    testCastToArray(da, DataArray[SInt, UShort](da), false, data)(Descriptors.SIntUShort)
    testCastToArray(da, DataArray[Vec2i, UShort](da), false, data)(Descriptors.Vec2iUShort)
    testCastToArray(da, DataArray[Vec3i, UShort](da), false, data)(Descriptors.Vec3iUShort)
    testCastToArray(da, DataArray[Vec4i, UShort](da), false, data)(Descriptors.Vec4iUShort)

    testCastToArray(da, DataArray[RFloat, UShort](da), false, data)(Descriptors.RFloatUShort)
    testCastToArray(da, DataArray[Vec2f, UShort](da), false, data)(Descriptors.Vec2fUShort)
    testCastToArray(da, DataArray[Vec3f, UShort](da), false, data)(Descriptors.Vec3fUShort)
    testCastToArray(da, DataArray[Vec4f, UShort](da), false, data)(Descriptors.Vec4fUShort)
    
    testCastToArray(da, DataArray[RDouble, UShort](da), false, data)(Descriptors.RDoubleUShort)
    testCastToArray(da, DataArray[Vec2d, UShort](da), false, data)(Descriptors.Vec2dUShort)
    testCastToArray(da, DataArray[Vec3d, UShort](da), false, data)(Descriptors.Vec3dUShort)
    testCastToArray(da, DataArray[Vec4d, UShort](da), false, data)(Descriptors.Vec4dUShort)

    
    testCastToArray(da, ReadDataArray[SInt, UShort](da), false, data)(Descriptors.SIntUShort)
    testCastToArray(da, ReadDataArray[Vec2i, UShort](da), false, data)(Descriptors.Vec2iUShort)
    testCastToArray(da, ReadDataArray[Vec3i, UShort](da), false, data)(Descriptors.Vec3iUShort)
    testCastToArray(da, ReadDataArray[Vec4i, UShort](da), false, data)(Descriptors.Vec4iUShort)

    testCastToArray(da, ReadDataArray[RFloat, UShort](da), false, data)(Descriptors.RFloatUShort)
    testCastToArray(da, ReadDataArray[Vec2f, UShort](da), false, data)(Descriptors.Vec2fUShort)
    testCastToArray(da, ReadDataArray[Vec3f, UShort](da), false, data)(Descriptors.Vec3fUShort)
    testCastToArray(da, ReadDataArray[Vec4f, UShort](da), false, data)(Descriptors.Vec4fUShort)
    
    testCastToArray(da, ReadDataArray[RDouble, UShort](da), false, data)(Descriptors.RDoubleUShort)
    testCastToArray(da, ReadDataArray[Vec2d, UShort](da), false, data)(Descriptors.Vec2dUShort)
    testCastToArray(da, ReadDataArray[Vec3d, UShort](da), false, data)(Descriptors.Vec3dUShort)
    testCastToArray(da, ReadDataArray[Vec4d, UShort](da), false, data)(Descriptors.Vec4dUShort)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, UShort]]
    intercept[IllegalArgumentException] { DataArray[SInt, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, UShort](roCast) }

    intercept[IllegalArgumentException] { DataArray[RFloat, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, UShort](roCast) }

    intercept[IllegalArgumentException] { DataArray[RDouble, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, UShort](roCast) }


    testCastToArray(ro, ReadDataArray[SInt, UShort](ro), true, data)(Descriptors.SIntUShort)
    testCastToArray(ro, ReadDataArray[Vec2i, UShort](ro), true, data)(Descriptors.Vec2iUShort)
    testCastToArray(ro, ReadDataArray[Vec3i, UShort](ro), true, data)(Descriptors.Vec3iUShort)
    testCastToArray(ro, ReadDataArray[Vec4i, UShort](ro), true, data)(Descriptors.Vec4iUShort)

    testCastToArray(ro, ReadDataArray[RFloat, UShort](ro), true, data)(Descriptors.RFloatUShort)
    testCastToArray(ro, ReadDataArray[Vec2f, UShort](ro), true, data)(Descriptors.Vec2fUShort)
    testCastToArray(ro, ReadDataArray[Vec3f, UShort](ro), true, data)(Descriptors.Vec3fUShort)
    testCastToArray(ro, ReadDataArray[Vec4f, UShort](ro), true, data)(Descriptors.Vec4fUShort)
    
    testCastToArray(ro, ReadDataArray[RDouble, UShort](ro), true, data)(Descriptors.RDoubleUShort)
    testCastToArray(ro, ReadDataArray[Vec2d, UShort](ro), true, data)(Descriptors.Vec2dUShort)
    testCastToArray(ro, ReadDataArray[Vec3d, UShort](ro), true, data)(Descriptors.Vec3dUShort)
    testCastToArray(ro, ReadDataArray[Vec4d, UShort](ro), true, data)(Descriptors.Vec4dUShort)
  }
  
  private def testSIntArrayCast(da: DataArray[_, SInt]) {
    val data = da.buffer()

    testCastToArray(da, DataArray[SInt, SInt](da), false, data)(Descriptors.SIntSInt)
    testCastToArray(da, DataArray[Vec2i, SInt](da), false, data)(Descriptors.Vec2iSInt)
    testCastToArray(da, DataArray[Vec3i, SInt](da), false, data)(Descriptors.Vec3iSInt)
    testCastToArray(da, DataArray[Vec4i, SInt](da), false, data)(Descriptors.Vec4iSInt)

    testCastToArray(da, DataArray[RFloat, SInt](da), false, data)(Descriptors.RFloatSInt)
    testCastToArray(da, DataArray[Vec2f, SInt](da), false, data)(Descriptors.Vec2fSInt)
    testCastToArray(da, DataArray[Vec3f, SInt](da), false, data)(Descriptors.Vec3fSInt)
    testCastToArray(da, DataArray[Vec4f, SInt](da), false, data)(Descriptors.Vec4fSInt)
    
    testCastToArray(da, DataArray[RDouble, SInt](da), false, data)(Descriptors.RDoubleSInt)
    testCastToArray(da, DataArray[Vec2d, SInt](da), false, data)(Descriptors.Vec2dSInt)
    testCastToArray(da, DataArray[Vec3d, SInt](da), false, data)(Descriptors.Vec3dSInt)
    testCastToArray(da, DataArray[Vec4d, SInt](da), false, data)(Descriptors.Vec4dSInt)

    
    testCastToArray(da, ReadDataArray[SInt, SInt](da), false, data)(Descriptors.SIntSInt)
    testCastToArray(da, ReadDataArray[Vec2i, SInt](da), false, data)(Descriptors.Vec2iSInt)
    testCastToArray(da, ReadDataArray[Vec3i, SInt](da), false, data)(Descriptors.Vec3iSInt)
    testCastToArray(da, ReadDataArray[Vec4i, SInt](da), false, data)(Descriptors.Vec4iSInt)

    testCastToArray(da, ReadDataArray[RFloat, SInt](da), false, data)(Descriptors.RFloatSInt)
    testCastToArray(da, ReadDataArray[Vec2f, SInt](da), false, data)(Descriptors.Vec2fSInt)
    testCastToArray(da, ReadDataArray[Vec3f, SInt](da), false, data)(Descriptors.Vec3fSInt)
    testCastToArray(da, ReadDataArray[Vec4f, SInt](da), false, data)(Descriptors.Vec4fSInt)
    
    testCastToArray(da, ReadDataArray[RDouble, SInt](da), false, data)(Descriptors.RDoubleSInt)
    testCastToArray(da, ReadDataArray[Vec2d, SInt](da), false, data)(Descriptors.Vec2dSInt)
    testCastToArray(da, ReadDataArray[Vec3d, SInt](da), false, data)(Descriptors.Vec3dSInt)
    testCastToArray(da, ReadDataArray[Vec4d, SInt](da), false, data)(Descriptors.Vec4dSInt)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, SInt]]
    intercept[IllegalArgumentException] { DataArray[SInt, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, SInt](roCast) }

    intercept[IllegalArgumentException] { DataArray[RFloat, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, SInt](roCast) }

    intercept[IllegalArgumentException] { DataArray[RDouble, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, SInt](roCast) }


    testCastToArray(ro, ReadDataArray[SInt, SInt](ro), true, data)(Descriptors.SIntSInt)
    testCastToArray(ro, ReadDataArray[Vec2i, SInt](ro), true, data)(Descriptors.Vec2iSInt)
    testCastToArray(ro, ReadDataArray[Vec3i, SInt](ro), true, data)(Descriptors.Vec3iSInt)
    testCastToArray(ro, ReadDataArray[Vec4i, SInt](ro), true, data)(Descriptors.Vec4iSInt)

    testCastToArray(ro, ReadDataArray[RFloat, SInt](ro), true, data)(Descriptors.RFloatSInt)
    testCastToArray(ro, ReadDataArray[Vec2f, SInt](ro), true, data)(Descriptors.Vec2fSInt)
    testCastToArray(ro, ReadDataArray[Vec3f, SInt](ro), true, data)(Descriptors.Vec3fSInt)
    testCastToArray(ro, ReadDataArray[Vec4f, SInt](ro), true, data)(Descriptors.Vec4fSInt)
    
    testCastToArray(ro, ReadDataArray[RDouble, SInt](ro), true, data)(Descriptors.RDoubleSInt)
    testCastToArray(ro, ReadDataArray[Vec2d, SInt](ro), true, data)(Descriptors.Vec2dSInt)
    testCastToArray(ro, ReadDataArray[Vec3d, SInt](ro), true, data)(Descriptors.Vec3dSInt)
    testCastToArray(ro, ReadDataArray[Vec4d, SInt](ro), true, data)(Descriptors.Vec4dSInt)
  }
  
  private def testUIntArrayCast(da: DataArray[_, UInt]) {
    val data = da.buffer()
    
    testCastToArray(da, DataArray[SInt, UInt](da), false, data)(Descriptors.SIntUInt)
    testCastToArray(da, DataArray[Vec2i, UInt](da), false, data)(Descriptors.Vec2iUInt)
    testCastToArray(da, DataArray[Vec3i, UInt](da), false, data)(Descriptors.Vec3iUInt)
    testCastToArray(da, DataArray[Vec4i, UInt](da), false, data)(Descriptors.Vec4iUInt)

    testCastToArray(da, DataArray[RFloat, UInt](da), false, data)(Descriptors.RFloatUInt)
    testCastToArray(da, DataArray[Vec2f, UInt](da), false, data)(Descriptors.Vec2fUInt)
    testCastToArray(da, DataArray[Vec3f, UInt](da), false, data)(Descriptors.Vec3fUInt)
    testCastToArray(da, DataArray[Vec4f, UInt](da), false, data)(Descriptors.Vec4fUInt)
    
    testCastToArray(da, DataArray[RDouble, UInt](da), false, data)(Descriptors.RDoubleUInt)
    testCastToArray(da, DataArray[Vec2d, UInt](da), false, data)(Descriptors.Vec2dUInt)
    testCastToArray(da, DataArray[Vec3d, UInt](da), false, data)(Descriptors.Vec3dUInt)
    testCastToArray(da, DataArray[Vec4d, UInt](da), false, data)(Descriptors.Vec4dUInt)

    
    testCastToArray(da, ReadDataArray[SInt, UInt](da), false, data)(Descriptors.SIntUInt)
    testCastToArray(da, ReadDataArray[Vec2i, UInt](da), false, data)(Descriptors.Vec2iUInt)
    testCastToArray(da, ReadDataArray[Vec3i, UInt](da), false, data)(Descriptors.Vec3iUInt)
    testCastToArray(da, ReadDataArray[Vec4i, UInt](da), false, data)(Descriptors.Vec4iUInt)

    testCastToArray(da, ReadDataArray[RFloat, UInt](da), false, data)(Descriptors.RFloatUInt)
    testCastToArray(da, ReadDataArray[Vec2f, UInt](da), false, data)(Descriptors.Vec2fUInt)
    testCastToArray(da, ReadDataArray[Vec3f, UInt](da), false, data)(Descriptors.Vec3fUInt)
    testCastToArray(da, ReadDataArray[Vec4f, UInt](da), false, data)(Descriptors.Vec4fUInt)
    
    testCastToArray(da, ReadDataArray[RDouble, UInt](da), false, data)(Descriptors.RDoubleUInt)
    testCastToArray(da, ReadDataArray[Vec2d, UInt](da), false, data)(Descriptors.Vec2dUInt)
    testCastToArray(da, ReadDataArray[Vec3d, UInt](da), false, data)(Descriptors.Vec3dUInt)
    testCastToArray(da, ReadDataArray[Vec4d, UInt](da), false, data)(Descriptors.Vec4dUInt)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, UInt]]
    intercept[IllegalArgumentException] { DataArray[SInt, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, UInt](roCast) }

    intercept[IllegalArgumentException] { DataArray[RFloat, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, UInt](roCast) }

    intercept[IllegalArgumentException] { DataArray[RDouble, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, UInt](roCast) }


    testCastToArray(ro, ReadDataArray[SInt, UInt](ro), true, data)(Descriptors.SIntUInt)
    testCastToArray(ro, ReadDataArray[Vec2i, UInt](ro), true, data)(Descriptors.Vec2iUInt)
    testCastToArray(ro, ReadDataArray[Vec3i, UInt](ro), true, data)(Descriptors.Vec3iUInt)
    testCastToArray(ro, ReadDataArray[Vec4i, UInt](ro), true, data)(Descriptors.Vec4iUInt)

    testCastToArray(ro, ReadDataArray[RFloat, UInt](ro), true, data)(Descriptors.RFloatUInt)
    testCastToArray(ro, ReadDataArray[Vec2f, UInt](ro), true, data)(Descriptors.Vec2fUInt)
    testCastToArray(ro, ReadDataArray[Vec3f, UInt](ro), true, data)(Descriptors.Vec3fUInt)
    testCastToArray(ro, ReadDataArray[Vec4f, UInt](ro), true, data)(Descriptors.Vec4fUInt)
    
    testCastToArray(ro, ReadDataArray[RDouble, UInt](ro), true, data)(Descriptors.RDoubleUInt)
    testCastToArray(ro, ReadDataArray[Vec2d, UInt](ro), true, data)(Descriptors.Vec2dUInt)
    testCastToArray(ro, ReadDataArray[Vec3d, UInt](ro), true, data)(Descriptors.Vec3dUInt)
    testCastToArray(ro, ReadDataArray[Vec4d, UInt](ro), true, data)(Descriptors.Vec4dUInt)
  }

  private def testHFloatArrayCast(da: DataArray[_, HFloat]) {
    val data = da.buffer()

    testCastToArray(da, DataArray[RFloat, HFloat](da), false, data)(Descriptors.RFloatHFloat)
    testCastToArray(da, DataArray[Vec2f, HFloat](da), false, data)(Descriptors.Vec2fHFloat)
    testCastToArray(da, DataArray[Vec3f, HFloat](da), false, data)(Descriptors.Vec3fHFloat)
    testCastToArray(da, DataArray[Vec4f, HFloat](da), false, data)(Descriptors.Vec4fHFloat)

    testCastToArray(da, DataArray[RDouble, HFloat](da), false, data)(Descriptors.RDoubleHFloat)
    testCastToArray(da, DataArray[Vec2d, HFloat](da), false, data)(Descriptors.Vec2dHFloat)
    testCastToArray(da, DataArray[Vec3d, HFloat](da), false, data)(Descriptors.Vec3dHFloat)
    testCastToArray(da, DataArray[Vec4d, HFloat](da), false, data)(Descriptors.Vec4dHFloat)


    testCastToArray(da, ReadDataArray[RFloat, HFloat](da), false, data)(Descriptors.RFloatHFloat)
    testCastToArray(da, ReadDataArray[Vec2f, HFloat](da), false, data)(Descriptors.Vec2fHFloat)
    testCastToArray(da, ReadDataArray[Vec3f, HFloat](da), false, data)(Descriptors.Vec3fHFloat)
    testCastToArray(da, ReadDataArray[Vec4f, HFloat](da), false, data)(Descriptors.Vec4fHFloat)

    testCastToArray(da, ReadDataArray[RDouble, HFloat](da), false, data)(Descriptors.RDoubleHFloat)
    testCastToArray(da, ReadDataArray[Vec2d, HFloat](da), false, data)(Descriptors.Vec2dHFloat)
    testCastToArray(da, ReadDataArray[Vec3d, HFloat](da), false, data)(Descriptors.Vec3dHFloat)
    testCastToArray(da, ReadDataArray[Vec4d, HFloat](da), false, data)(Descriptors.Vec4dHFloat)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, HFloat]]
    intercept[IllegalArgumentException] { DataArray[RFloat, HFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, HFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, HFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, HFloat](roCast) }

    intercept[IllegalArgumentException] { DataArray[RDouble, HFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, HFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, HFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, HFloat](roCast) }


    testCastToArray(ro, ReadDataArray[RFloat, HFloat](ro), true, data)(Descriptors.RFloatHFloat)
    testCastToArray(ro, ReadDataArray[Vec2f, HFloat](ro), true, data)(Descriptors.Vec2fHFloat)
    testCastToArray(ro, ReadDataArray[Vec3f, HFloat](ro), true, data)(Descriptors.Vec3fHFloat)
    testCastToArray(ro, ReadDataArray[Vec4f, HFloat](ro), true, data)(Descriptors.Vec4fHFloat)

    testCastToArray(ro, ReadDataArray[RDouble, HFloat](ro), true, data)(Descriptors.RDoubleHFloat)
    testCastToArray(ro, ReadDataArray[Vec2d, HFloat](ro), true, data)(Descriptors.Vec2dHFloat)
    testCastToArray(ro, ReadDataArray[Vec3d, HFloat](ro), true, data)(Descriptors.Vec3dHFloat)
    testCastToArray(ro, ReadDataArray[Vec4d, HFloat](ro), true, data)(Descriptors.Vec4dHFloat)
  }
  
  private def testRFloatArrayCast(da: DataArray[_, RFloat]) {
    val data = da.buffer()

    testCastToArray(da, DataArray[RFloat, RFloat](da), false, data)(Descriptors.RFloatRFloat)
    testCastToArray(da, DataArray[Vec2f, RFloat](da), false, data)(Descriptors.Vec2fRFloat)
    testCastToArray(da, DataArray[Vec3f, RFloat](da), false, data)(Descriptors.Vec3fRFloat)
    testCastToArray(da, DataArray[Vec4f, RFloat](da), false, data)(Descriptors.Vec4fRFloat)
    testCastToArray(da, DataArray[Mat3x2f, RFloat](da), false, data)(Descriptors.Mat3x2fRFloat)

    testCastToArray(da, DataArray[RDouble, RFloat](da), false, data)(Descriptors.RDoubleRFloat)
    testCastToArray(da, DataArray[Vec2d, RFloat](da), false, data)(Descriptors.Vec2dRFloat)
    testCastToArray(da, DataArray[Vec3d, RFloat](da), false, data)(Descriptors.Vec3dRFloat)
    testCastToArray(da, DataArray[Vec4d, RFloat](da), false, data)(Descriptors.Vec4dRFloat)
    testCastToArray(da, DataArray[Mat3x2d, RFloat](da), false, data)(Descriptors.Mat3x2dRFloat)


    testCastToArray(da, ReadDataArray[RFloat, RFloat](da), false, data)(Descriptors.RFloatRFloat)
    testCastToArray(da, ReadDataArray[Vec2f, RFloat](da), false, data)(Descriptors.Vec2fRFloat)
    testCastToArray(da, ReadDataArray[Vec3f, RFloat](da), false, data)(Descriptors.Vec3fRFloat)
    testCastToArray(da, ReadDataArray[Vec4f, RFloat](da), false, data)(Descriptors.Vec4fRFloat)
    testCastToArray(da, ReadDataArray[Mat3x2f, RFloat](da), false, data)(Descriptors.Mat3x2fRFloat)

    testCastToArray(da, ReadDataArray[RDouble, RFloat](da), false, data)(Descriptors.RDoubleRFloat)
    testCastToArray(da, ReadDataArray[Vec2d, RFloat](da), false, data)(Descriptors.Vec2dRFloat)
    testCastToArray(da, ReadDataArray[Vec3d, RFloat](da), false, data)(Descriptors.Vec3dRFloat)
    testCastToArray(da, ReadDataArray[Vec4d, RFloat](da), false, data)(Descriptors.Vec4dRFloat)
    testCastToArray(da, ReadDataArray[Mat3x2d, RFloat](da), false, data)(Descriptors.Mat3x2dRFloat)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, RFloat]]
    intercept[IllegalArgumentException] { DataArray[RFloat, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Mat3x2f, RFloat](roCast) }

    intercept[IllegalArgumentException] { DataArray[RDouble, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Mat3x2d, RFloat](roCast) }


    testCastToArray(ro, ReadDataArray[RFloat, RFloat](ro), true, data)(Descriptors.RFloatRFloat)
    testCastToArray(ro, ReadDataArray[Vec2f, RFloat](ro), true, data)(Descriptors.Vec2fRFloat)
    testCastToArray(ro, ReadDataArray[Vec3f, RFloat](ro), true, data)(Descriptors.Vec3fRFloat)
    testCastToArray(ro, ReadDataArray[Vec4f, RFloat](ro), true, data)(Descriptors.Vec4fRFloat)
    testCastToArray(ro, ReadDataArray[Mat3x2f, RFloat](ro), true, data)(Descriptors.Mat3x2fRFloat)

    testCastToArray(ro, ReadDataArray[RDouble, RFloat](ro), true, data)(Descriptors.RDoubleRFloat)
    testCastToArray(ro, ReadDataArray[Vec2d, RFloat](ro), true, data)(Descriptors.Vec2dRFloat)
    testCastToArray(ro, ReadDataArray[Vec3d, RFloat](ro), true, data)(Descriptors.Vec3dRFloat)
    testCastToArray(ro, ReadDataArray[Vec4d, RFloat](ro), true, data)(Descriptors.Vec4dRFloat)
    testCastToArray(ro, ReadDataArray[Mat3x2d, RFloat](ro), true, data)(Descriptors.Mat3x2dRFloat)
  }
  
  private def testRDoubleArrayCast(da: DataArray[_, RDouble]) {
    val data = da.buffer()

    testCastToArray(da, DataArray[RDouble, RDouble](da), false, data)(Descriptors.RDoubleRDouble)
    testCastToArray(da, DataArray[Vec2d, RDouble](da), false, data)(Descriptors.Vec2dRDouble)
    testCastToArray(da, DataArray[Vec3d, RDouble](da), false, data)(Descriptors.Vec3dRDouble)
    testCastToArray(da, DataArray[Vec4d, RDouble](da), false, data)(Descriptors.Vec4dRDouble)
    testCastToArray(da, DataArray[Mat3x2d, RDouble](da), false, data)(Descriptors.Mat3x2dRDouble)


    testCastToArray(da, ReadDataArray[RDouble, RDouble](da), false, data)(Descriptors.RDoubleRDouble)
    testCastToArray(da, ReadDataArray[Vec2d, RDouble](da), false, data)(Descriptors.Vec2dRDouble)
    testCastToArray(da, ReadDataArray[Vec3d, RDouble](da), false, data)(Descriptors.Vec3dRDouble)
    testCastToArray(da, ReadDataArray[Vec4d, RDouble](da), false, data)(Descriptors.Vec4dRDouble)
    testCastToArray(da, ReadDataArray[Mat3x2d, RDouble](da), false, data)(Descriptors.Mat3x2dRDouble)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, RDouble]]
    intercept[IllegalArgumentException] { DataArray[RDouble, RDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, RDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, RDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, RDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Mat3x2d, RDouble](roCast) }


    testCastToArray(ro, ReadDataArray[RDouble, RDouble](ro), true, data)(Descriptors.RDoubleRDouble)
    testCastToArray(ro, ReadDataArray[Vec2d, RDouble](ro), true, data)(Descriptors.Vec2dRDouble)
    testCastToArray(ro, ReadDataArray[Vec3d, RDouble](ro), true, data)(Descriptors.Vec3dRDouble)
    testCastToArray(ro, ReadDataArray[Vec4d, RDouble](ro), true, data)(Descriptors.Vec4dRDouble)
    testCastToArray(ro, ReadDataArray[Mat3x2d, RDouble](ro), true, data)(Descriptors.Mat3x2dRDouble)
  }


  def testBufferCast[F <: Format, R <: Raw](
    factory: (ByteBuffer) => DataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {

    for (size <- 0 to 1; extraBytes <- 0 to 8) {
      val (bytes, _) = genRandomBuffer(size*8*4*2 + extraBytes, Descriptors.SIntSByte)
      val seq = factory(bytes)


      testCastToBuffer(seq, DataBuffer[SInt, SByte](_), bytes)(Descriptors.SIntSByte)
      testCastToBuffer(seq, DataBuffer[SInt, UByte](_), bytes)(Descriptors.SIntUByte)
      testCastToBuffer(seq, DataBuffer[SInt, SShort](_), bytes)(Descriptors.SIntSShort)
      testCastToBuffer(seq, DataBuffer[SInt, UShort](_), bytes)(Descriptors.SIntUShort)
      testCastToBuffer(seq, DataBuffer[SInt, SInt](_), bytes)(Descriptors.SIntSInt)
      testCastToBuffer(seq, DataBuffer[SInt, UInt](_), bytes)(Descriptors.SIntUInt)
      
      testCastToBuffer(seq, DataBuffer[Vec2i, SByte](_), bytes)(Descriptors.Vec2iSByte)
      testCastToBuffer(seq, DataBuffer[Vec2i, UByte](_), bytes)(Descriptors.Vec2iUByte)
      testCastToBuffer(seq, DataBuffer[Vec2i, SShort](_), bytes)(Descriptors.Vec2iSShort)
      testCastToBuffer(seq, DataBuffer[Vec2i, UShort](_), bytes)(Descriptors.Vec2iUShort)
      testCastToBuffer(seq, DataBuffer[Vec2i, SInt](_), bytes)(Descriptors.Vec2iSInt)
      testCastToBuffer(seq, DataBuffer[Vec2i, UInt](_), bytes)(Descriptors.Vec2iUInt)
      
      testCastToBuffer(seq, DataBuffer[Vec3i, SByte](_), bytes)(Descriptors.Vec3iSByte)
      testCastToBuffer(seq, DataBuffer[Vec3i, UByte](_), bytes)(Descriptors.Vec3iUByte)
      testCastToBuffer(seq, DataBuffer[Vec3i, SShort](_), bytes)(Descriptors.Vec3iSShort)
      testCastToBuffer(seq, DataBuffer[Vec3i, UShort](_), bytes)(Descriptors.Vec3iUShort)
      testCastToBuffer(seq, DataBuffer[Vec3i, SInt](_), bytes)(Descriptors.Vec3iSInt)
      testCastToBuffer(seq, DataBuffer[Vec3i, UInt](_), bytes)(Descriptors.Vec3iUInt)
      
      testCastToBuffer(seq, DataBuffer[Vec4i, SByte](_), bytes)(Descriptors.Vec4iSByte)
      testCastToBuffer(seq, DataBuffer[Vec4i, UByte](_), bytes)(Descriptors.Vec4iUByte)
      testCastToBuffer(seq, DataBuffer[Vec4i, SShort](_), bytes)(Descriptors.Vec4iSShort)
      testCastToBuffer(seq, DataBuffer[Vec4i, UShort](_), bytes)(Descriptors.Vec4iUShort)
      testCastToBuffer(seq, DataBuffer[Vec4i, SInt](_), bytes)(Descriptors.Vec4iSInt)
      testCastToBuffer(seq, DataBuffer[Vec4i, UInt](_), bytes)(Descriptors.Vec4iUInt)
      
      testCastToBuffer(seq, DataBuffer[RFloat, SByte](_), bytes)(Descriptors.RFloatSByte)
      testCastToBuffer(seq, DataBuffer[RFloat, UByte](_), bytes)(Descriptors.RFloatUByte)
      testCastToBuffer(seq, DataBuffer[RFloat, SShort](_), bytes)(Descriptors.RFloatSShort)
      testCastToBuffer(seq, DataBuffer[RFloat, UShort](_), bytes)(Descriptors.RFloatUShort)
      testCastToBuffer(seq, DataBuffer[RFloat, SInt](_), bytes)(Descriptors.RFloatSInt)
      testCastToBuffer(seq, DataBuffer[RFloat, UInt](_), bytes)(Descriptors.RFloatUInt)
      testCastToBuffer(seq, DataBuffer[RFloat, HFloat](_), bytes)(Descriptors.RFloatHFloat)
      testCastToBuffer(seq, DataBuffer[RFloat, RFloat](_), bytes)(Descriptors.RFloatRFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec2f, SByte](_), bytes)(Descriptors.Vec2fSByte)
      testCastToBuffer(seq, DataBuffer[Vec2f, UByte](_), bytes)(Descriptors.Vec2fUByte)
      testCastToBuffer(seq, DataBuffer[Vec2f, SShort](_), bytes)(Descriptors.Vec2fSShort)
      testCastToBuffer(seq, DataBuffer[Vec2f, UShort](_), bytes)(Descriptors.Vec2fUShort)
      testCastToBuffer(seq, DataBuffer[Vec2f, SInt](_), bytes)(Descriptors.Vec2fSInt)
      testCastToBuffer(seq, DataBuffer[Vec2f, UInt](_), bytes)(Descriptors.Vec2fUInt)
      testCastToBuffer(seq, DataBuffer[Vec2f, HFloat](_), bytes)(Descriptors.Vec2fHFloat)
      testCastToBuffer(seq, DataBuffer[Vec2f, RFloat](_), bytes)(Descriptors.Vec2fRFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec3f, SByte](_), bytes)(Descriptors.Vec3fSByte)
      testCastToBuffer(seq, DataBuffer[Vec3f, UByte](_), bytes)(Descriptors.Vec3fUByte)
      testCastToBuffer(seq, DataBuffer[Vec3f, SShort](_), bytes)(Descriptors.Vec3fSShort)
      testCastToBuffer(seq, DataBuffer[Vec3f, UShort](_), bytes)(Descriptors.Vec3fUShort)
      testCastToBuffer(seq, DataBuffer[Vec3f, SInt](_), bytes)(Descriptors.Vec3fSInt)
      testCastToBuffer(seq, DataBuffer[Vec3f, UInt](_), bytes)(Descriptors.Vec3fUInt)
      testCastToBuffer(seq, DataBuffer[Vec3f, HFloat](_), bytes)(Descriptors.Vec3fHFloat)
      testCastToBuffer(seq, DataBuffer[Vec3f, RFloat](_), bytes)(Descriptors.Vec3fRFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec4f, SByte](_), bytes)(Descriptors.Vec4fSByte)
      testCastToBuffer(seq, DataBuffer[Vec4f, UByte](_), bytes)(Descriptors.Vec4fUByte)
      testCastToBuffer(seq, DataBuffer[Vec4f, SShort](_), bytes)(Descriptors.Vec4fSShort)
      testCastToBuffer(seq, DataBuffer[Vec4f, UShort](_), bytes)(Descriptors.Vec4fUShort)
      testCastToBuffer(seq, DataBuffer[Vec4f, SInt](_), bytes)(Descriptors.Vec4fSInt)
      testCastToBuffer(seq, DataBuffer[Vec4f, UInt](_), bytes)(Descriptors.Vec4fUInt)
      testCastToBuffer(seq, DataBuffer[Vec4f, HFloat](_), bytes)(Descriptors.Vec4fHFloat)
      testCastToBuffer(seq, DataBuffer[Vec4f, RFloat](_), bytes)(Descriptors.Vec4fRFloat)
      
      testCastToBuffer(seq, DataBuffer[Mat3x2f, RFloat](_), bytes)(Descriptors.Mat3x2fRFloat)
      
      testCastToBuffer(seq, DataBuffer[RDouble, SByte](_), bytes)(Descriptors.RDoubleSByte)
      testCastToBuffer(seq, DataBuffer[RDouble, UByte](_), bytes)(Descriptors.RDoubleUByte)
      testCastToBuffer(seq, DataBuffer[RDouble, SShort](_), bytes)(Descriptors.RDoubleSShort)
      testCastToBuffer(seq, DataBuffer[RDouble, UShort](_), bytes)(Descriptors.RDoubleUShort)
      testCastToBuffer(seq, DataBuffer[RDouble, SInt](_), bytes)(Descriptors.RDoubleSInt)
      testCastToBuffer(seq, DataBuffer[RDouble, UInt](_), bytes)(Descriptors.RDoubleUInt)
      testCastToBuffer(seq, DataBuffer[RDouble, HFloat](_), bytes)(Descriptors.RDoubleHFloat)
      testCastToBuffer(seq, DataBuffer[RDouble, RFloat](_), bytes)(Descriptors.RDoubleRFloat)
      testCastToBuffer(seq, DataBuffer[RDouble, RDouble](_), bytes)(Descriptors.RDoubleRDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec2d, SByte](_), bytes)(Descriptors.Vec2dSByte)
      testCastToBuffer(seq, DataBuffer[Vec2d, UByte](_), bytes)(Descriptors.Vec2dUByte)
      testCastToBuffer(seq, DataBuffer[Vec2d, SShort](_), bytes)(Descriptors.Vec2dSShort)
      testCastToBuffer(seq, DataBuffer[Vec2d, UShort](_), bytes)(Descriptors.Vec2dUShort)
      testCastToBuffer(seq, DataBuffer[Vec2d, SInt](_), bytes)(Descriptors.Vec2dSInt)
      testCastToBuffer(seq, DataBuffer[Vec2d, UInt](_), bytes)(Descriptors.Vec2dUInt)
      testCastToBuffer(seq, DataBuffer[Vec2d, HFloat](_), bytes)(Descriptors.Vec2dHFloat)
      testCastToBuffer(seq, DataBuffer[Vec2d, RFloat](_), bytes)(Descriptors.Vec2dRFloat)
      testCastToBuffer(seq, DataBuffer[Vec2d, RDouble](_), bytes)(Descriptors.Vec2dRDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec3d, SByte](_), bytes)(Descriptors.Vec3dSByte)
      testCastToBuffer(seq, DataBuffer[Vec3d, UByte](_), bytes)(Descriptors.Vec3dUByte)
      testCastToBuffer(seq, DataBuffer[Vec3d, SShort](_), bytes)(Descriptors.Vec3dSShort)
      testCastToBuffer(seq, DataBuffer[Vec3d, UShort](_), bytes)(Descriptors.Vec3dUShort)
      testCastToBuffer(seq, DataBuffer[Vec3d, SInt](_), bytes)(Descriptors.Vec3dSInt)
      testCastToBuffer(seq, DataBuffer[Vec3d, UInt](_), bytes)(Descriptors.Vec3dUInt)
      testCastToBuffer(seq, DataBuffer[Vec3d, HFloat](_), bytes)(Descriptors.Vec3dHFloat)
      testCastToBuffer(seq, DataBuffer[Vec3d, RFloat](_), bytes)(Descriptors.Vec3dRFloat)
      testCastToBuffer(seq, DataBuffer[Vec3d, RDouble](_), bytes)(Descriptors.Vec3dRDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec4d, SByte](_), bytes)(Descriptors.Vec4dSByte)
      testCastToBuffer(seq, DataBuffer[Vec4d, UByte](_), bytes)(Descriptors.Vec4dUByte)
      testCastToBuffer(seq, DataBuffer[Vec4d, SShort](_), bytes)(Descriptors.Vec4dSShort)
      testCastToBuffer(seq, DataBuffer[Vec4d, UShort](_), bytes)(Descriptors.Vec4dUShort)
      testCastToBuffer(seq, DataBuffer[Vec4d, SInt](_), bytes)(Descriptors.Vec4dSInt)
      testCastToBuffer(seq, DataBuffer[Vec4d, UInt](_), bytes)(Descriptors.Vec4dUInt)
      testCastToBuffer(seq, DataBuffer[Vec4d, HFloat](_), bytes)(Descriptors.Vec4dHFloat)
      testCastToBuffer(seq, DataBuffer[Vec4d, RFloat](_), bytes)(Descriptors.Vec4dRFloat)
      testCastToBuffer(seq, DataBuffer[Vec4d, RDouble](_), bytes)(Descriptors.Vec4dRDouble)
      
      testCastToBuffer(seq, DataBuffer[Mat3x2d, RFloat](_), bytes)(Descriptors.Mat3x2dRFloat)
      testCastToBuffer(seq, DataBuffer[Mat3x2d, RDouble](_), bytes)(Descriptors.Mat3x2dRDouble)
      
      
      testCastToReadBuffer(seq, ReadDataBuffer[SInt, SByte](_), bytes)(Descriptors.SIntSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[SInt, UByte](_), bytes)(Descriptors.SIntUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[SInt, SShort](_), bytes)(Descriptors.SIntSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[SInt, UShort](_), bytes)(Descriptors.SIntUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[SInt, SInt](_), bytes)(Descriptors.SIntSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[SInt, UInt](_), bytes)(Descriptors.SIntUInt)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2i, SByte](_), bytes)(Descriptors.Vec2iSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2i, UByte](_), bytes)(Descriptors.Vec2iUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2i, SShort](_), bytes)(Descriptors.Vec2iSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2i, UShort](_), bytes)(Descriptors.Vec2iUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2i, SInt](_), bytes)(Descriptors.Vec2iSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2i, UInt](_), bytes)(Descriptors.Vec2iUInt)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3i, SByte](_), bytes)(Descriptors.Vec3iSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3i, UByte](_), bytes)(Descriptors.Vec3iUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3i, SShort](_), bytes)(Descriptors.Vec3iSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3i, UShort](_), bytes)(Descriptors.Vec3iUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3i, SInt](_), bytes)(Descriptors.Vec3iSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3i, UInt](_), bytes)(Descriptors.Vec3iUInt)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4i, SByte](_), bytes)(Descriptors.Vec4iSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4i, UByte](_), bytes)(Descriptors.Vec4iUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4i, SShort](_), bytes)(Descriptors.Vec4iSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4i, UShort](_), bytes)(Descriptors.Vec4iUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4i, SInt](_), bytes)(Descriptors.Vec4iSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4i, UInt](_), bytes)(Descriptors.Vec4iUInt)
      
      testCastToReadBuffer(seq, ReadDataBuffer[RFloat, SByte](_), bytes)(Descriptors.RFloatSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[RFloat, UByte](_), bytes)(Descriptors.RFloatUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[RFloat, SShort](_), bytes)(Descriptors.RFloatSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[RFloat, UShort](_), bytes)(Descriptors.RFloatUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[RFloat, SInt](_), bytes)(Descriptors.RFloatSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[RFloat, UInt](_), bytes)(Descriptors.RFloatUInt)
      testCastToReadBuffer(seq, ReadDataBuffer[RFloat, HFloat](_), bytes)(Descriptors.RFloatHFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[RFloat, RFloat](_), bytes)(Descriptors.RFloatRFloat)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2f, SByte](_), bytes)(Descriptors.Vec2fSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2f, UByte](_), bytes)(Descriptors.Vec2fUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2f, SShort](_), bytes)(Descriptors.Vec2fSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2f, UShort](_), bytes)(Descriptors.Vec2fUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2f, SInt](_), bytes)(Descriptors.Vec2fSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2f, UInt](_), bytes)(Descriptors.Vec2fUInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2f, HFloat](_), bytes)(Descriptors.Vec2fHFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2f, RFloat](_), bytes)(Descriptors.Vec2fRFloat)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3f, SByte](_), bytes)(Descriptors.Vec3fSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3f, UByte](_), bytes)(Descriptors.Vec3fUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3f, SShort](_), bytes)(Descriptors.Vec3fSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3f, UShort](_), bytes)(Descriptors.Vec3fUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3f, SInt](_), bytes)(Descriptors.Vec3fSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3f, UInt](_), bytes)(Descriptors.Vec3fUInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3f, HFloat](_), bytes)(Descriptors.Vec3fHFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3f, RFloat](_), bytes)(Descriptors.Vec3fRFloat)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4f, SByte](_), bytes)(Descriptors.Vec4fSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4f, UByte](_), bytes)(Descriptors.Vec4fUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4f, SShort](_), bytes)(Descriptors.Vec4fSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4f, UShort](_), bytes)(Descriptors.Vec4fUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4f, SInt](_), bytes)(Descriptors.Vec4fSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4f, UInt](_), bytes)(Descriptors.Vec4fUInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4f, HFloat](_), bytes)(Descriptors.Vec4fHFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4f, RFloat](_), bytes)(Descriptors.Vec4fRFloat)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Mat3x2f, RFloat](_), bytes)(Descriptors.Mat3x2fRFloat)
      
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, SByte](_), bytes)(Descriptors.RDoubleSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, UByte](_), bytes)(Descriptors.RDoubleUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, SShort](_), bytes)(Descriptors.RDoubleSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, UShort](_), bytes)(Descriptors.RDoubleUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, SInt](_), bytes)(Descriptors.RDoubleSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, UInt](_), bytes)(Descriptors.RDoubleUInt)
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, HFloat](_), bytes)(Descriptors.RDoubleHFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, RFloat](_), bytes)(Descriptors.RDoubleRFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[RDouble, RDouble](_), bytes)(Descriptors.RDoubleRDouble)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, SByte](_), bytes)(Descriptors.Vec2dSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, UByte](_), bytes)(Descriptors.Vec2dUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, SShort](_), bytes)(Descriptors.Vec2dSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, UShort](_), bytes)(Descriptors.Vec2dUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, SInt](_), bytes)(Descriptors.Vec2dSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, UInt](_), bytes)(Descriptors.Vec2dUInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, HFloat](_), bytes)(Descriptors.Vec2dHFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, RFloat](_), bytes)(Descriptors.Vec2dRFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec2d, RDouble](_), bytes)(Descriptors.Vec2dRDouble)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, SByte](_), bytes)(Descriptors.Vec3dSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, UByte](_), bytes)(Descriptors.Vec3dUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, SShort](_), bytes)(Descriptors.Vec3dSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, UShort](_), bytes)(Descriptors.Vec3dUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, SInt](_), bytes)(Descriptors.Vec3dSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, UInt](_), bytes)(Descriptors.Vec3dUInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, HFloat](_), bytes)(Descriptors.Vec3dHFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, RFloat](_), bytes)(Descriptors.Vec3dRFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec3d, RDouble](_), bytes)(Descriptors.Vec3dRDouble)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, SByte](_), bytes)(Descriptors.Vec4dSByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, UByte](_), bytes)(Descriptors.Vec4dUByte)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, SShort](_), bytes)(Descriptors.Vec4dSShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, UShort](_), bytes)(Descriptors.Vec4dUShort)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, SInt](_), bytes)(Descriptors.Vec4dSInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, UInt](_), bytes)(Descriptors.Vec4dUInt)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, HFloat](_), bytes)(Descriptors.Vec4dHFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, RFloat](_), bytes)(Descriptors.Vec4dRFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Vec4d, RDouble](_), bytes)(Descriptors.Vec4dRDouble)
      
      testCastToReadBuffer(seq, ReadDataBuffer[Mat3x2d, RFloat](_), bytes)(Descriptors.Mat3x2dRFloat)
      testCastToReadBuffer(seq, ReadDataBuffer[Mat3x2d, RDouble](_), bytes)(Descriptors.Mat3x2dRDouble)
    }
    
    for (size <- 0 to 1; extraBytes <- 0 to 8) {
      val (bytes, _) = genRandomBuffer(size*8*4*2 + extraBytes, Descriptors.SIntSByte)
      val seq = factory(bytes)

      
      testCastToView(seq, DataView[SInt, SByte](_, _, _), bytes)(Descriptors.SIntSByte)
      testCastToView(seq, DataView[SInt, UByte](_, _, _), bytes)(Descriptors.SIntUByte)
      testCastToView(seq, DataView[SInt, SShort](_, _, _), bytes)(Descriptors.SIntSShort)
      testCastToView(seq, DataView[SInt, UShort](_, _, _), bytes)(Descriptors.SIntUShort)
      testCastToView(seq, DataView[SInt, SInt](_, _, _), bytes)(Descriptors.SIntSInt)
      testCastToView(seq, DataView[SInt, UInt](_, _, _), bytes)(Descriptors.SIntUInt)
      
      testCastToView(seq, DataView[Vec2i, SByte](_, _, _), bytes)(Descriptors.Vec2iSByte)
      testCastToView(seq, DataView[Vec2i, UByte](_, _, _), bytes)(Descriptors.Vec2iUByte)
      testCastToView(seq, DataView[Vec2i, SShort](_, _, _), bytes)(Descriptors.Vec2iSShort)
      testCastToView(seq, DataView[Vec2i, UShort](_, _, _), bytes)(Descriptors.Vec2iUShort)
      testCastToView(seq, DataView[Vec2i, SInt](_, _, _), bytes)(Descriptors.Vec2iSInt)
      testCastToView(seq, DataView[Vec2i, UInt](_, _, _), bytes)(Descriptors.Vec2iUInt)
      
      testCastToView(seq, DataView[Vec3i, SByte](_, _, _), bytes)(Descriptors.Vec3iSByte)
      testCastToView(seq, DataView[Vec3i, UByte](_, _, _), bytes)(Descriptors.Vec3iUByte)
      testCastToView(seq, DataView[Vec3i, SShort](_, _, _), bytes)(Descriptors.Vec3iSShort)
      testCastToView(seq, DataView[Vec3i, UShort](_, _, _), bytes)(Descriptors.Vec3iUShort)
      testCastToView(seq, DataView[Vec3i, SInt](_, _, _), bytes)(Descriptors.Vec3iSInt)
      testCastToView(seq, DataView[Vec3i, UInt](_, _, _), bytes)(Descriptors.Vec3iUInt)
      
      testCastToView(seq, DataView[Vec4i, SByte](_, _, _), bytes)(Descriptors.Vec4iSByte)
      testCastToView(seq, DataView[Vec4i, UByte](_, _, _), bytes)(Descriptors.Vec4iUByte)
      testCastToView(seq, DataView[Vec4i, SShort](_, _, _), bytes)(Descriptors.Vec4iSShort)
      testCastToView(seq, DataView[Vec4i, UShort](_, _, _), bytes)(Descriptors.Vec4iUShort)
      testCastToView(seq, DataView[Vec4i, SInt](_, _, _), bytes)(Descriptors.Vec4iSInt)
      testCastToView(seq, DataView[Vec4i, UInt](_, _, _), bytes)(Descriptors.Vec4iUInt)
      
      testCastToView(seq, DataView[RFloat, SByte](_, _, _), bytes)(Descriptors.RFloatSByte)
      testCastToView(seq, DataView[RFloat, UByte](_, _, _), bytes)(Descriptors.RFloatUByte)
      testCastToView(seq, DataView[RFloat, SShort](_, _, _), bytes)(Descriptors.RFloatSShort)
      testCastToView(seq, DataView[RFloat, UShort](_, _, _), bytes)(Descriptors.RFloatUShort)
      testCastToView(seq, DataView[RFloat, SInt](_, _, _), bytes)(Descriptors.RFloatSInt)
      testCastToView(seq, DataView[RFloat, UInt](_, _, _), bytes)(Descriptors.RFloatUInt)
      testCastToView(seq, DataView[RFloat, HFloat](_, _, _), bytes)(Descriptors.RFloatHFloat)
      testCastToView(seq, DataView[RFloat, RFloat](_, _, _), bytes)(Descriptors.RFloatRFloat)
      
      testCastToView(seq, DataView[Vec2f, SByte](_, _, _), bytes)(Descriptors.Vec2fSByte)
      testCastToView(seq, DataView[Vec2f, UByte](_, _, _), bytes)(Descriptors.Vec2fUByte)
      testCastToView(seq, DataView[Vec2f, SShort](_, _, _), bytes)(Descriptors.Vec2fSShort)
      testCastToView(seq, DataView[Vec2f, UShort](_, _, _), bytes)(Descriptors.Vec2fUShort)
      testCastToView(seq, DataView[Vec2f, SInt](_, _, _), bytes)(Descriptors.Vec2fSInt)
      testCastToView(seq, DataView[Vec2f, UInt](_, _, _), bytes)(Descriptors.Vec2fUInt)
      testCastToView(seq, DataView[Vec2f, HFloat](_, _, _), bytes)(Descriptors.Vec2fHFloat)
      testCastToView(seq, DataView[Vec2f, RFloat](_, _, _), bytes)(Descriptors.Vec2fRFloat)
      
      testCastToView(seq, DataView[Vec3f, SByte](_, _, _), bytes)(Descriptors.Vec3fSByte)
      testCastToView(seq, DataView[Vec3f, UByte](_, _, _), bytes)(Descriptors.Vec3fUByte)
      testCastToView(seq, DataView[Vec3f, SShort](_, _, _), bytes)(Descriptors.Vec3fSShort)
      testCastToView(seq, DataView[Vec3f, UShort](_, _, _), bytes)(Descriptors.Vec3fUShort)
      testCastToView(seq, DataView[Vec3f, SInt](_, _, _), bytes)(Descriptors.Vec3fSInt)
      testCastToView(seq, DataView[Vec3f, UInt](_, _, _), bytes)(Descriptors.Vec3fUInt)
      testCastToView(seq, DataView[Vec3f, HFloat](_, _, _), bytes)(Descriptors.Vec3fHFloat)
      testCastToView(seq, DataView[Vec3f, RFloat](_, _, _), bytes)(Descriptors.Vec3fRFloat)
      
      testCastToView(seq, DataView[Vec4f, SByte](_, _, _), bytes)(Descriptors.Vec4fSByte)
      testCastToView(seq, DataView[Vec4f, UByte](_, _, _), bytes)(Descriptors.Vec4fUByte)
      testCastToView(seq, DataView[Vec4f, SShort](_, _, _), bytes)(Descriptors.Vec4fSShort)
      testCastToView(seq, DataView[Vec4f, UShort](_, _, _), bytes)(Descriptors.Vec4fUShort)
      testCastToView(seq, DataView[Vec4f, SInt](_, _, _), bytes)(Descriptors.Vec4fSInt)
      testCastToView(seq, DataView[Vec4f, UInt](_, _, _), bytes)(Descriptors.Vec4fUInt)
      testCastToView(seq, DataView[Vec4f, HFloat](_, _, _), bytes)(Descriptors.Vec4fHFloat)
      testCastToView(seq, DataView[Vec4f, RFloat](_, _, _), bytes)(Descriptors.Vec4fRFloat)
      
      testCastToView(seq, DataView[RDouble, SByte](_, _, _), bytes)(Descriptors.RDoubleSByte)
      testCastToView(seq, DataView[RDouble, UByte](_, _, _), bytes)(Descriptors.RDoubleUByte)
      testCastToView(seq, DataView[RDouble, SShort](_, _, _), bytes)(Descriptors.RDoubleSShort)
      testCastToView(seq, DataView[RDouble, UShort](_, _, _), bytes)(Descriptors.RDoubleUShort)
      testCastToView(seq, DataView[RDouble, SInt](_, _, _), bytes)(Descriptors.RDoubleSInt)
      testCastToView(seq, DataView[RDouble, UInt](_, _, _), bytes)(Descriptors.RDoubleUInt)
      testCastToView(seq, DataView[RDouble, HFloat](_, _, _), bytes)(Descriptors.RDoubleHFloat)
      testCastToView(seq, DataView[RDouble, RFloat](_, _, _), bytes)(Descriptors.RDoubleRFloat)
      testCastToView(seq, DataView[RDouble, RDouble](_, _, _), bytes)(Descriptors.RDoubleRDouble)
      
      testCastToView(seq, DataView[Vec2d, SByte](_, _, _), bytes)(Descriptors.Vec2dSByte)
      testCastToView(seq, DataView[Vec2d, UByte](_, _, _), bytes)(Descriptors.Vec2dUByte)
      testCastToView(seq, DataView[Vec2d, SShort](_, _, _), bytes)(Descriptors.Vec2dSShort)
      testCastToView(seq, DataView[Vec2d, UShort](_, _, _), bytes)(Descriptors.Vec2dUShort)
      testCastToView(seq, DataView[Vec2d, SInt](_, _, _), bytes)(Descriptors.Vec2dSInt)
      testCastToView(seq, DataView[Vec2d, UInt](_, _, _), bytes)(Descriptors.Vec2dUInt)
      testCastToView(seq, DataView[Vec2d, HFloat](_, _, _), bytes)(Descriptors.Vec2dHFloat)
      testCastToView(seq, DataView[Vec2d, RFloat](_, _, _), bytes)(Descriptors.Vec2dRFloat)
      testCastToView(seq, DataView[Vec2d, RDouble](_, _, _), bytes)(Descriptors.Vec2dRDouble)
      
      testCastToView(seq, DataView[Vec3d, SByte](_, _, _), bytes)(Descriptors.Vec3dSByte)
      testCastToView(seq, DataView[Vec3d, UByte](_, _, _), bytes)(Descriptors.Vec3dUByte)
      testCastToView(seq, DataView[Vec3d, SShort](_, _, _), bytes)(Descriptors.Vec3dSShort)
      testCastToView(seq, DataView[Vec3d, UShort](_, _, _), bytes)(Descriptors.Vec3dUShort)
      testCastToView(seq, DataView[Vec3d, SInt](_, _, _), bytes)(Descriptors.Vec3dSInt)
      testCastToView(seq, DataView[Vec3d, UInt](_, _, _), bytes)(Descriptors.Vec3dUInt)
      testCastToView(seq, DataView[Vec3d, HFloat](_, _, _), bytes)(Descriptors.Vec3dHFloat)
      testCastToView(seq, DataView[Vec3d, RFloat](_, _, _), bytes)(Descriptors.Vec3dRFloat)
      testCastToView(seq, DataView[Vec3d, RDouble](_, _, _), bytes)(Descriptors.Vec3dRDouble)
      
      testCastToView(seq, DataView[Vec4d, SByte](_, _, _), bytes)(Descriptors.Vec4dSByte)
      testCastToView(seq, DataView[Vec4d, UByte](_, _, _), bytes)(Descriptors.Vec4dUByte)
      testCastToView(seq, DataView[Vec4d, SShort](_, _, _), bytes)(Descriptors.Vec4dSShort)
      testCastToView(seq, DataView[Vec4d, UShort](_, _, _), bytes)(Descriptors.Vec4dUShort)
      testCastToView(seq, DataView[Vec4d, SInt](_, _, _), bytes)(Descriptors.Vec4dSInt)
      testCastToView(seq, DataView[Vec4d, UInt](_, _, _), bytes)(Descriptors.Vec4dUInt)
      testCastToView(seq, DataView[Vec4d, HFloat](_, _, _), bytes)(Descriptors.Vec4dHFloat)
      testCastToView(seq, DataView[Vec4d, RFloat](_, _, _), bytes)(Descriptors.Vec4dRFloat)
      testCastToView(seq, DataView[Vec4d, RDouble](_, _, _), bytes)(Descriptors.Vec4dRDouble)

      
      testCastToReadView(seq, ReadDataView[SInt, SByte](_, _, _), bytes)(Descriptors.SIntSByte)
      testCastToReadView(seq, ReadDataView[SInt, UByte](_, _, _), bytes)(Descriptors.SIntUByte)
      testCastToReadView(seq, ReadDataView[SInt, SShort](_, _, _), bytes)(Descriptors.SIntSShort)
      testCastToReadView(seq, ReadDataView[SInt, UShort](_, _, _), bytes)(Descriptors.SIntUShort)
      testCastToReadView(seq, ReadDataView[SInt, SInt](_, _, _), bytes)(Descriptors.SIntSInt)
      testCastToReadView(seq, ReadDataView[SInt, UInt](_, _, _), bytes)(Descriptors.SIntUInt)
      
      testCastToReadView(seq, ReadDataView[Vec2i, SByte](_, _, _), bytes)(Descriptors.Vec2iSByte)
      testCastToReadView(seq, ReadDataView[Vec2i, UByte](_, _, _), bytes)(Descriptors.Vec2iUByte)
      testCastToReadView(seq, ReadDataView[Vec2i, SShort](_, _, _), bytes)(Descriptors.Vec2iSShort)
      testCastToReadView(seq, ReadDataView[Vec2i, UShort](_, _, _), bytes)(Descriptors.Vec2iUShort)
      testCastToReadView(seq, ReadDataView[Vec2i, SInt](_, _, _), bytes)(Descriptors.Vec2iSInt)
      testCastToReadView(seq, ReadDataView[Vec2i, UInt](_, _, _), bytes)(Descriptors.Vec2iUInt)
      
      testCastToReadView(seq, ReadDataView[Vec3i, SByte](_, _, _), bytes)(Descriptors.Vec3iSByte)
      testCastToReadView(seq, ReadDataView[Vec3i, UByte](_, _, _), bytes)(Descriptors.Vec3iUByte)
      testCastToReadView(seq, ReadDataView[Vec3i, SShort](_, _, _), bytes)(Descriptors.Vec3iSShort)
      testCastToReadView(seq, ReadDataView[Vec3i, UShort](_, _, _), bytes)(Descriptors.Vec3iUShort)
      testCastToReadView(seq, ReadDataView[Vec3i, SInt](_, _, _), bytes)(Descriptors.Vec3iSInt)
      testCastToReadView(seq, ReadDataView[Vec3i, UInt](_, _, _), bytes)(Descriptors.Vec3iUInt)
      
      testCastToReadView(seq, ReadDataView[Vec4i, SByte](_, _, _), bytes)(Descriptors.Vec4iSByte)
      testCastToReadView(seq, ReadDataView[Vec4i, UByte](_, _, _), bytes)(Descriptors.Vec4iUByte)
      testCastToReadView(seq, ReadDataView[Vec4i, SShort](_, _, _), bytes)(Descriptors.Vec4iSShort)
      testCastToReadView(seq, ReadDataView[Vec4i, UShort](_, _, _), bytes)(Descriptors.Vec4iUShort)
      testCastToReadView(seq, ReadDataView[Vec4i, SInt](_, _, _), bytes)(Descriptors.Vec4iSInt)
      testCastToReadView(seq, ReadDataView[Vec4i, UInt](_, _, _), bytes)(Descriptors.Vec4iUInt)
      
      testCastToReadView(seq, ReadDataView[RFloat, SByte](_, _, _), bytes)(Descriptors.RFloatSByte)
      testCastToReadView(seq, ReadDataView[RFloat, UByte](_, _, _), bytes)(Descriptors.RFloatUByte)
      testCastToReadView(seq, ReadDataView[RFloat, SShort](_, _, _), bytes)(Descriptors.RFloatSShort)
      testCastToReadView(seq, ReadDataView[RFloat, UShort](_, _, _), bytes)(Descriptors.RFloatUShort)
      testCastToReadView(seq, ReadDataView[RFloat, SInt](_, _, _), bytes)(Descriptors.RFloatSInt)
      testCastToReadView(seq, ReadDataView[RFloat, UInt](_, _, _), bytes)(Descriptors.RFloatUInt)
      testCastToReadView(seq, ReadDataView[RFloat, HFloat](_, _, _), bytes)(Descriptors.RFloatHFloat)
      testCastToReadView(seq, ReadDataView[RFloat, RFloat](_, _, _), bytes)(Descriptors.RFloatRFloat)
      
      testCastToReadView(seq, ReadDataView[Vec2f, SByte](_, _, _), bytes)(Descriptors.Vec2fSByte)
      testCastToReadView(seq, ReadDataView[Vec2f, UByte](_, _, _), bytes)(Descriptors.Vec2fUByte)
      testCastToReadView(seq, ReadDataView[Vec2f, SShort](_, _, _), bytes)(Descriptors.Vec2fSShort)
      testCastToReadView(seq, ReadDataView[Vec2f, UShort](_, _, _), bytes)(Descriptors.Vec2fUShort)
      testCastToReadView(seq, ReadDataView[Vec2f, SInt](_, _, _), bytes)(Descriptors.Vec2fSInt)
      testCastToReadView(seq, ReadDataView[Vec2f, UInt](_, _, _), bytes)(Descriptors.Vec2fUInt)
      testCastToReadView(seq, ReadDataView[Vec2f, HFloat](_, _, _), bytes)(Descriptors.Vec2fHFloat)
      testCastToReadView(seq, ReadDataView[Vec2f, RFloat](_, _, _), bytes)(Descriptors.Vec2fRFloat)
      
      testCastToReadView(seq, ReadDataView[Vec3f, SByte](_, _, _), bytes)(Descriptors.Vec3fSByte)
      testCastToReadView(seq, ReadDataView[Vec3f, UByte](_, _, _), bytes)(Descriptors.Vec3fUByte)
      testCastToReadView(seq, ReadDataView[Vec3f, SShort](_, _, _), bytes)(Descriptors.Vec3fSShort)
      testCastToReadView(seq, ReadDataView[Vec3f, UShort](_, _, _), bytes)(Descriptors.Vec3fUShort)
      testCastToReadView(seq, ReadDataView[Vec3f, SInt](_, _, _), bytes)(Descriptors.Vec3fSInt)
      testCastToReadView(seq, ReadDataView[Vec3f, UInt](_, _, _), bytes)(Descriptors.Vec3fUInt)
      testCastToReadView(seq, ReadDataView[Vec3f, HFloat](_, _, _), bytes)(Descriptors.Vec3fHFloat)
      testCastToReadView(seq, ReadDataView[Vec3f, RFloat](_, _, _), bytes)(Descriptors.Vec3fRFloat)
      
      testCastToReadView(seq, ReadDataView[Vec4f, SByte](_, _, _), bytes)(Descriptors.Vec4fSByte)
      testCastToReadView(seq, ReadDataView[Vec4f, UByte](_, _, _), bytes)(Descriptors.Vec4fUByte)
      testCastToReadView(seq, ReadDataView[Vec4f, SShort](_, _, _), bytes)(Descriptors.Vec4fSShort)
      testCastToReadView(seq, ReadDataView[Vec4f, UShort](_, _, _), bytes)(Descriptors.Vec4fUShort)
      testCastToReadView(seq, ReadDataView[Vec4f, SInt](_, _, _), bytes)(Descriptors.Vec4fSInt)
      testCastToReadView(seq, ReadDataView[Vec4f, UInt](_, _, _), bytes)(Descriptors.Vec4fUInt)
      testCastToReadView(seq, ReadDataView[Vec4f, HFloat](_, _, _), bytes)(Descriptors.Vec4fHFloat)
      testCastToReadView(seq, ReadDataView[Vec4f, RFloat](_, _, _), bytes)(Descriptors.Vec4fRFloat)
      
      testCastToReadView(seq, ReadDataView[RDouble, SByte](_, _, _), bytes)(Descriptors.RDoubleSByte)
      testCastToReadView(seq, ReadDataView[RDouble, UByte](_, _, _), bytes)(Descriptors.RDoubleUByte)
      testCastToReadView(seq, ReadDataView[RDouble, SShort](_, _, _), bytes)(Descriptors.RDoubleSShort)
      testCastToReadView(seq, ReadDataView[RDouble, UShort](_, _, _), bytes)(Descriptors.RDoubleUShort)
      testCastToReadView(seq, ReadDataView[RDouble, SInt](_, _, _), bytes)(Descriptors.RDoubleSInt)
      testCastToReadView(seq, ReadDataView[RDouble, UInt](_, _, _), bytes)(Descriptors.RDoubleUInt)
      testCastToReadView(seq, ReadDataView[RDouble, HFloat](_, _, _), bytes)(Descriptors.RDoubleHFloat)
      testCastToReadView(seq, ReadDataView[RDouble, RFloat](_, _, _), bytes)(Descriptors.RDoubleRFloat)
      testCastToReadView(seq, ReadDataView[RDouble, RDouble](_, _, _), bytes)(Descriptors.RDoubleRDouble)
      
      testCastToReadView(seq, ReadDataView[Vec2d, SByte](_, _, _), bytes)(Descriptors.Vec2dSByte)
      testCastToReadView(seq, ReadDataView[Vec2d, UByte](_, _, _), bytes)(Descriptors.Vec2dUByte)
      testCastToReadView(seq, ReadDataView[Vec2d, SShort](_, _, _), bytes)(Descriptors.Vec2dSShort)
      testCastToReadView(seq, ReadDataView[Vec2d, UShort](_, _, _), bytes)(Descriptors.Vec2dUShort)
      testCastToReadView(seq, ReadDataView[Vec2d, SInt](_, _, _), bytes)(Descriptors.Vec2dSInt)
      testCastToReadView(seq, ReadDataView[Vec2d, UInt](_, _, _), bytes)(Descriptors.Vec2dUInt)
      testCastToReadView(seq, ReadDataView[Vec2d, HFloat](_, _, _), bytes)(Descriptors.Vec2dHFloat)
      testCastToReadView(seq, ReadDataView[Vec2d, RFloat](_, _, _), bytes)(Descriptors.Vec2dRFloat)
      testCastToReadView(seq, ReadDataView[Vec2d, RDouble](_, _, _), bytes)(Descriptors.Vec2dRDouble)
      
      testCastToReadView(seq, ReadDataView[Vec3d, SByte](_, _, _), bytes)(Descriptors.Vec3dSByte)
      testCastToReadView(seq, ReadDataView[Vec3d, UByte](_, _, _), bytes)(Descriptors.Vec3dUByte)
      testCastToReadView(seq, ReadDataView[Vec3d, SShort](_, _, _), bytes)(Descriptors.Vec3dSShort)
      testCastToReadView(seq, ReadDataView[Vec3d, UShort](_, _, _), bytes)(Descriptors.Vec3dUShort)
      testCastToReadView(seq, ReadDataView[Vec3d, SInt](_, _, _), bytes)(Descriptors.Vec3dSInt)
      testCastToReadView(seq, ReadDataView[Vec3d, UInt](_, _, _), bytes)(Descriptors.Vec3dUInt)
      testCastToReadView(seq, ReadDataView[Vec3d, HFloat](_, _, _), bytes)(Descriptors.Vec3dHFloat)
      testCastToReadView(seq, ReadDataView[Vec3d, RFloat](_, _, _), bytes)(Descriptors.Vec3dRFloat)
      testCastToReadView(seq, ReadDataView[Vec3d, RDouble](_, _, _), bytes)(Descriptors.Vec3dRDouble)
      
      testCastToReadView(seq, ReadDataView[Vec4d, SByte](_, _, _), bytes)(Descriptors.Vec4dSByte)
      testCastToReadView(seq, ReadDataView[Vec4d, UByte](_, _, _), bytes)(Descriptors.Vec4dUByte)
      testCastToReadView(seq, ReadDataView[Vec4d, SShort](_, _, _), bytes)(Descriptors.Vec4dSShort)
      testCastToReadView(seq, ReadDataView[Vec4d, UShort](_, _, _), bytes)(Descriptors.Vec4dUShort)
      testCastToReadView(seq, ReadDataView[Vec4d, SInt](_, _, _), bytes)(Descriptors.Vec4dSInt)
      testCastToReadView(seq, ReadDataView[Vec4d, UInt](_, _, _), bytes)(Descriptors.Vec4dUInt)
      testCastToReadView(seq, ReadDataView[Vec4d, HFloat](_, _, _), bytes)(Descriptors.Vec4dHFloat)
      testCastToReadView(seq, ReadDataView[Vec4d, RFloat](_, _, _), bytes)(Descriptors.Vec4dRFloat)
      testCastToReadView(seq, ReadDataView[Vec4d, RDouble](_, _, _), bytes)(Descriptors.Vec4dRDouble)
    }
    
    if (descriptor.rawType == RawType.SByte) {
      for (size <- 0 to 1; extraBytes <- 0 to 8) {
        val (bytes, _) = genRandomBuffer(size*8*4*2 + extraBytes, Descriptors.SIntSByte)
        val seq = factory(bytes).asInstanceOf[Contiguous[_, SByte]]
        
        val contigTest = Contiguous[SInt, SByte](seq).asInstanceOf[DataBuffer[SInt, SByte]]
        testBuffer(contigTest, false, bytes)(Descriptors.SIntSByte)
        assert(seq.sharesStorageWith(contigTest))
        assert(contigTest.sharesStorageWith(seq))
        
        val ro = seq.asReadOnly().asInstanceOf[ReadContiguous[_, SByte]]
        intercept[IllegalArgumentException] { Contiguous[SInt, SByte](ro.asInstanceOf[Contiguous[_, SByte]]) }
        
        val readContigTest = ReadContiguous[SInt, SByte](ro).asInstanceOf[ReadDataBuffer[SInt, SByte]]
        testBuffer(readContigTest, true, bytes)(Descriptors.SIntSByte)
        assert(seq.sharesStorageWith(readContigTest))
        assert(readContigTest.sharesStorageWith(seq))
      }
    }
  }

  private def testCastToBuffer[F <: Format, R <: Raw](
    original: DataBuffer[_, _],
    factory: (DataBuffer[_, _]) => DataBuffer[F, R],
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[F, R]) {
    val cast = factory(original)
    testBuffer(cast, false, wrap(bytes, descriptor))(descriptor)
    assert(original.sharesStorageWith(cast))
    assert(cast.sharesStorageWith(original))
    
    intercept[IllegalArgumentException] { factory(original.asReadOnly().asInstanceOf[DataBuffer[_, _]]) }
  }
  
  private def testCastToReadBuffer[F <: Format, R <: Raw](
    original: ReadDataBuffer[_, _],
    factory: (ReadDataBuffer[_, _]) => ReadDataBuffer[F, R],
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[F, R]) {
    assert(!original.isReadOnly)
    
    {
      val cast = factory(original)
      testBuffer(cast, false, wrap(bytes, descriptor))(descriptor)
      assert(original.sharesStorageWith(cast))
      assert(cast.sharesStorageWith(original))
    }
    
    {
      val cast = factory(original.asReadOnly())
      testBuffer(cast, true, wrap(bytes, descriptor))(descriptor)
      assert(original.sharesStorageWith(cast))
      assert(cast.sharesStorageWith(original))
    }
  }

  private def testCastToView[F <: Format, R <: Raw](
    original: DataBuffer[_, _],
    factory: (DataBuffer[_, _], Int, Int) => DataView[F, R],
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[F, R]) {
    val data = wrap(bytes, descriptor)

    for (
      stride <- descriptor.components to (descriptor.components + 4);
      offset <- 0 to min(stride - descriptor.components, data.limit)
    ) {
      val cast = factory(original, offset, stride)
      testView(cast, offset, stride, false, data)(descriptor)
      assert(original.sharesStorageWith(cast))
      assert(cast.sharesStorageWith(original))
    }

    intercept[IllegalArgumentException] { factory(original.asReadOnly().asInstanceOf[DataBuffer[_, _]], 0, 1) }
  }

  private def testCastToReadView[F <: Format, R <: Raw](
    original: ReadDataBuffer[_, Raw],
    factory: (ReadDataBuffer[_, _], Int, Int) => ReadDataView[F, R],
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[F, R]) {
    assert(!original.isReadOnly)
    val data = wrap(bytes, descriptor)
    val ro = original.asReadOnly()

    for (
      stride <- descriptor.components to (descriptor.components + 4);
      offset <- 0 to min(stride - descriptor.components, data.limit)
    ) {
      {
        val cast = factory(original, offset, stride)
        testView(cast, offset, stride, false, data)(descriptor)
        assert(original.sharesStorageWith(cast))
        assert(cast.sharesStorageWith(original))
      }

      {
        val cast = factory(ro, offset, stride)
        testView(cast, offset, stride, true, data)(descriptor)
        assert(original.sharesStorageWith(cast))
        assert(cast.sharesStorageWith(original))
      }
    }
  }

}
