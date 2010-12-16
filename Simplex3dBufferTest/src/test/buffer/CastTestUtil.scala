/*
 * Simplex3d, BufferTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBufferTest.
 *
 * Simplex3dBufferTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBufferTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.buffer

import java.nio._
import scala.reflect._
import org.scalatest._
import simplex3d.math._
import simplex3d.math.floatm._
import simplex3d.math.doublem._
import simplex3d.math.CoreMath._
import simplex3d.buffer._
import simplex3d.buffer.RawType._
import simplex3d.buffer.floatm._
import simplex3d.buffer.doublem._

import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object CastTestUtil extends FunSuite {

  def testArrayCast[E <: Meta, R <: Raw](
    factory: (R#Array) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
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

  private def testCastToArray[E <: Meta, R <: Raw](
    original: ReadDataArray[_, _],
    cast: ReadDataArray[E, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    testArray(cast, readOnly, data)(descriptor)
    assert(original.sharesStoreObject(cast))
    assert(cast.sharesStoreObject(original))
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
  }
  
  private def testUByteArrayCast(da: DataArray[_, UByte]) {
    val data = da.buffer()
    
    testCastToArray(da, DataArray[SInt, UByte](da), false, data)(Descriptors.SIntUByte)
    testCastToArray(da, DataArray[Vec2i, UByte](da), false, data)(Descriptors.Vec2iUByte)
    testCastToArray(da, DataArray[Vec3i, UByte](da), false, data)(Descriptors.Vec3iUByte)
    testCastToArray(da, DataArray[Vec4i, UByte](da), false, data)(Descriptors.Vec4iUByte)

    testCastToArray(da, IndexArray[UByte](da), false, data)(Descriptors.SIntUByte)

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

    testCastToArray(da, ReadIndexArray[UByte](da), false, data)(Descriptors.SIntUByte)

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
    
    intercept[IllegalArgumentException] { IndexArray[UByte](roCast) }
    
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

    testCastToArray(ro, ReadIndexArray[UByte](ro), true, data)(Descriptors.SIntUByte)

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

    testCastToArray(da, IndexArray[UShort](da), false, data)(Descriptors.SIntUShort)

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

    testCastToArray(da, ReadIndexArray[UShort](da), false, data)(Descriptors.SIntUShort)

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

    intercept[IllegalArgumentException] { IndexArray[UShort](roCast) }

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

    testCastToArray(ro, ReadIndexArray[UShort](ro), true, data)(Descriptors.SIntUShort)

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

    testCastToArray(da, IndexArray[UInt](da), false, data)(Descriptors.SIntUInt)

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

    testCastToArray(da, ReadIndexArray[UInt](da), false, data)(Descriptors.SIntUInt)

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

    intercept[IllegalArgumentException] { IndexArray[UInt](roCast) }

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

    testCastToArray(ro, ReadIndexArray[UInt](ro), true, data)(Descriptors.SIntUInt)

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

    testCastToArray(da, DataArray[RDouble, RFloat](da), false, data)(Descriptors.RDoubleRFloat)
    testCastToArray(da, DataArray[Vec2d, RFloat](da), false, data)(Descriptors.Vec2dRFloat)
    testCastToArray(da, DataArray[Vec3d, RFloat](da), false, data)(Descriptors.Vec3dRFloat)
    testCastToArray(da, DataArray[Vec4d, RFloat](da), false, data)(Descriptors.Vec4dRFloat)


    testCastToArray(da, ReadDataArray[RFloat, RFloat](da), false, data)(Descriptors.RFloatRFloat)
    testCastToArray(da, ReadDataArray[Vec2f, RFloat](da), false, data)(Descriptors.Vec2fRFloat)
    testCastToArray(da, ReadDataArray[Vec3f, RFloat](da), false, data)(Descriptors.Vec3fRFloat)
    testCastToArray(da, ReadDataArray[Vec4f, RFloat](da), false, data)(Descriptors.Vec4fRFloat)

    testCastToArray(da, ReadDataArray[RDouble, RFloat](da), false, data)(Descriptors.RDoubleRFloat)
    testCastToArray(da, ReadDataArray[Vec2d, RFloat](da), false, data)(Descriptors.Vec2dRFloat)
    testCastToArray(da, ReadDataArray[Vec3d, RFloat](da), false, data)(Descriptors.Vec3dRFloat)
    testCastToArray(da, ReadDataArray[Vec4d, RFloat](da), false, data)(Descriptors.Vec4dRFloat)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, RFloat]]
    intercept[IllegalArgumentException] { DataArray[RFloat, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, RFloat](roCast) }

    intercept[IllegalArgumentException] { DataArray[RDouble, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, RFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, RFloat](roCast) }


    testCastToArray(ro, ReadDataArray[RFloat, RFloat](ro), true, data)(Descriptors.RFloatRFloat)
    testCastToArray(ro, ReadDataArray[Vec2f, RFloat](ro), true, data)(Descriptors.Vec2fRFloat)
    testCastToArray(ro, ReadDataArray[Vec3f, RFloat](ro), true, data)(Descriptors.Vec3fRFloat)
    testCastToArray(ro, ReadDataArray[Vec4f, RFloat](ro), true, data)(Descriptors.Vec4fRFloat)

    testCastToArray(ro, ReadDataArray[RDouble, RFloat](ro), true, data)(Descriptors.RDoubleRFloat)
    testCastToArray(ro, ReadDataArray[Vec2d, RFloat](ro), true, data)(Descriptors.Vec2dRFloat)
    testCastToArray(ro, ReadDataArray[Vec3d, RFloat](ro), true, data)(Descriptors.Vec3dRFloat)
    testCastToArray(ro, ReadDataArray[Vec4d, RFloat](ro), true, data)(Descriptors.Vec4dRFloat)
  }
  
  private def testRDoubleArrayCast(da: DataArray[_, RDouble]) {
    val data = da.buffer()

    testCastToArray(da, DataArray[RDouble, RDouble](da), false, data)(Descriptors.RDoubleRDouble)
    testCastToArray(da, DataArray[Vec2d, RDouble](da), false, data)(Descriptors.Vec2dRDouble)
    testCastToArray(da, DataArray[Vec3d, RDouble](da), false, data)(Descriptors.Vec3dRDouble)
    testCastToArray(da, DataArray[Vec4d, RDouble](da), false, data)(Descriptors.Vec4dRDouble)


    testCastToArray(da, ReadDataArray[RDouble, RDouble](da), false, data)(Descriptors.RDoubleRDouble)
    testCastToArray(da, ReadDataArray[Vec2d, RDouble](da), false, data)(Descriptors.Vec2dRDouble)
    testCastToArray(da, ReadDataArray[Vec3d, RDouble](da), false, data)(Descriptors.Vec3dRDouble)
    testCastToArray(da, ReadDataArray[Vec4d, RDouble](da), false, data)(Descriptors.Vec4dRDouble)


    val ro = da.asReadOnly()
    val roCast = ro.asInstanceOf[DataArray[_, RDouble]]
    intercept[IllegalArgumentException] { DataArray[RDouble, RDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, RDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, RDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, RDouble](roCast) }


    testCastToArray(ro, ReadDataArray[RDouble, RDouble](ro), true, data)(Descriptors.RDoubleRDouble)
    testCastToArray(ro, ReadDataArray[Vec2d, RDouble](ro), true, data)(Descriptors.Vec2dRDouble)
    testCastToArray(ro, ReadDataArray[Vec3d, RDouble](ro), true, data)(Descriptors.Vec3dRDouble)
    testCastToArray(ro, ReadDataArray[Vec4d, RDouble](ro), true, data)(Descriptors.Vec4dRDouble)
  }


  def testBufferCast[E <: Meta, R <: Raw](
    factory: (ByteBuffer) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {

    for (size <- 0 to 1; extraBytes <- 0 to 8) {
      val (bytes, _) = genRandomBuffer(size*8*4*2 + extraBytes, Descriptors.SIntSByte)
      val seq = factory(bytes)


      testCastToBuffer(seq, IndexBuffer[UByte](seq), false, bytes)(Descriptors.SIntUByte)
      testCastToBuffer(seq, IndexBuffer[UShort](seq), false, bytes)(Descriptors.SIntUShort)
      testCastToBuffer(seq, IndexBuffer[UInt](seq), false, bytes)(Descriptors.SIntUInt)
      
      testCastToBuffer(seq, DataBuffer[SInt, SByte](seq), false, bytes)(Descriptors.SIntSByte)
      testCastToBuffer(seq, DataBuffer[SInt, UByte](seq), false, bytes)(Descriptors.SIntUByte)
      testCastToBuffer(seq, DataBuffer[SInt, SShort](seq), false, bytes)(Descriptors.SIntSShort)
      testCastToBuffer(seq, DataBuffer[SInt, UShort](seq), false, bytes)(Descriptors.SIntUShort)
      testCastToBuffer(seq, DataBuffer[SInt, SInt](seq), false, bytes)(Descriptors.SIntSInt)
      testCastToBuffer(seq, DataBuffer[SInt, UInt](seq), false, bytes)(Descriptors.SIntUInt)
      
      testCastToBuffer(seq, DataBuffer[Vec2i, SByte](seq), false, bytes)(Descriptors.Vec2iSByte)
      testCastToBuffer(seq, DataBuffer[Vec2i, UByte](seq), false, bytes)(Descriptors.Vec2iUByte)
      testCastToBuffer(seq, DataBuffer[Vec2i, SShort](seq), false, bytes)(Descriptors.Vec2iSShort)
      testCastToBuffer(seq, DataBuffer[Vec2i, UShort](seq), false, bytes)(Descriptors.Vec2iUShort)
      testCastToBuffer(seq, DataBuffer[Vec2i, SInt](seq), false, bytes)(Descriptors.Vec2iSInt)
      testCastToBuffer(seq, DataBuffer[Vec2i, UInt](seq), false, bytes)(Descriptors.Vec2iUInt)
      
      testCastToBuffer(seq, DataBuffer[Vec3i, SByte](seq), false, bytes)(Descriptors.Vec3iSByte)
      testCastToBuffer(seq, DataBuffer[Vec3i, UByte](seq), false, bytes)(Descriptors.Vec3iUByte)
      testCastToBuffer(seq, DataBuffer[Vec3i, SShort](seq), false, bytes)(Descriptors.Vec3iSShort)
      testCastToBuffer(seq, DataBuffer[Vec3i, UShort](seq), false, bytes)(Descriptors.Vec3iUShort)
      testCastToBuffer(seq, DataBuffer[Vec3i, SInt](seq), false, bytes)(Descriptors.Vec3iSInt)
      testCastToBuffer(seq, DataBuffer[Vec3i, UInt](seq), false, bytes)(Descriptors.Vec3iUInt)
      
      testCastToBuffer(seq, DataBuffer[Vec4i, SByte](seq), false, bytes)(Descriptors.Vec4iSByte)
      testCastToBuffer(seq, DataBuffer[Vec4i, UByte](seq), false, bytes)(Descriptors.Vec4iUByte)
      testCastToBuffer(seq, DataBuffer[Vec4i, SShort](seq), false, bytes)(Descriptors.Vec4iSShort)
      testCastToBuffer(seq, DataBuffer[Vec4i, UShort](seq), false, bytes)(Descriptors.Vec4iUShort)
      testCastToBuffer(seq, DataBuffer[Vec4i, SInt](seq), false, bytes)(Descriptors.Vec4iSInt)
      testCastToBuffer(seq, DataBuffer[Vec4i, UInt](seq), false, bytes)(Descriptors.Vec4iUInt)
      
      testCastToBuffer(seq, DataBuffer[RFloat, SByte](seq), false, bytes)(Descriptors.RFloatSByte)
      testCastToBuffer(seq, DataBuffer[RFloat, UByte](seq), false, bytes)(Descriptors.RFloatUByte)
      testCastToBuffer(seq, DataBuffer[RFloat, SShort](seq), false, bytes)(Descriptors.RFloatSShort)
      testCastToBuffer(seq, DataBuffer[RFloat, UShort](seq), false, bytes)(Descriptors.RFloatUShort)
      testCastToBuffer(seq, DataBuffer[RFloat, SInt](seq), false, bytes)(Descriptors.RFloatSInt)
      testCastToBuffer(seq, DataBuffer[RFloat, UInt](seq), false, bytes)(Descriptors.RFloatUInt)
      testCastToBuffer(seq, DataBuffer[RFloat, HFloat](seq), false, bytes)(Descriptors.RFloatHFloat)
      testCastToBuffer(seq, DataBuffer[RFloat, RFloat](seq), false, bytes)(Descriptors.RFloatRFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec2f, SByte](seq), false, bytes)(Descriptors.Vec2fSByte)
      testCastToBuffer(seq, DataBuffer[Vec2f, UByte](seq), false, bytes)(Descriptors.Vec2fUByte)
      testCastToBuffer(seq, DataBuffer[Vec2f, SShort](seq), false, bytes)(Descriptors.Vec2fSShort)
      testCastToBuffer(seq, DataBuffer[Vec2f, UShort](seq), false, bytes)(Descriptors.Vec2fUShort)
      testCastToBuffer(seq, DataBuffer[Vec2f, SInt](seq), false, bytes)(Descriptors.Vec2fSInt)
      testCastToBuffer(seq, DataBuffer[Vec2f, UInt](seq), false, bytes)(Descriptors.Vec2fUInt)
      testCastToBuffer(seq, DataBuffer[Vec2f, HFloat](seq), false, bytes)(Descriptors.Vec2fHFloat)
      testCastToBuffer(seq, DataBuffer[Vec2f, RFloat](seq), false, bytes)(Descriptors.Vec2fRFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec3f, SByte](seq), false, bytes)(Descriptors.Vec3fSByte)
      testCastToBuffer(seq, DataBuffer[Vec3f, UByte](seq), false, bytes)(Descriptors.Vec3fUByte)
      testCastToBuffer(seq, DataBuffer[Vec3f, SShort](seq), false, bytes)(Descriptors.Vec3fSShort)
      testCastToBuffer(seq, DataBuffer[Vec3f, UShort](seq), false, bytes)(Descriptors.Vec3fUShort)
      testCastToBuffer(seq, DataBuffer[Vec3f, SInt](seq), false, bytes)(Descriptors.Vec3fSInt)
      testCastToBuffer(seq, DataBuffer[Vec3f, UInt](seq), false, bytes)(Descriptors.Vec3fUInt)
      testCastToBuffer(seq, DataBuffer[Vec3f, HFloat](seq), false, bytes)(Descriptors.Vec3fHFloat)
      testCastToBuffer(seq, DataBuffer[Vec3f, RFloat](seq), false, bytes)(Descriptors.Vec3fRFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec4f, SByte](seq), false, bytes)(Descriptors.Vec4fSByte)
      testCastToBuffer(seq, DataBuffer[Vec4f, UByte](seq), false, bytes)(Descriptors.Vec4fUByte)
      testCastToBuffer(seq, DataBuffer[Vec4f, SShort](seq), false, bytes)(Descriptors.Vec4fSShort)
      testCastToBuffer(seq, DataBuffer[Vec4f, UShort](seq), false, bytes)(Descriptors.Vec4fUShort)
      testCastToBuffer(seq, DataBuffer[Vec4f, SInt](seq), false, bytes)(Descriptors.Vec4fSInt)
      testCastToBuffer(seq, DataBuffer[Vec4f, UInt](seq), false, bytes)(Descriptors.Vec4fUInt)
      testCastToBuffer(seq, DataBuffer[Vec4f, HFloat](seq), false, bytes)(Descriptors.Vec4fHFloat)
      testCastToBuffer(seq, DataBuffer[Vec4f, RFloat](seq), false, bytes)(Descriptors.Vec4fRFloat)
      
      testCastToBuffer(seq, DataBuffer[RDouble, SByte](seq), false, bytes)(Descriptors.RDoubleSByte)
      testCastToBuffer(seq, DataBuffer[RDouble, UByte](seq), false, bytes)(Descriptors.RDoubleUByte)
      testCastToBuffer(seq, DataBuffer[RDouble, SShort](seq), false, bytes)(Descriptors.RDoubleSShort)
      testCastToBuffer(seq, DataBuffer[RDouble, UShort](seq), false, bytes)(Descriptors.RDoubleUShort)
      testCastToBuffer(seq, DataBuffer[RDouble, SInt](seq), false, bytes)(Descriptors.RDoubleSInt)
      testCastToBuffer(seq, DataBuffer[RDouble, UInt](seq), false, bytes)(Descriptors.RDoubleUInt)
      testCastToBuffer(seq, DataBuffer[RDouble, HFloat](seq), false, bytes)(Descriptors.RDoubleHFloat)
      testCastToBuffer(seq, DataBuffer[RDouble, RFloat](seq), false, bytes)(Descriptors.RDoubleRFloat)
      testCastToBuffer(seq, DataBuffer[RDouble, RDouble](seq), false, bytes)(Descriptors.RDoubleRDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec2d, SByte](seq), false, bytes)(Descriptors.Vec2dSByte)
      testCastToBuffer(seq, DataBuffer[Vec2d, UByte](seq), false, bytes)(Descriptors.Vec2dUByte)
      testCastToBuffer(seq, DataBuffer[Vec2d, SShort](seq), false, bytes)(Descriptors.Vec2dSShort)
      testCastToBuffer(seq, DataBuffer[Vec2d, UShort](seq), false, bytes)(Descriptors.Vec2dUShort)
      testCastToBuffer(seq, DataBuffer[Vec2d, SInt](seq), false, bytes)(Descriptors.Vec2dSInt)
      testCastToBuffer(seq, DataBuffer[Vec2d, UInt](seq), false, bytes)(Descriptors.Vec2dUInt)
      testCastToBuffer(seq, DataBuffer[Vec2d, HFloat](seq), false, bytes)(Descriptors.Vec2dHFloat)
      testCastToBuffer(seq, DataBuffer[Vec2d, RFloat](seq), false, bytes)(Descriptors.Vec2dRFloat)
      testCastToBuffer(seq, DataBuffer[Vec2d, RDouble](seq), false, bytes)(Descriptors.Vec2dRDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec3d, SByte](seq), false, bytes)(Descriptors.Vec3dSByte)
      testCastToBuffer(seq, DataBuffer[Vec3d, UByte](seq), false, bytes)(Descriptors.Vec3dUByte)
      testCastToBuffer(seq, DataBuffer[Vec3d, SShort](seq), false, bytes)(Descriptors.Vec3dSShort)
      testCastToBuffer(seq, DataBuffer[Vec3d, UShort](seq), false, bytes)(Descriptors.Vec3dUShort)
      testCastToBuffer(seq, DataBuffer[Vec3d, SInt](seq), false, bytes)(Descriptors.Vec3dSInt)
      testCastToBuffer(seq, DataBuffer[Vec3d, UInt](seq), false, bytes)(Descriptors.Vec3dUInt)
      testCastToBuffer(seq, DataBuffer[Vec3d, HFloat](seq), false, bytes)(Descriptors.Vec3dHFloat)
      testCastToBuffer(seq, DataBuffer[Vec3d, RFloat](seq), false, bytes)(Descriptors.Vec3dRFloat)
      testCastToBuffer(seq, DataBuffer[Vec3d, RDouble](seq), false, bytes)(Descriptors.Vec3dRDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec4d, SByte](seq), false, bytes)(Descriptors.Vec4dSByte)
      testCastToBuffer(seq, DataBuffer[Vec4d, UByte](seq), false, bytes)(Descriptors.Vec4dUByte)
      testCastToBuffer(seq, DataBuffer[Vec4d, SShort](seq), false, bytes)(Descriptors.Vec4dSShort)
      testCastToBuffer(seq, DataBuffer[Vec4d, UShort](seq), false, bytes)(Descriptors.Vec4dUShort)
      testCastToBuffer(seq, DataBuffer[Vec4d, SInt](seq), false, bytes)(Descriptors.Vec4dSInt)
      testCastToBuffer(seq, DataBuffer[Vec4d, UInt](seq), false, bytes)(Descriptors.Vec4dUInt)
      testCastToBuffer(seq, DataBuffer[Vec4d, HFloat](seq), false, bytes)(Descriptors.Vec4dHFloat)
      testCastToBuffer(seq, DataBuffer[Vec4d, RFloat](seq), false, bytes)(Descriptors.Vec4dRFloat)
      testCastToBuffer(seq, DataBuffer[Vec4d, RDouble](seq), false, bytes)(Descriptors.Vec4dRDouble)
      
      
      testCastToBuffer(seq, ReadIndexBuffer[UByte](seq), false, bytes)(Descriptors.SIntUByte)
      testCastToBuffer(seq, ReadIndexBuffer[UShort](seq), false, bytes)(Descriptors.SIntUShort)
      testCastToBuffer(seq, ReadIndexBuffer[UInt](seq), false, bytes)(Descriptors.SIntUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[SInt, SByte](seq), false, bytes)(Descriptors.SIntSByte)
      testCastToBuffer(seq, ReadDataBuffer[SInt, UByte](seq), false, bytes)(Descriptors.SIntUByte)
      testCastToBuffer(seq, ReadDataBuffer[SInt, SShort](seq), false, bytes)(Descriptors.SIntSShort)
      testCastToBuffer(seq, ReadDataBuffer[SInt, UShort](seq), false, bytes)(Descriptors.SIntUShort)
      testCastToBuffer(seq, ReadDataBuffer[SInt, SInt](seq), false, bytes)(Descriptors.SIntSInt)
      testCastToBuffer(seq, ReadDataBuffer[SInt, UInt](seq), false, bytes)(Descriptors.SIntUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, SByte](seq), false, bytes)(Descriptors.Vec2iSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, UByte](seq), false, bytes)(Descriptors.Vec2iUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, SShort](seq), false, bytes)(Descriptors.Vec2iSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, UShort](seq), false, bytes)(Descriptors.Vec2iUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, SInt](seq), false, bytes)(Descriptors.Vec2iSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, UInt](seq), false, bytes)(Descriptors.Vec2iUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, SByte](seq), false, bytes)(Descriptors.Vec3iSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, UByte](seq), false, bytes)(Descriptors.Vec3iUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, SShort](seq), false, bytes)(Descriptors.Vec3iSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, UShort](seq), false, bytes)(Descriptors.Vec3iUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, SInt](seq), false, bytes)(Descriptors.Vec3iSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, UInt](seq), false, bytes)(Descriptors.Vec3iUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, SByte](seq), false, bytes)(Descriptors.Vec4iSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, UByte](seq), false, bytes)(Descriptors.Vec4iUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, SShort](seq), false, bytes)(Descriptors.Vec4iSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, UShort](seq), false, bytes)(Descriptors.Vec4iUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, SInt](seq), false, bytes)(Descriptors.Vec4iSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, UInt](seq), false, bytes)(Descriptors.Vec4iUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[RFloat, SByte](seq), false, bytes)(Descriptors.RFloatSByte)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, UByte](seq), false, bytes)(Descriptors.RFloatUByte)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, SShort](seq), false, bytes)(Descriptors.RFloatSShort)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, UShort](seq), false, bytes)(Descriptors.RFloatUShort)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, SInt](seq), false, bytes)(Descriptors.RFloatSInt)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, UInt](seq), false, bytes)(Descriptors.RFloatUInt)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, HFloat](seq), false, bytes)(Descriptors.RFloatHFloat)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, RFloat](seq), false, bytes)(Descriptors.RFloatRFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SByte](seq), false, bytes)(Descriptors.Vec2fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UByte](seq), false, bytes)(Descriptors.Vec2fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SShort](seq), false, bytes)(Descriptors.Vec2fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UShort](seq), false, bytes)(Descriptors.Vec2fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SInt](seq), false, bytes)(Descriptors.Vec2fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UInt](seq), false, bytes)(Descriptors.Vec2fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, HFloat](seq), false, bytes)(Descriptors.Vec2fHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, RFloat](seq), false, bytes)(Descriptors.Vec2fRFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SByte](seq), false, bytes)(Descriptors.Vec3fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UByte](seq), false, bytes)(Descriptors.Vec3fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SShort](seq), false, bytes)(Descriptors.Vec3fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UShort](seq), false, bytes)(Descriptors.Vec3fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SInt](seq), false, bytes)(Descriptors.Vec3fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UInt](seq), false, bytes)(Descriptors.Vec3fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, HFloat](seq), false, bytes)(Descriptors.Vec3fHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, RFloat](seq), false, bytes)(Descriptors.Vec3fRFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SByte](seq), false, bytes)(Descriptors.Vec4fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UByte](seq), false, bytes)(Descriptors.Vec4fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SShort](seq), false, bytes)(Descriptors.Vec4fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UShort](seq), false, bytes)(Descriptors.Vec4fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SInt](seq), false, bytes)(Descriptors.Vec4fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UInt](seq), false, bytes)(Descriptors.Vec4fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, HFloat](seq), false, bytes)(Descriptors.Vec4fHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, RFloat](seq), false, bytes)(Descriptors.Vec4fRFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[RDouble, SByte](seq), false, bytes)(Descriptors.RDoubleSByte)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, UByte](seq), false, bytes)(Descriptors.RDoubleUByte)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, SShort](seq), false, bytes)(Descriptors.RDoubleSShort)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, UShort](seq), false, bytes)(Descriptors.RDoubleUShort)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, SInt](seq), false, bytes)(Descriptors.RDoubleSInt)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, UInt](seq), false, bytes)(Descriptors.RDoubleUInt)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, HFloat](seq), false, bytes)(Descriptors.RDoubleHFloat)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, RFloat](seq), false, bytes)(Descriptors.RDoubleRFloat)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, RDouble](seq), false, bytes)(Descriptors.RDoubleRDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SByte](seq), false, bytes)(Descriptors.Vec2dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UByte](seq), false, bytes)(Descriptors.Vec2dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SShort](seq), false, bytes)(Descriptors.Vec2dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UShort](seq), false, bytes)(Descriptors.Vec2dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SInt](seq), false, bytes)(Descriptors.Vec2dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UInt](seq), false, bytes)(Descriptors.Vec2dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, HFloat](seq), false, bytes)(Descriptors.Vec2dHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, RFloat](seq), false, bytes)(Descriptors.Vec2dRFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, RDouble](seq), false, bytes)(Descriptors.Vec2dRDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SByte](seq), false, bytes)(Descriptors.Vec3dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UByte](seq), false, bytes)(Descriptors.Vec3dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SShort](seq), false, bytes)(Descriptors.Vec3dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UShort](seq), false, bytes)(Descriptors.Vec3dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SInt](seq), false, bytes)(Descriptors.Vec3dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UInt](seq), false, bytes)(Descriptors.Vec3dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, HFloat](seq), false, bytes)(Descriptors.Vec3dHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, RFloat](seq), false, bytes)(Descriptors.Vec3dRFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, RDouble](seq), false, bytes)(Descriptors.Vec3dRDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SByte](seq), false, bytes)(Descriptors.Vec4dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UByte](seq), false, bytes)(Descriptors.Vec4dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SShort](seq), false, bytes)(Descriptors.Vec4dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UShort](seq), false, bytes)(Descriptors.Vec4dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SInt](seq), false, bytes)(Descriptors.Vec4dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UInt](seq), false, bytes)(Descriptors.Vec4dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, HFloat](seq), false, bytes)(Descriptors.Vec4dHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, RFloat](seq), false, bytes)(Descriptors.Vec4dRFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, RDouble](seq), false, bytes)(Descriptors.Vec4dRDouble)
      
      
      val ro = seq.asReadOnly()
      val roCast = ro.asInstanceOf[DataBuffer[_, Raw]]
      
      intercept[IllegalArgumentException] { IndexBuffer[UByte](roCast) }
      intercept[IllegalArgumentException] { IndexBuffer[UShort](roCast) }
      intercept[IllegalArgumentException] { IndexBuffer[UInt](roCast) }
      
      intercept[IllegalArgumentException] { DataBuffer[SInt, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[SInt, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[SInt, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[SInt, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[SInt, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[SInt, UInt](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec2i, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2i, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2i, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2i, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2i, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2i, UInt](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec3i, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3i, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3i, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3i, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3i, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3i, UInt](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec4i, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4i, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4i, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4i, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4i, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4i, UInt](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[RFloat, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RFloat, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RFloat, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RFloat, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RFloat, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RFloat, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RFloat, HFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RFloat, RFloat](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec2f, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, HFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, RFloat](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec3f, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, HFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, RFloat](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec4f, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, HFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, RFloat](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[RDouble, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RDouble, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RDouble, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RDouble, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RDouble, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RDouble, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RDouble, HFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RDouble, RFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[RDouble, RDouble](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec2d, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, HFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, RFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, RDouble](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec3d, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, HFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, RFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, RDouble](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec4d, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, HFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, RFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, RDouble](roCast) }


      testCastToBuffer(seq, ReadIndexBuffer[UByte](ro), true, bytes)(Descriptors.SIntUByte)
      testCastToBuffer(seq, ReadIndexBuffer[UShort](ro), true, bytes)(Descriptors.SIntUShort)
      testCastToBuffer(seq, ReadIndexBuffer[UInt](ro), true, bytes)(Descriptors.SIntUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[SInt, SByte](ro), true, bytes)(Descriptors.SIntSByte)
      testCastToBuffer(seq, ReadDataBuffer[SInt, UByte](ro), true, bytes)(Descriptors.SIntUByte)
      testCastToBuffer(seq, ReadDataBuffer[SInt, SShort](ro), true, bytes)(Descriptors.SIntSShort)
      testCastToBuffer(seq, ReadDataBuffer[SInt, UShort](ro), true, bytes)(Descriptors.SIntUShort)
      testCastToBuffer(seq, ReadDataBuffer[SInt, SInt](ro), true, bytes)(Descriptors.SIntSInt)
      testCastToBuffer(seq, ReadDataBuffer[SInt, UInt](ro), true, bytes)(Descriptors.SIntUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, SByte](ro), true, bytes)(Descriptors.Vec2iSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, UByte](ro), true, bytes)(Descriptors.Vec2iUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, SShort](ro), true, bytes)(Descriptors.Vec2iSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, UShort](ro), true, bytes)(Descriptors.Vec2iUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, SInt](ro), true, bytes)(Descriptors.Vec2iSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2i, UInt](ro), true, bytes)(Descriptors.Vec2iUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, SByte](ro), true, bytes)(Descriptors.Vec3iSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, UByte](ro), true, bytes)(Descriptors.Vec3iUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, SShort](ro), true, bytes)(Descriptors.Vec3iSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, UShort](ro), true, bytes)(Descriptors.Vec3iUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, SInt](ro), true, bytes)(Descriptors.Vec3iSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3i, UInt](ro), true, bytes)(Descriptors.Vec3iUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, SByte](ro), true, bytes)(Descriptors.Vec4iSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, UByte](ro), true, bytes)(Descriptors.Vec4iUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, SShort](ro), true, bytes)(Descriptors.Vec4iSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, UShort](ro), true, bytes)(Descriptors.Vec4iUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, SInt](ro), true, bytes)(Descriptors.Vec4iSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4i, UInt](ro), true, bytes)(Descriptors.Vec4iUInt)
      
      testCastToBuffer(seq, ReadDataBuffer[RFloat, SByte](ro), true, bytes)(Descriptors.RFloatSByte)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, UByte](ro), true, bytes)(Descriptors.RFloatUByte)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, SShort](ro), true, bytes)(Descriptors.RFloatSShort)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, UShort](ro), true, bytes)(Descriptors.RFloatUShort)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, SInt](ro), true, bytes)(Descriptors.RFloatSInt)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, UInt](ro), true, bytes)(Descriptors.RFloatUInt)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, HFloat](ro), true, bytes)(Descriptors.RFloatHFloat)
      testCastToBuffer(seq, ReadDataBuffer[RFloat, RFloat](ro), true, bytes)(Descriptors.RFloatRFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SByte](ro), true, bytes)(Descriptors.Vec2fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UByte](ro), true, bytes)(Descriptors.Vec2fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SShort](ro), true, bytes)(Descriptors.Vec2fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UShort](ro), true, bytes)(Descriptors.Vec2fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SInt](ro), true, bytes)(Descriptors.Vec2fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UInt](ro), true, bytes)(Descriptors.Vec2fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, HFloat](ro), true, bytes)(Descriptors.Vec2fHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, RFloat](ro), true, bytes)(Descriptors.Vec2fRFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SByte](ro), true, bytes)(Descriptors.Vec3fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UByte](ro), true, bytes)(Descriptors.Vec3fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SShort](ro), true, bytes)(Descriptors.Vec3fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UShort](ro), true, bytes)(Descriptors.Vec3fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SInt](ro), true, bytes)(Descriptors.Vec3fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UInt](ro), true, bytes)(Descriptors.Vec3fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, HFloat](ro), true, bytes)(Descriptors.Vec3fHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, RFloat](ro), true, bytes)(Descriptors.Vec3fRFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SByte](ro), true, bytes)(Descriptors.Vec4fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UByte](ro), true, bytes)(Descriptors.Vec4fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SShort](ro), true, bytes)(Descriptors.Vec4fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UShort](ro), true, bytes)(Descriptors.Vec4fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SInt](ro), true, bytes)(Descriptors.Vec4fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UInt](ro), true, bytes)(Descriptors.Vec4fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, HFloat](ro), true, bytes)(Descriptors.Vec4fHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, RFloat](ro), true, bytes)(Descriptors.Vec4fRFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[RDouble, SByte](ro), true, bytes)(Descriptors.RDoubleSByte)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, UByte](ro), true, bytes)(Descriptors.RDoubleUByte)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, SShort](ro), true, bytes)(Descriptors.RDoubleSShort)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, UShort](ro), true, bytes)(Descriptors.RDoubleUShort)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, SInt](ro), true, bytes)(Descriptors.RDoubleSInt)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, UInt](ro), true, bytes)(Descriptors.RDoubleUInt)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, HFloat](ro), true, bytes)(Descriptors.RDoubleHFloat)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, RFloat](ro), true, bytes)(Descriptors.RDoubleRFloat)
      testCastToBuffer(seq, ReadDataBuffer[RDouble, RDouble](ro), true, bytes)(Descriptors.RDoubleRDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SByte](ro), true, bytes)(Descriptors.Vec2dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UByte](ro), true, bytes)(Descriptors.Vec2dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SShort](ro), true, bytes)(Descriptors.Vec2dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UShort](ro), true, bytes)(Descriptors.Vec2dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SInt](ro), true, bytes)(Descriptors.Vec2dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UInt](ro), true, bytes)(Descriptors.Vec2dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, HFloat](ro), true, bytes)(Descriptors.Vec2dHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, RFloat](ro), true, bytes)(Descriptors.Vec2dRFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, RDouble](ro), true, bytes)(Descriptors.Vec2dRDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SByte](ro), true, bytes)(Descriptors.Vec3dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UByte](ro), true, bytes)(Descriptors.Vec3dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SShort](ro), true, bytes)(Descriptors.Vec3dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UShort](ro), true, bytes)(Descriptors.Vec3dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SInt](ro), true, bytes)(Descriptors.Vec3dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UInt](ro), true, bytes)(Descriptors.Vec3dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, HFloat](ro), true, bytes)(Descriptors.Vec3dHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, RFloat](ro), true, bytes)(Descriptors.Vec3dRFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, RDouble](ro), true, bytes)(Descriptors.Vec3dRDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SByte](ro), true, bytes)(Descriptors.Vec4dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UByte](ro), true, bytes)(Descriptors.Vec4dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SShort](ro), true, bytes)(Descriptors.Vec4dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UShort](ro), true, bytes)(Descriptors.Vec4dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SInt](ro), true, bytes)(Descriptors.Vec4dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UInt](ro), true, bytes)(Descriptors.Vec4dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, HFloat](ro), true, bytes)(Descriptors.Vec4dHFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, RFloat](ro), true, bytes)(Descriptors.Vec4dRFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, RDouble](ro), true, bytes)(Descriptors.Vec4dRDouble)
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
  }

  private def testCastToBuffer[E <: Meta, R <: Raw](
    original: ReadDataBuffer[_, _],
    cast: ReadDataBuffer[E, R],
    readOnly: Boolean,
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[E, R]) {
    testBuffer(cast, readOnly, wrap(bytes, descriptor))(descriptor)
    assert(original.sharesStoreObject(cast))
    assert(cast.sharesStoreObject(original))
  }

  private def testCastToView[E <: Meta, R <: Raw](
    original: DataBuffer[_, _],
    factory: (DataBuffer[_, _], Int, Int) => DataView[E, R],
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[E, R]) {
    val data = wrap(bytes, descriptor)

    for (
      stride <- descriptor.components to (descriptor.components + 4);
      offset <- 0 to min(stride - descriptor.components, data.limit)
    ) {
      val cast = factory(original, offset, stride)
      testView(cast, offset, stride, false, data)(descriptor)
      assert(original.sharesStoreObject(cast))
      assert(cast.sharesStoreObject(original))
    }

    intercept[IllegalArgumentException] { factory(original.asReadOnly().asInstanceOf[DataBuffer[_, _]], 0, 1) }
  }

  private def testCastToReadView[E <: Meta, R <: Raw](
    original: ReadDataBuffer[_, Raw],
    factory: (ReadDataBuffer[_, _], Int, Int) => ReadDataView[E, R],
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(!original.readOnly)
    val data = wrap(bytes, descriptor)
    val ro = original.asReadOnly()

    for (
      stride <- descriptor.components to (descriptor.components + 4);
      offset <- 0 to min(stride - descriptor.components, data.limit)
    ) {
      {
        val cast = factory(original, offset, stride)
        testView(cast, offset, stride, false, data)(descriptor)
        assert(original.sharesStoreObject(cast))
        assert(cast.sharesStoreObject(original))
      }

      {
        val cast = factory(ro, offset, stride)
        testView(cast, offset, stride, true, data)(descriptor)
        assert(original.sharesStoreObject(cast))
        assert(cast.sharesStoreObject(original))
      }
    }
  }
}
