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
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem._
import simplex3d.buffer._
import simplex3d.buffer.RawType._
import simplex3d.buffer.intm._
import simplex3d.buffer.floatm._
import simplex3d.buffer.doublem._

import TestUtil._
import AttributeTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
object CastTest extends FunSuite {

  def testArrayCast[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
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
        case HalfFloat => testHalfFloatArrayCast(seq.asInstanceOf[DataArray[_, HalfFloat]])
        case RawFloat => testRawFloatArrayCast(seq.asInstanceOf[DataArray[_, RawFloat]])
        case RawDouble => testRawDoubleArrayCast(seq.asInstanceOf[DataArray[_, RawDouble]])
      }
    }
  }

  private def testCastToArray[E <: MetaElement, R <: RawData](
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
    val data = da.asBuffer()

    testCastToArray(da, DataArray[Int1, SByte](da), false, data)(Descriptors.Int1SByte)
    testCastToArray(da, DataArray[Vec2i, SByte](da), false, data)(Descriptors.Vec2iSByte)
    testCastToArray(da, DataArray[Vec3i, SByte](da), false, data)(Descriptors.Vec3iSByte)
    testCastToArray(da, DataArray[Vec4i, SByte](da), false, data)(Descriptors.Vec4iSByte)

    testCastToArray(da, DataArray[Float1, SByte](da), false, data)(Descriptors.Float1SByte)
    testCastToArray(da, DataArray[Vec2f, SByte](da), false, data)(Descriptors.Vec2fSByte)
    testCastToArray(da, DataArray[Vec3f, SByte](da), false, data)(Descriptors.Vec3fSByte)
    testCastToArray(da, DataArray[Vec4f, SByte](da), false, data)(Descriptors.Vec4fSByte)
    
    testCastToArray(da, DataArray[Double1, SByte](da), false, data)(Descriptors.Double1SByte)
    testCastToArray(da, DataArray[Vec2d, SByte](da), false, data)(Descriptors.Vec2dSByte)
    testCastToArray(da, DataArray[Vec3d, SByte](da), false, data)(Descriptors.Vec3dSByte)
    testCastToArray(da, DataArray[Vec4d, SByte](da), false, data)(Descriptors.Vec4dSByte)

    
    testCastToArray(da, ReadDataArray[Int1, SByte](da), false, data)(Descriptors.Int1SByte)
    testCastToArray(da, ReadDataArray[Vec2i, SByte](da), false, data)(Descriptors.Vec2iSByte)
    testCastToArray(da, ReadDataArray[Vec3i, SByte](da), false, data)(Descriptors.Vec3iSByte)
    testCastToArray(da, ReadDataArray[Vec4i, SByte](da), false, data)(Descriptors.Vec4iSByte)

    testCastToArray(da, ReadDataArray[Float1, SByte](da), false, data)(Descriptors.Float1SByte)
    testCastToArray(da, ReadDataArray[Vec2f, SByte](da), false, data)(Descriptors.Vec2fSByte)
    testCastToArray(da, ReadDataArray[Vec3f, SByte](da), false, data)(Descriptors.Vec3fSByte)
    testCastToArray(da, ReadDataArray[Vec4f, SByte](da), false, data)(Descriptors.Vec4fSByte)
    
    testCastToArray(da, ReadDataArray[Double1, SByte](da), false, data)(Descriptors.Double1SByte)
    testCastToArray(da, ReadDataArray[Vec2d, SByte](da), false, data)(Descriptors.Vec2dSByte)
    testCastToArray(da, ReadDataArray[Vec3d, SByte](da), false, data)(Descriptors.Vec3dSByte)
    testCastToArray(da, ReadDataArray[Vec4d, SByte](da), false, data)(Descriptors.Vec4dSByte)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, SByte]]
    intercept[IllegalArgumentException] { DataArray[Int1, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, SByte](roCast) }
    
    intercept[IllegalArgumentException] { DataArray[Float1, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, SByte](roCast) }
    
    intercept[IllegalArgumentException] { DataArray[Double1, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, SByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, SByte](roCast) }
    
    
    testCastToArray(ro, ReadDataArray[Int1, SByte](ro), true, data)(Descriptors.Int1SByte)
    testCastToArray(ro, ReadDataArray[Vec2i, SByte](ro), true, data)(Descriptors.Vec2iSByte)
    testCastToArray(ro, ReadDataArray[Vec3i, SByte](ro), true, data)(Descriptors.Vec3iSByte)
    testCastToArray(ro, ReadDataArray[Vec4i, SByte](ro), true, data)(Descriptors.Vec4iSByte)

    testCastToArray(ro, ReadDataArray[Float1, SByte](ro), true, data)(Descriptors.Float1SByte)
    testCastToArray(ro, ReadDataArray[Vec2f, SByte](ro), true, data)(Descriptors.Vec2fSByte)
    testCastToArray(ro, ReadDataArray[Vec3f, SByte](ro), true, data)(Descriptors.Vec3fSByte)
    testCastToArray(ro, ReadDataArray[Vec4f, SByte](ro), true, data)(Descriptors.Vec4fSByte)
    
    testCastToArray(ro, ReadDataArray[Double1, SByte](ro), true, data)(Descriptors.Double1SByte)
    testCastToArray(ro, ReadDataArray[Vec2d, SByte](ro), true, data)(Descriptors.Vec2dSByte)
    testCastToArray(ro, ReadDataArray[Vec3d, SByte](ro), true, data)(Descriptors.Vec3dSByte)
    testCastToArray(ro, ReadDataArray[Vec4d, SByte](ro), true, data)(Descriptors.Vec4dSByte)
  }
  
  private def testUByteArrayCast(da: DataArray[_, UByte]) {
    val data = da.asBuffer()
    
    testCastToArray(da, DataArray[Int1, UByte](da), false, data)(Descriptors.Int1UByte)
    testCastToArray(da, DataArray[Vec2i, UByte](da), false, data)(Descriptors.Vec2iUByte)
    testCastToArray(da, DataArray[Vec3i, UByte](da), false, data)(Descriptors.Vec3iUByte)
    testCastToArray(da, DataArray[Vec4i, UByte](da), false, data)(Descriptors.Vec4iUByte)

    testCastToArray(da, IndexArray[UByte](da), false, data)(Descriptors.Int1UByte)

    testCastToArray(da, DataArray[Float1, UByte](da), false, data)(Descriptors.Float1UByte)
    testCastToArray(da, DataArray[Vec2f, UByte](da), false, data)(Descriptors.Vec2fUByte)
    testCastToArray(da, DataArray[Vec3f, UByte](da), false, data)(Descriptors.Vec3fUByte)
    testCastToArray(da, DataArray[Vec4f, UByte](da), false, data)(Descriptors.Vec4fUByte)
    
    testCastToArray(da, DataArray[Double1, UByte](da), false, data)(Descriptors.Double1UByte)
    testCastToArray(da, DataArray[Vec2d, UByte](da), false, data)(Descriptors.Vec2dUByte)
    testCastToArray(da, DataArray[Vec3d, UByte](da), false, data)(Descriptors.Vec3dUByte)
    testCastToArray(da, DataArray[Vec4d, UByte](da), false, data)(Descriptors.Vec4dUByte)

    
    testCastToArray(da, ReadDataArray[Int1, UByte](da), false, data)(Descriptors.Int1UByte)
    testCastToArray(da, ReadDataArray[Vec2i, UByte](da), false, data)(Descriptors.Vec2iUByte)
    testCastToArray(da, ReadDataArray[Vec3i, UByte](da), false, data)(Descriptors.Vec3iUByte)
    testCastToArray(da, ReadDataArray[Vec4i, UByte](da), false, data)(Descriptors.Vec4iUByte)

    testCastToArray(da, ReadIndexArray[UByte](da), false, data)(Descriptors.Int1UByte)

    testCastToArray(da, ReadDataArray[Float1, UByte](da), false, data)(Descriptors.Float1UByte)
    testCastToArray(da, ReadDataArray[Vec2f, UByte](da), false, data)(Descriptors.Vec2fUByte)
    testCastToArray(da, ReadDataArray[Vec3f, UByte](da), false, data)(Descriptors.Vec3fUByte)
    testCastToArray(da, ReadDataArray[Vec4f, UByte](da), false, data)(Descriptors.Vec4fUByte)
    
    testCastToArray(da, ReadDataArray[Double1, UByte](da), false, data)(Descriptors.Double1UByte)
    testCastToArray(da, ReadDataArray[Vec2d, UByte](da), false, data)(Descriptors.Vec2dUByte)
    testCastToArray(da, ReadDataArray[Vec3d, UByte](da), false, data)(Descriptors.Vec3dUByte)
    testCastToArray(da, ReadDataArray[Vec4d, UByte](da), false, data)(Descriptors.Vec4dUByte)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, UByte]]
    intercept[IllegalArgumentException] { DataArray[Int1, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, UByte](roCast) }
    
    intercept[IllegalArgumentException] { IndexArray[UByte](roCast) }
    
    intercept[IllegalArgumentException] { DataArray[Float1, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, UByte](roCast) }
    
    intercept[IllegalArgumentException] { DataArray[Double1, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, UByte](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, UByte](roCast) }


    testCastToArray(ro, ReadDataArray[Int1, UByte](ro), true, data)(Descriptors.Int1UByte)
    testCastToArray(ro, ReadDataArray[Vec2i, UByte](ro), true, data)(Descriptors.Vec2iUByte)
    testCastToArray(ro, ReadDataArray[Vec3i, UByte](ro), true, data)(Descriptors.Vec3iUByte)
    testCastToArray(ro, ReadDataArray[Vec4i, UByte](ro), true, data)(Descriptors.Vec4iUByte)

    testCastToArray(ro, ReadIndexArray[UByte](ro), true, data)(Descriptors.Int1UByte)

    testCastToArray(ro, ReadDataArray[Float1, UByte](ro), true, data)(Descriptors.Float1UByte)
    testCastToArray(ro, ReadDataArray[Vec2f, UByte](ro), true, data)(Descriptors.Vec2fUByte)
    testCastToArray(ro, ReadDataArray[Vec3f, UByte](ro), true, data)(Descriptors.Vec3fUByte)
    testCastToArray(ro, ReadDataArray[Vec4f, UByte](ro), true, data)(Descriptors.Vec4fUByte)
    
    testCastToArray(ro, ReadDataArray[Double1, UByte](ro), true, data)(Descriptors.Double1UByte)
    testCastToArray(ro, ReadDataArray[Vec2d, UByte](ro), true, data)(Descriptors.Vec2dUByte)
    testCastToArray(ro, ReadDataArray[Vec3d, UByte](ro), true, data)(Descriptors.Vec3dUByte)
    testCastToArray(ro, ReadDataArray[Vec4d, UByte](ro), true, data)(Descriptors.Vec4dUByte)
  }

  private def testSShortArrayCast(da: DataArray[_, SShort]) {
    val data = da.asBuffer()

    testCastToArray(da, DataArray[Int1, SShort](da), false, data)(Descriptors.Int1SShort)
    testCastToArray(da, DataArray[Vec2i, SShort](da), false, data)(Descriptors.Vec2iSShort)
    testCastToArray(da, DataArray[Vec3i, SShort](da), false, data)(Descriptors.Vec3iSShort)
    testCastToArray(da, DataArray[Vec4i, SShort](da), false, data)(Descriptors.Vec4iSShort)

    testCastToArray(da, DataArray[Float1, SShort](da), false, data)(Descriptors.Float1SShort)
    testCastToArray(da, DataArray[Vec2f, SShort](da), false, data)(Descriptors.Vec2fSShort)
    testCastToArray(da, DataArray[Vec3f, SShort](da), false, data)(Descriptors.Vec3fSShort)
    testCastToArray(da, DataArray[Vec4f, SShort](da), false, data)(Descriptors.Vec4fSShort)
    
    testCastToArray(da, DataArray[Double1, SShort](da), false, data)(Descriptors.Double1SShort)
    testCastToArray(da, DataArray[Vec2d, SShort](da), false, data)(Descriptors.Vec2dSShort)
    testCastToArray(da, DataArray[Vec3d, SShort](da), false, data)(Descriptors.Vec3dSShort)
    testCastToArray(da, DataArray[Vec4d, SShort](da), false, data)(Descriptors.Vec4dSShort)

    
    testCastToArray(da, ReadDataArray[Int1, SShort](da), false, data)(Descriptors.Int1SShort)
    testCastToArray(da, ReadDataArray[Vec2i, SShort](da), false, data)(Descriptors.Vec2iSShort)
    testCastToArray(da, ReadDataArray[Vec3i, SShort](da), false, data)(Descriptors.Vec3iSShort)
    testCastToArray(da, ReadDataArray[Vec4i, SShort](da), false, data)(Descriptors.Vec4iSShort)

    testCastToArray(da, ReadDataArray[Float1, SShort](da), false, data)(Descriptors.Float1SShort)
    testCastToArray(da, ReadDataArray[Vec2f, SShort](da), false, data)(Descriptors.Vec2fSShort)
    testCastToArray(da, ReadDataArray[Vec3f, SShort](da), false, data)(Descriptors.Vec3fSShort)
    testCastToArray(da, ReadDataArray[Vec4f, SShort](da), false, data)(Descriptors.Vec4fSShort)
    
    testCastToArray(da, ReadDataArray[Double1, SShort](da), false, data)(Descriptors.Double1SShort)
    testCastToArray(da, ReadDataArray[Vec2d, SShort](da), false, data)(Descriptors.Vec2dSShort)
    testCastToArray(da, ReadDataArray[Vec3d, SShort](da), false, data)(Descriptors.Vec3dSShort)
    testCastToArray(da, ReadDataArray[Vec4d, SShort](da), false, data)(Descriptors.Vec4dSShort)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, SShort]]
    intercept[IllegalArgumentException] { DataArray[Int1, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, SShort](roCast) }

    intercept[IllegalArgumentException] { DataArray[Float1, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, SShort](roCast) }

    intercept[IllegalArgumentException] { DataArray[Double1, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, SShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, SShort](roCast) }


    testCastToArray(ro, ReadDataArray[Int1, SShort](ro), true, data)(Descriptors.Int1SShort)
    testCastToArray(ro, ReadDataArray[Vec2i, SShort](ro), true, data)(Descriptors.Vec2iSShort)
    testCastToArray(ro, ReadDataArray[Vec3i, SShort](ro), true, data)(Descriptors.Vec3iSShort)
    testCastToArray(ro, ReadDataArray[Vec4i, SShort](ro), true, data)(Descriptors.Vec4iSShort)

    testCastToArray(ro, ReadDataArray[Float1, SShort](ro), true, data)(Descriptors.Float1SShort)
    testCastToArray(ro, ReadDataArray[Vec2f, SShort](ro), true, data)(Descriptors.Vec2fSShort)
    testCastToArray(ro, ReadDataArray[Vec3f, SShort](ro), true, data)(Descriptors.Vec3fSShort)
    testCastToArray(ro, ReadDataArray[Vec4f, SShort](ro), true, data)(Descriptors.Vec4fSShort)
    
    testCastToArray(ro, ReadDataArray[Double1, SShort](ro), true, data)(Descriptors.Double1SShort)
    testCastToArray(ro, ReadDataArray[Vec2d, SShort](ro), true, data)(Descriptors.Vec2dSShort)
    testCastToArray(ro, ReadDataArray[Vec3d, SShort](ro), true, data)(Descriptors.Vec3dSShort)
    testCastToArray(ro, ReadDataArray[Vec4d, SShort](ro), true, data)(Descriptors.Vec4dSShort)
  }
  
  private def testUShortArrayCast(da: DataArray[_, UShort]) {
    val data = da.asBuffer()
    
    testCastToArray(da, DataArray[Int1, UShort](da), false, data)(Descriptors.Int1UShort)
    testCastToArray(da, DataArray[Vec2i, UShort](da), false, data)(Descriptors.Vec2iUShort)
    testCastToArray(da, DataArray[Vec3i, UShort](da), false, data)(Descriptors.Vec3iUShort)
    testCastToArray(da, DataArray[Vec4i, UShort](da), false, data)(Descriptors.Vec4iUShort)

    testCastToArray(da, IndexArray[UShort](da), false, data)(Descriptors.Int1UShort)

    testCastToArray(da, DataArray[Float1, UShort](da), false, data)(Descriptors.Float1UShort)
    testCastToArray(da, DataArray[Vec2f, UShort](da), false, data)(Descriptors.Vec2fUShort)
    testCastToArray(da, DataArray[Vec3f, UShort](da), false, data)(Descriptors.Vec3fUShort)
    testCastToArray(da, DataArray[Vec4f, UShort](da), false, data)(Descriptors.Vec4fUShort)
    
    testCastToArray(da, DataArray[Double1, UShort](da), false, data)(Descriptors.Double1UShort)
    testCastToArray(da, DataArray[Vec2d, UShort](da), false, data)(Descriptors.Vec2dUShort)
    testCastToArray(da, DataArray[Vec3d, UShort](da), false, data)(Descriptors.Vec3dUShort)
    testCastToArray(da, DataArray[Vec4d, UShort](da), false, data)(Descriptors.Vec4dUShort)

    
    testCastToArray(da, ReadDataArray[Int1, UShort](da), false, data)(Descriptors.Int1UShort)
    testCastToArray(da, ReadDataArray[Vec2i, UShort](da), false, data)(Descriptors.Vec2iUShort)
    testCastToArray(da, ReadDataArray[Vec3i, UShort](da), false, data)(Descriptors.Vec3iUShort)
    testCastToArray(da, ReadDataArray[Vec4i, UShort](da), false, data)(Descriptors.Vec4iUShort)

    testCastToArray(da, ReadIndexArray[UShort](da), false, data)(Descriptors.Int1UShort)

    testCastToArray(da, ReadDataArray[Float1, UShort](da), false, data)(Descriptors.Float1UShort)
    testCastToArray(da, ReadDataArray[Vec2f, UShort](da), false, data)(Descriptors.Vec2fUShort)
    testCastToArray(da, ReadDataArray[Vec3f, UShort](da), false, data)(Descriptors.Vec3fUShort)
    testCastToArray(da, ReadDataArray[Vec4f, UShort](da), false, data)(Descriptors.Vec4fUShort)
    
    testCastToArray(da, ReadDataArray[Double1, UShort](da), false, data)(Descriptors.Double1UShort)
    testCastToArray(da, ReadDataArray[Vec2d, UShort](da), false, data)(Descriptors.Vec2dUShort)
    testCastToArray(da, ReadDataArray[Vec3d, UShort](da), false, data)(Descriptors.Vec3dUShort)
    testCastToArray(da, ReadDataArray[Vec4d, UShort](da), false, data)(Descriptors.Vec4dUShort)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, UShort]]
    intercept[IllegalArgumentException] { DataArray[Int1, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, UShort](roCast) }

    intercept[IllegalArgumentException] { IndexArray[UShort](roCast) }

    intercept[IllegalArgumentException] { DataArray[Float1, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, UShort](roCast) }

    intercept[IllegalArgumentException] { DataArray[Double1, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, UShort](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, UShort](roCast) }


    testCastToArray(ro, ReadDataArray[Int1, UShort](ro), true, data)(Descriptors.Int1UShort)
    testCastToArray(ro, ReadDataArray[Vec2i, UShort](ro), true, data)(Descriptors.Vec2iUShort)
    testCastToArray(ro, ReadDataArray[Vec3i, UShort](ro), true, data)(Descriptors.Vec3iUShort)
    testCastToArray(ro, ReadDataArray[Vec4i, UShort](ro), true, data)(Descriptors.Vec4iUShort)

    testCastToArray(ro, ReadIndexArray[UShort](ro), true, data)(Descriptors.Int1UShort)

    testCastToArray(ro, ReadDataArray[Float1, UShort](ro), true, data)(Descriptors.Float1UShort)
    testCastToArray(ro, ReadDataArray[Vec2f, UShort](ro), true, data)(Descriptors.Vec2fUShort)
    testCastToArray(ro, ReadDataArray[Vec3f, UShort](ro), true, data)(Descriptors.Vec3fUShort)
    testCastToArray(ro, ReadDataArray[Vec4f, UShort](ro), true, data)(Descriptors.Vec4fUShort)
    
    testCastToArray(ro, ReadDataArray[Double1, UShort](ro), true, data)(Descriptors.Double1UShort)
    testCastToArray(ro, ReadDataArray[Vec2d, UShort](ro), true, data)(Descriptors.Vec2dUShort)
    testCastToArray(ro, ReadDataArray[Vec3d, UShort](ro), true, data)(Descriptors.Vec3dUShort)
    testCastToArray(ro, ReadDataArray[Vec4d, UShort](ro), true, data)(Descriptors.Vec4dUShort)
  }
  
  private def testSIntArrayCast(da: DataArray[_, SInt]) {
    val data = da.asBuffer()

    testCastToArray(da, DataArray[Int1, SInt](da), false, data)(Descriptors.Int1SInt)
    testCastToArray(da, DataArray[Vec2i, SInt](da), false, data)(Descriptors.Vec2iSInt)
    testCastToArray(da, DataArray[Vec3i, SInt](da), false, data)(Descriptors.Vec3iSInt)
    testCastToArray(da, DataArray[Vec4i, SInt](da), false, data)(Descriptors.Vec4iSInt)

    testCastToArray(da, DataArray[Float1, SInt](da), false, data)(Descriptors.Float1SInt)
    testCastToArray(da, DataArray[Vec2f, SInt](da), false, data)(Descriptors.Vec2fSInt)
    testCastToArray(da, DataArray[Vec3f, SInt](da), false, data)(Descriptors.Vec3fSInt)
    testCastToArray(da, DataArray[Vec4f, SInt](da), false, data)(Descriptors.Vec4fSInt)
    
    testCastToArray(da, DataArray[Double1, SInt](da), false, data)(Descriptors.Double1SInt)
    testCastToArray(da, DataArray[Vec2d, SInt](da), false, data)(Descriptors.Vec2dSInt)
    testCastToArray(da, DataArray[Vec3d, SInt](da), false, data)(Descriptors.Vec3dSInt)
    testCastToArray(da, DataArray[Vec4d, SInt](da), false, data)(Descriptors.Vec4dSInt)

    
    testCastToArray(da, ReadDataArray[Int1, SInt](da), false, data)(Descriptors.Int1SInt)
    testCastToArray(da, ReadDataArray[Vec2i, SInt](da), false, data)(Descriptors.Vec2iSInt)
    testCastToArray(da, ReadDataArray[Vec3i, SInt](da), false, data)(Descriptors.Vec3iSInt)
    testCastToArray(da, ReadDataArray[Vec4i, SInt](da), false, data)(Descriptors.Vec4iSInt)

    testCastToArray(da, ReadDataArray[Float1, SInt](da), false, data)(Descriptors.Float1SInt)
    testCastToArray(da, ReadDataArray[Vec2f, SInt](da), false, data)(Descriptors.Vec2fSInt)
    testCastToArray(da, ReadDataArray[Vec3f, SInt](da), false, data)(Descriptors.Vec3fSInt)
    testCastToArray(da, ReadDataArray[Vec4f, SInt](da), false, data)(Descriptors.Vec4fSInt)
    
    testCastToArray(da, ReadDataArray[Double1, SInt](da), false, data)(Descriptors.Double1SInt)
    testCastToArray(da, ReadDataArray[Vec2d, SInt](da), false, data)(Descriptors.Vec2dSInt)
    testCastToArray(da, ReadDataArray[Vec3d, SInt](da), false, data)(Descriptors.Vec3dSInt)
    testCastToArray(da, ReadDataArray[Vec4d, SInt](da), false, data)(Descriptors.Vec4dSInt)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, SInt]]
    intercept[IllegalArgumentException] { DataArray[Int1, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, SInt](roCast) }

    intercept[IllegalArgumentException] { DataArray[Float1, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, SInt](roCast) }

    intercept[IllegalArgumentException] { DataArray[Double1, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, SInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, SInt](roCast) }


    testCastToArray(ro, ReadDataArray[Int1, SInt](ro), true, data)(Descriptors.Int1SInt)
    testCastToArray(ro, ReadDataArray[Vec2i, SInt](ro), true, data)(Descriptors.Vec2iSInt)
    testCastToArray(ro, ReadDataArray[Vec3i, SInt](ro), true, data)(Descriptors.Vec3iSInt)
    testCastToArray(ro, ReadDataArray[Vec4i, SInt](ro), true, data)(Descriptors.Vec4iSInt)

    testCastToArray(ro, ReadDataArray[Float1, SInt](ro), true, data)(Descriptors.Float1SInt)
    testCastToArray(ro, ReadDataArray[Vec2f, SInt](ro), true, data)(Descriptors.Vec2fSInt)
    testCastToArray(ro, ReadDataArray[Vec3f, SInt](ro), true, data)(Descriptors.Vec3fSInt)
    testCastToArray(ro, ReadDataArray[Vec4f, SInt](ro), true, data)(Descriptors.Vec4fSInt)
    
    testCastToArray(ro, ReadDataArray[Double1, SInt](ro), true, data)(Descriptors.Double1SInt)
    testCastToArray(ro, ReadDataArray[Vec2d, SInt](ro), true, data)(Descriptors.Vec2dSInt)
    testCastToArray(ro, ReadDataArray[Vec3d, SInt](ro), true, data)(Descriptors.Vec3dSInt)
    testCastToArray(ro, ReadDataArray[Vec4d, SInt](ro), true, data)(Descriptors.Vec4dSInt)
  }
  
  private def testUIntArrayCast(da: DataArray[_, UInt]) {
    val data = da.asBuffer()
    
    testCastToArray(da, DataArray[Int1, UInt](da), false, data)(Descriptors.Int1UInt)
    testCastToArray(da, DataArray[Vec2i, UInt](da), false, data)(Descriptors.Vec2iUInt)
    testCastToArray(da, DataArray[Vec3i, UInt](da), false, data)(Descriptors.Vec3iUInt)
    testCastToArray(da, DataArray[Vec4i, UInt](da), false, data)(Descriptors.Vec4iUInt)

    testCastToArray(da, IndexArray[UInt](da), false, data)(Descriptors.Int1UInt)

    testCastToArray(da, DataArray[Float1, UInt](da), false, data)(Descriptors.Float1UInt)
    testCastToArray(da, DataArray[Vec2f, UInt](da), false, data)(Descriptors.Vec2fUInt)
    testCastToArray(da, DataArray[Vec3f, UInt](da), false, data)(Descriptors.Vec3fUInt)
    testCastToArray(da, DataArray[Vec4f, UInt](da), false, data)(Descriptors.Vec4fUInt)
    
    testCastToArray(da, DataArray[Double1, UInt](da), false, data)(Descriptors.Double1UInt)
    testCastToArray(da, DataArray[Vec2d, UInt](da), false, data)(Descriptors.Vec2dUInt)
    testCastToArray(da, DataArray[Vec3d, UInt](da), false, data)(Descriptors.Vec3dUInt)
    testCastToArray(da, DataArray[Vec4d, UInt](da), false, data)(Descriptors.Vec4dUInt)

    
    testCastToArray(da, ReadDataArray[Int1, UInt](da), false, data)(Descriptors.Int1UInt)
    testCastToArray(da, ReadDataArray[Vec2i, UInt](da), false, data)(Descriptors.Vec2iUInt)
    testCastToArray(da, ReadDataArray[Vec3i, UInt](da), false, data)(Descriptors.Vec3iUInt)
    testCastToArray(da, ReadDataArray[Vec4i, UInt](da), false, data)(Descriptors.Vec4iUInt)

    testCastToArray(da, ReadIndexArray[UInt](da), false, data)(Descriptors.Int1UInt)

    testCastToArray(da, ReadDataArray[Float1, UInt](da), false, data)(Descriptors.Float1UInt)
    testCastToArray(da, ReadDataArray[Vec2f, UInt](da), false, data)(Descriptors.Vec2fUInt)
    testCastToArray(da, ReadDataArray[Vec3f, UInt](da), false, data)(Descriptors.Vec3fUInt)
    testCastToArray(da, ReadDataArray[Vec4f, UInt](da), false, data)(Descriptors.Vec4fUInt)
    
    testCastToArray(da, ReadDataArray[Double1, UInt](da), false, data)(Descriptors.Double1UInt)
    testCastToArray(da, ReadDataArray[Vec2d, UInt](da), false, data)(Descriptors.Vec2dUInt)
    testCastToArray(da, ReadDataArray[Vec3d, UInt](da), false, data)(Descriptors.Vec3dUInt)
    testCastToArray(da, ReadDataArray[Vec4d, UInt](da), false, data)(Descriptors.Vec4dUInt)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, UInt]]
    intercept[IllegalArgumentException] { DataArray[Int1, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2i, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3i, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4i, UInt](roCast) }

    intercept[IllegalArgumentException] { IndexArray[UInt](roCast) }

    intercept[IllegalArgumentException] { DataArray[Float1, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, UInt](roCast) }

    intercept[IllegalArgumentException] { DataArray[Double1, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, UInt](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, UInt](roCast) }


    testCastToArray(ro, ReadDataArray[Int1, UInt](ro), true, data)(Descriptors.Int1UInt)
    testCastToArray(ro, ReadDataArray[Vec2i, UInt](ro), true, data)(Descriptors.Vec2iUInt)
    testCastToArray(ro, ReadDataArray[Vec3i, UInt](ro), true, data)(Descriptors.Vec3iUInt)
    testCastToArray(ro, ReadDataArray[Vec4i, UInt](ro), true, data)(Descriptors.Vec4iUInt)

    testCastToArray(ro, ReadIndexArray[UInt](ro), true, data)(Descriptors.Int1UInt)

    testCastToArray(ro, ReadDataArray[Float1, UInt](ro), true, data)(Descriptors.Float1UInt)
    testCastToArray(ro, ReadDataArray[Vec2f, UInt](ro), true, data)(Descriptors.Vec2fUInt)
    testCastToArray(ro, ReadDataArray[Vec3f, UInt](ro), true, data)(Descriptors.Vec3fUInt)
    testCastToArray(ro, ReadDataArray[Vec4f, UInt](ro), true, data)(Descriptors.Vec4fUInt)
    
    testCastToArray(ro, ReadDataArray[Double1, UInt](ro), true, data)(Descriptors.Double1UInt)
    testCastToArray(ro, ReadDataArray[Vec2d, UInt](ro), true, data)(Descriptors.Vec2dUInt)
    testCastToArray(ro, ReadDataArray[Vec3d, UInt](ro), true, data)(Descriptors.Vec3dUInt)
    testCastToArray(ro, ReadDataArray[Vec4d, UInt](ro), true, data)(Descriptors.Vec4dUInt)
  }

  private def testHalfFloatArrayCast(da: DataArray[_, HalfFloat]) {
    val data = da.asBuffer()

    testCastToArray(da, DataArray[Float1, HalfFloat](da), false, data)(Descriptors.Float1HalfFloat)
    testCastToArray(da, DataArray[Vec2f, HalfFloat](da), false, data)(Descriptors.Vec2fHalfFloat)
    testCastToArray(da, DataArray[Vec3f, HalfFloat](da), false, data)(Descriptors.Vec3fHalfFloat)
    testCastToArray(da, DataArray[Vec4f, HalfFloat](da), false, data)(Descriptors.Vec4fHalfFloat)

    testCastToArray(da, DataArray[Double1, HalfFloat](da), false, data)(Descriptors.Double1HalfFloat)
    testCastToArray(da, DataArray[Vec2d, HalfFloat](da), false, data)(Descriptors.Vec2dHalfFloat)
    testCastToArray(da, DataArray[Vec3d, HalfFloat](da), false, data)(Descriptors.Vec3dHalfFloat)
    testCastToArray(da, DataArray[Vec4d, HalfFloat](da), false, data)(Descriptors.Vec4dHalfFloat)


    testCastToArray(da, ReadDataArray[Float1, HalfFloat](da), false, data)(Descriptors.Float1HalfFloat)
    testCastToArray(da, ReadDataArray[Vec2f, HalfFloat](da), false, data)(Descriptors.Vec2fHalfFloat)
    testCastToArray(da, ReadDataArray[Vec3f, HalfFloat](da), false, data)(Descriptors.Vec3fHalfFloat)
    testCastToArray(da, ReadDataArray[Vec4f, HalfFloat](da), false, data)(Descriptors.Vec4fHalfFloat)

    testCastToArray(da, ReadDataArray[Double1, HalfFloat](da), false, data)(Descriptors.Double1HalfFloat)
    testCastToArray(da, ReadDataArray[Vec2d, HalfFloat](da), false, data)(Descriptors.Vec2dHalfFloat)
    testCastToArray(da, ReadDataArray[Vec3d, HalfFloat](da), false, data)(Descriptors.Vec3dHalfFloat)
    testCastToArray(da, ReadDataArray[Vec4d, HalfFloat](da), false, data)(Descriptors.Vec4dHalfFloat)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, HalfFloat]]
    intercept[IllegalArgumentException] { DataArray[Float1, HalfFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, HalfFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, HalfFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, HalfFloat](roCast) }

    intercept[IllegalArgumentException] { DataArray[Double1, HalfFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, HalfFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, HalfFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, HalfFloat](roCast) }


    testCastToArray(ro, ReadDataArray[Float1, HalfFloat](ro), true, data)(Descriptors.Float1HalfFloat)
    testCastToArray(ro, ReadDataArray[Vec2f, HalfFloat](ro), true, data)(Descriptors.Vec2fHalfFloat)
    testCastToArray(ro, ReadDataArray[Vec3f, HalfFloat](ro), true, data)(Descriptors.Vec3fHalfFloat)
    testCastToArray(ro, ReadDataArray[Vec4f, HalfFloat](ro), true, data)(Descriptors.Vec4fHalfFloat)

    testCastToArray(ro, ReadDataArray[Double1, HalfFloat](ro), true, data)(Descriptors.Double1HalfFloat)
    testCastToArray(ro, ReadDataArray[Vec2d, HalfFloat](ro), true, data)(Descriptors.Vec2dHalfFloat)
    testCastToArray(ro, ReadDataArray[Vec3d, HalfFloat](ro), true, data)(Descriptors.Vec3dHalfFloat)
    testCastToArray(ro, ReadDataArray[Vec4d, HalfFloat](ro), true, data)(Descriptors.Vec4dHalfFloat)
  }
  
  private def testRawFloatArrayCast(da: DataArray[_, RawFloat]) {
    val data = da.asBuffer()

    testCastToArray(da, DataArray[Float1, RawFloat](da), false, data)(Descriptors.Float1RawFloat)
    testCastToArray(da, DataArray[Vec2f, RawFloat](da), false, data)(Descriptors.Vec2fRawFloat)
    testCastToArray(da, DataArray[Vec3f, RawFloat](da), false, data)(Descriptors.Vec3fRawFloat)
    testCastToArray(da, DataArray[Vec4f, RawFloat](da), false, data)(Descriptors.Vec4fRawFloat)

    testCastToArray(da, DataArray[Double1, RawFloat](da), false, data)(Descriptors.Double1RawFloat)
    testCastToArray(da, DataArray[Vec2d, RawFloat](da), false, data)(Descriptors.Vec2dRawFloat)
    testCastToArray(da, DataArray[Vec3d, RawFloat](da), false, data)(Descriptors.Vec3dRawFloat)
    testCastToArray(da, DataArray[Vec4d, RawFloat](da), false, data)(Descriptors.Vec4dRawFloat)


    testCastToArray(da, ReadDataArray[Float1, RawFloat](da), false, data)(Descriptors.Float1RawFloat)
    testCastToArray(da, ReadDataArray[Vec2f, RawFloat](da), false, data)(Descriptors.Vec2fRawFloat)
    testCastToArray(da, ReadDataArray[Vec3f, RawFloat](da), false, data)(Descriptors.Vec3fRawFloat)
    testCastToArray(da, ReadDataArray[Vec4f, RawFloat](da), false, data)(Descriptors.Vec4fRawFloat)

    testCastToArray(da, ReadDataArray[Double1, RawFloat](da), false, data)(Descriptors.Double1RawFloat)
    testCastToArray(da, ReadDataArray[Vec2d, RawFloat](da), false, data)(Descriptors.Vec2dRawFloat)
    testCastToArray(da, ReadDataArray[Vec3d, RawFloat](da), false, data)(Descriptors.Vec3dRawFloat)
    testCastToArray(da, ReadDataArray[Vec4d, RawFloat](da), false, data)(Descriptors.Vec4dRawFloat)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, RawFloat]]
    intercept[IllegalArgumentException] { DataArray[Float1, RawFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2f, RawFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3f, RawFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4f, RawFloat](roCast) }

    intercept[IllegalArgumentException] { DataArray[Double1, RawFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, RawFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, RawFloat](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, RawFloat](roCast) }


    testCastToArray(ro, ReadDataArray[Float1, RawFloat](ro), true, data)(Descriptors.Float1RawFloat)
    testCastToArray(ro, ReadDataArray[Vec2f, RawFloat](ro), true, data)(Descriptors.Vec2fRawFloat)
    testCastToArray(ro, ReadDataArray[Vec3f, RawFloat](ro), true, data)(Descriptors.Vec3fRawFloat)
    testCastToArray(ro, ReadDataArray[Vec4f, RawFloat](ro), true, data)(Descriptors.Vec4fRawFloat)

    testCastToArray(ro, ReadDataArray[Double1, RawFloat](ro), true, data)(Descriptors.Double1RawFloat)
    testCastToArray(ro, ReadDataArray[Vec2d, RawFloat](ro), true, data)(Descriptors.Vec2dRawFloat)
    testCastToArray(ro, ReadDataArray[Vec3d, RawFloat](ro), true, data)(Descriptors.Vec3dRawFloat)
    testCastToArray(ro, ReadDataArray[Vec4d, RawFloat](ro), true, data)(Descriptors.Vec4dRawFloat)
  }
  
  private def testRawDoubleArrayCast(da: DataArray[_, RawDouble]) {
    val data = da.asBuffer()

    testCastToArray(da, DataArray[Double1, RawDouble](da), false, data)(Descriptors.Double1RawDouble)
    testCastToArray(da, DataArray[Vec2d, RawDouble](da), false, data)(Descriptors.Vec2dRawDouble)
    testCastToArray(da, DataArray[Vec3d, RawDouble](da), false, data)(Descriptors.Vec3dRawDouble)
    testCastToArray(da, DataArray[Vec4d, RawDouble](da), false, data)(Descriptors.Vec4dRawDouble)


    testCastToArray(da, ReadDataArray[Double1, RawDouble](da), false, data)(Descriptors.Double1RawDouble)
    testCastToArray(da, ReadDataArray[Vec2d, RawDouble](da), false, data)(Descriptors.Vec2dRawDouble)
    testCastToArray(da, ReadDataArray[Vec3d, RawDouble](da), false, data)(Descriptors.Vec3dRawDouble)
    testCastToArray(da, ReadDataArray[Vec4d, RawDouble](da), false, data)(Descriptors.Vec4dRawDouble)


    val ro = da.asReadOnlySeq()
    val roCast = ro.asInstanceOf[DataArray[_, RawDouble]]
    intercept[IllegalArgumentException] { DataArray[Double1, RawDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec2d, RawDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec3d, RawDouble](roCast) }
    intercept[IllegalArgumentException] { DataArray[Vec4d, RawDouble](roCast) }


    testCastToArray(ro, ReadDataArray[Double1, RawDouble](ro), true, data)(Descriptors.Double1RawDouble)
    testCastToArray(ro, ReadDataArray[Vec2d, RawDouble](ro), true, data)(Descriptors.Vec2dRawDouble)
    testCastToArray(ro, ReadDataArray[Vec3d, RawDouble](ro), true, data)(Descriptors.Vec3dRawDouble)
    testCastToArray(ro, ReadDataArray[Vec4d, RawDouble](ro), true, data)(Descriptors.Vec4dRawDouble)
  }


  def testBufferCast[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {

    for (size <- 0 to 1; extraBytes <- 0 to 8) {
      val (bytes, _) = genRandomBuffer(size*8*4*2 + extraBytes, Descriptors.Int1SByte)
      val seq = factory(bytes)


      testCastToBuffer(seq, IndexBuffer[UByte](seq), false, bytes)(Descriptors.Int1UByte)
      testCastToBuffer(seq, IndexBuffer[UShort](seq), false, bytes)(Descriptors.Int1UShort)
      testCastToBuffer(seq, IndexBuffer[UInt](seq), false, bytes)(Descriptors.Int1UInt)
      
      testCastToBuffer(seq, DataBuffer[Int1, SByte](seq), false, bytes)(Descriptors.Int1SByte)
      testCastToBuffer(seq, DataBuffer[Int1, UByte](seq), false, bytes)(Descriptors.Int1UByte)
      testCastToBuffer(seq, DataBuffer[Int1, SShort](seq), false, bytes)(Descriptors.Int1SShort)
      testCastToBuffer(seq, DataBuffer[Int1, UShort](seq), false, bytes)(Descriptors.Int1UShort)
      testCastToBuffer(seq, DataBuffer[Int1, SInt](seq), false, bytes)(Descriptors.Int1SInt)
      testCastToBuffer(seq, DataBuffer[Int1, UInt](seq), false, bytes)(Descriptors.Int1UInt)
      
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
      
      testCastToBuffer(seq, DataBuffer[Float1, SByte](seq), false, bytes)(Descriptors.Float1SByte)
      testCastToBuffer(seq, DataBuffer[Float1, UByte](seq), false, bytes)(Descriptors.Float1UByte)
      testCastToBuffer(seq, DataBuffer[Float1, SShort](seq), false, bytes)(Descriptors.Float1SShort)
      testCastToBuffer(seq, DataBuffer[Float1, UShort](seq), false, bytes)(Descriptors.Float1UShort)
      testCastToBuffer(seq, DataBuffer[Float1, SInt](seq), false, bytes)(Descriptors.Float1SInt)
      testCastToBuffer(seq, DataBuffer[Float1, UInt](seq), false, bytes)(Descriptors.Float1UInt)
      testCastToBuffer(seq, DataBuffer[Float1, HalfFloat](seq), false, bytes)(Descriptors.Float1HalfFloat)
      testCastToBuffer(seq, DataBuffer[Float1, RawFloat](seq), false, bytes)(Descriptors.Float1RawFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec2f, SByte](seq), false, bytes)(Descriptors.Vec2fSByte)
      testCastToBuffer(seq, DataBuffer[Vec2f, UByte](seq), false, bytes)(Descriptors.Vec2fUByte)
      testCastToBuffer(seq, DataBuffer[Vec2f, SShort](seq), false, bytes)(Descriptors.Vec2fSShort)
      testCastToBuffer(seq, DataBuffer[Vec2f, UShort](seq), false, bytes)(Descriptors.Vec2fUShort)
      testCastToBuffer(seq, DataBuffer[Vec2f, SInt](seq), false, bytes)(Descriptors.Vec2fSInt)
      testCastToBuffer(seq, DataBuffer[Vec2f, UInt](seq), false, bytes)(Descriptors.Vec2fUInt)
      testCastToBuffer(seq, DataBuffer[Vec2f, HalfFloat](seq), false, bytes)(Descriptors.Vec2fHalfFloat)
      testCastToBuffer(seq, DataBuffer[Vec2f, RawFloat](seq), false, bytes)(Descriptors.Vec2fRawFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec3f, SByte](seq), false, bytes)(Descriptors.Vec3fSByte)
      testCastToBuffer(seq, DataBuffer[Vec3f, UByte](seq), false, bytes)(Descriptors.Vec3fUByte)
      testCastToBuffer(seq, DataBuffer[Vec3f, SShort](seq), false, bytes)(Descriptors.Vec3fSShort)
      testCastToBuffer(seq, DataBuffer[Vec3f, UShort](seq), false, bytes)(Descriptors.Vec3fUShort)
      testCastToBuffer(seq, DataBuffer[Vec3f, SInt](seq), false, bytes)(Descriptors.Vec3fSInt)
      testCastToBuffer(seq, DataBuffer[Vec3f, UInt](seq), false, bytes)(Descriptors.Vec3fUInt)
      testCastToBuffer(seq, DataBuffer[Vec3f, HalfFloat](seq), false, bytes)(Descriptors.Vec3fHalfFloat)
      testCastToBuffer(seq, DataBuffer[Vec3f, RawFloat](seq), false, bytes)(Descriptors.Vec3fRawFloat)
      
      testCastToBuffer(seq, DataBuffer[Vec4f, SByte](seq), false, bytes)(Descriptors.Vec4fSByte)
      testCastToBuffer(seq, DataBuffer[Vec4f, UByte](seq), false, bytes)(Descriptors.Vec4fUByte)
      testCastToBuffer(seq, DataBuffer[Vec4f, SShort](seq), false, bytes)(Descriptors.Vec4fSShort)
      testCastToBuffer(seq, DataBuffer[Vec4f, UShort](seq), false, bytes)(Descriptors.Vec4fUShort)
      testCastToBuffer(seq, DataBuffer[Vec4f, SInt](seq), false, bytes)(Descriptors.Vec4fSInt)
      testCastToBuffer(seq, DataBuffer[Vec4f, UInt](seq), false, bytes)(Descriptors.Vec4fUInt)
      testCastToBuffer(seq, DataBuffer[Vec4f, HalfFloat](seq), false, bytes)(Descriptors.Vec4fHalfFloat)
      testCastToBuffer(seq, DataBuffer[Vec4f, RawFloat](seq), false, bytes)(Descriptors.Vec4fRawFloat)
      
      testCastToBuffer(seq, DataBuffer[Double1, SByte](seq), false, bytes)(Descriptors.Double1SByte)
      testCastToBuffer(seq, DataBuffer[Double1, UByte](seq), false, bytes)(Descriptors.Double1UByte)
      testCastToBuffer(seq, DataBuffer[Double1, SShort](seq), false, bytes)(Descriptors.Double1SShort)
      testCastToBuffer(seq, DataBuffer[Double1, UShort](seq), false, bytes)(Descriptors.Double1UShort)
      testCastToBuffer(seq, DataBuffer[Double1, SInt](seq), false, bytes)(Descriptors.Double1SInt)
      testCastToBuffer(seq, DataBuffer[Double1, UInt](seq), false, bytes)(Descriptors.Double1UInt)
      testCastToBuffer(seq, DataBuffer[Double1, HalfFloat](seq), false, bytes)(Descriptors.Double1HalfFloat)
      testCastToBuffer(seq, DataBuffer[Double1, RawFloat](seq), false, bytes)(Descriptors.Double1RawFloat)
      testCastToBuffer(seq, DataBuffer[Double1, RawDouble](seq), false, bytes)(Descriptors.Double1RawDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec2d, SByte](seq), false, bytes)(Descriptors.Vec2dSByte)
      testCastToBuffer(seq, DataBuffer[Vec2d, UByte](seq), false, bytes)(Descriptors.Vec2dUByte)
      testCastToBuffer(seq, DataBuffer[Vec2d, SShort](seq), false, bytes)(Descriptors.Vec2dSShort)
      testCastToBuffer(seq, DataBuffer[Vec2d, UShort](seq), false, bytes)(Descriptors.Vec2dUShort)
      testCastToBuffer(seq, DataBuffer[Vec2d, SInt](seq), false, bytes)(Descriptors.Vec2dSInt)
      testCastToBuffer(seq, DataBuffer[Vec2d, UInt](seq), false, bytes)(Descriptors.Vec2dUInt)
      testCastToBuffer(seq, DataBuffer[Vec2d, HalfFloat](seq), false, bytes)(Descriptors.Vec2dHalfFloat)
      testCastToBuffer(seq, DataBuffer[Vec2d, RawFloat](seq), false, bytes)(Descriptors.Vec2dRawFloat)
      testCastToBuffer(seq, DataBuffer[Vec2d, RawDouble](seq), false, bytes)(Descriptors.Vec2dRawDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec3d, SByte](seq), false, bytes)(Descriptors.Vec3dSByte)
      testCastToBuffer(seq, DataBuffer[Vec3d, UByte](seq), false, bytes)(Descriptors.Vec3dUByte)
      testCastToBuffer(seq, DataBuffer[Vec3d, SShort](seq), false, bytes)(Descriptors.Vec3dSShort)
      testCastToBuffer(seq, DataBuffer[Vec3d, UShort](seq), false, bytes)(Descriptors.Vec3dUShort)
      testCastToBuffer(seq, DataBuffer[Vec3d, SInt](seq), false, bytes)(Descriptors.Vec3dSInt)
      testCastToBuffer(seq, DataBuffer[Vec3d, UInt](seq), false, bytes)(Descriptors.Vec3dUInt)
      testCastToBuffer(seq, DataBuffer[Vec3d, HalfFloat](seq), false, bytes)(Descriptors.Vec3dHalfFloat)
      testCastToBuffer(seq, DataBuffer[Vec3d, RawFloat](seq), false, bytes)(Descriptors.Vec3dRawFloat)
      testCastToBuffer(seq, DataBuffer[Vec3d, RawDouble](seq), false, bytes)(Descriptors.Vec3dRawDouble)
      
      testCastToBuffer(seq, DataBuffer[Vec4d, SByte](seq), false, bytes)(Descriptors.Vec4dSByte)
      testCastToBuffer(seq, DataBuffer[Vec4d, UByte](seq), false, bytes)(Descriptors.Vec4dUByte)
      testCastToBuffer(seq, DataBuffer[Vec4d, SShort](seq), false, bytes)(Descriptors.Vec4dSShort)
      testCastToBuffer(seq, DataBuffer[Vec4d, UShort](seq), false, bytes)(Descriptors.Vec4dUShort)
      testCastToBuffer(seq, DataBuffer[Vec4d, SInt](seq), false, bytes)(Descriptors.Vec4dSInt)
      testCastToBuffer(seq, DataBuffer[Vec4d, UInt](seq), false, bytes)(Descriptors.Vec4dUInt)
      testCastToBuffer(seq, DataBuffer[Vec4d, HalfFloat](seq), false, bytes)(Descriptors.Vec4dHalfFloat)
      testCastToBuffer(seq, DataBuffer[Vec4d, RawFloat](seq), false, bytes)(Descriptors.Vec4dRawFloat)
      testCastToBuffer(seq, DataBuffer[Vec4d, RawDouble](seq), false, bytes)(Descriptors.Vec4dRawDouble)
      
      
      testCastToBuffer(seq, ReadIndexBuffer[UByte](seq), false, bytes)(Descriptors.Int1UByte)
      testCastToBuffer(seq, ReadIndexBuffer[UShort](seq), false, bytes)(Descriptors.Int1UShort)
      testCastToBuffer(seq, ReadIndexBuffer[UInt](seq), false, bytes)(Descriptors.Int1UInt)
      
      testCastToBuffer(seq, ReadDataBuffer[Int1, SByte](seq), false, bytes)(Descriptors.Int1SByte)
      testCastToBuffer(seq, ReadDataBuffer[Int1, UByte](seq), false, bytes)(Descriptors.Int1UByte)
      testCastToBuffer(seq, ReadDataBuffer[Int1, SShort](seq), false, bytes)(Descriptors.Int1SShort)
      testCastToBuffer(seq, ReadDataBuffer[Int1, UShort](seq), false, bytes)(Descriptors.Int1UShort)
      testCastToBuffer(seq, ReadDataBuffer[Int1, SInt](seq), false, bytes)(Descriptors.Int1SInt)
      testCastToBuffer(seq, ReadDataBuffer[Int1, UInt](seq), false, bytes)(Descriptors.Int1UInt)
      
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
      
      testCastToBuffer(seq, ReadDataBuffer[Float1, SByte](seq), false, bytes)(Descriptors.Float1SByte)
      testCastToBuffer(seq, ReadDataBuffer[Float1, UByte](seq), false, bytes)(Descriptors.Float1UByte)
      testCastToBuffer(seq, ReadDataBuffer[Float1, SShort](seq), false, bytes)(Descriptors.Float1SShort)
      testCastToBuffer(seq, ReadDataBuffer[Float1, UShort](seq), false, bytes)(Descriptors.Float1UShort)
      testCastToBuffer(seq, ReadDataBuffer[Float1, SInt](seq), false, bytes)(Descriptors.Float1SInt)
      testCastToBuffer(seq, ReadDataBuffer[Float1, UInt](seq), false, bytes)(Descriptors.Float1UInt)
      testCastToBuffer(seq, ReadDataBuffer[Float1, HalfFloat](seq), false, bytes)(Descriptors.Float1HalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Float1, RawFloat](seq), false, bytes)(Descriptors.Float1RawFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SByte](seq), false, bytes)(Descriptors.Vec2fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UByte](seq), false, bytes)(Descriptors.Vec2fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SShort](seq), false, bytes)(Descriptors.Vec2fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UShort](seq), false, bytes)(Descriptors.Vec2fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SInt](seq), false, bytes)(Descriptors.Vec2fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UInt](seq), false, bytes)(Descriptors.Vec2fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, HalfFloat](seq), false, bytes)(Descriptors.Vec2fHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, RawFloat](seq), false, bytes)(Descriptors.Vec2fRawFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SByte](seq), false, bytes)(Descriptors.Vec3fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UByte](seq), false, bytes)(Descriptors.Vec3fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SShort](seq), false, bytes)(Descriptors.Vec3fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UShort](seq), false, bytes)(Descriptors.Vec3fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SInt](seq), false, bytes)(Descriptors.Vec3fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UInt](seq), false, bytes)(Descriptors.Vec3fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, HalfFloat](seq), false, bytes)(Descriptors.Vec3fHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, RawFloat](seq), false, bytes)(Descriptors.Vec3fRawFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SByte](seq), false, bytes)(Descriptors.Vec4fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UByte](seq), false, bytes)(Descriptors.Vec4fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SShort](seq), false, bytes)(Descriptors.Vec4fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UShort](seq), false, bytes)(Descriptors.Vec4fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SInt](seq), false, bytes)(Descriptors.Vec4fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UInt](seq), false, bytes)(Descriptors.Vec4fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, HalfFloat](seq), false, bytes)(Descriptors.Vec4fHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, RawFloat](seq), false, bytes)(Descriptors.Vec4fRawFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Double1, SByte](seq), false, bytes)(Descriptors.Double1SByte)
      testCastToBuffer(seq, ReadDataBuffer[Double1, UByte](seq), false, bytes)(Descriptors.Double1UByte)
      testCastToBuffer(seq, ReadDataBuffer[Double1, SShort](seq), false, bytes)(Descriptors.Double1SShort)
      testCastToBuffer(seq, ReadDataBuffer[Double1, UShort](seq), false, bytes)(Descriptors.Double1UShort)
      testCastToBuffer(seq, ReadDataBuffer[Double1, SInt](seq), false, bytes)(Descriptors.Double1SInt)
      testCastToBuffer(seq, ReadDataBuffer[Double1, UInt](seq), false, bytes)(Descriptors.Double1UInt)
      testCastToBuffer(seq, ReadDataBuffer[Double1, HalfFloat](seq), false, bytes)(Descriptors.Double1HalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Double1, RawFloat](seq), false, bytes)(Descriptors.Double1RawFloat)
      testCastToBuffer(seq, ReadDataBuffer[Double1, RawDouble](seq), false, bytes)(Descriptors.Double1RawDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SByte](seq), false, bytes)(Descriptors.Vec2dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UByte](seq), false, bytes)(Descriptors.Vec2dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SShort](seq), false, bytes)(Descriptors.Vec2dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UShort](seq), false, bytes)(Descriptors.Vec2dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SInt](seq), false, bytes)(Descriptors.Vec2dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UInt](seq), false, bytes)(Descriptors.Vec2dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, HalfFloat](seq), false, bytes)(Descriptors.Vec2dHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, RawFloat](seq), false, bytes)(Descriptors.Vec2dRawFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, RawDouble](seq), false, bytes)(Descriptors.Vec2dRawDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SByte](seq), false, bytes)(Descriptors.Vec3dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UByte](seq), false, bytes)(Descriptors.Vec3dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SShort](seq), false, bytes)(Descriptors.Vec3dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UShort](seq), false, bytes)(Descriptors.Vec3dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SInt](seq), false, bytes)(Descriptors.Vec3dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UInt](seq), false, bytes)(Descriptors.Vec3dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, HalfFloat](seq), false, bytes)(Descriptors.Vec3dHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, RawFloat](seq), false, bytes)(Descriptors.Vec3dRawFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, RawDouble](seq), false, bytes)(Descriptors.Vec3dRawDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SByte](seq), false, bytes)(Descriptors.Vec4dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UByte](seq), false, bytes)(Descriptors.Vec4dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SShort](seq), false, bytes)(Descriptors.Vec4dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UShort](seq), false, bytes)(Descriptors.Vec4dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SInt](seq), false, bytes)(Descriptors.Vec4dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UInt](seq), false, bytes)(Descriptors.Vec4dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, HalfFloat](seq), false, bytes)(Descriptors.Vec4dHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, RawFloat](seq), false, bytes)(Descriptors.Vec4dRawFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, RawDouble](seq), false, bytes)(Descriptors.Vec4dRawDouble)
      
      
      val ro = seq.asReadOnlySeq()
      val roCast = ro.asInstanceOf[DataBuffer[_, RawData]]
      
      intercept[IllegalArgumentException] { IndexBuffer[UByte](roCast) }
      intercept[IllegalArgumentException] { IndexBuffer[UShort](roCast) }
      intercept[IllegalArgumentException] { IndexBuffer[UInt](roCast) }
      
      intercept[IllegalArgumentException] { DataBuffer[Int1, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Int1, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Int1, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Int1, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Int1, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Int1, UInt](roCast) }

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

      intercept[IllegalArgumentException] { DataBuffer[Float1, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Float1, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Float1, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Float1, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Float1, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Float1, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Float1, HalfFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Float1, RawFloat](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec2f, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, HalfFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2f, RawFloat](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec3f, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, HalfFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3f, RawFloat](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec4f, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, HalfFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4f, RawFloat](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Double1, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Double1, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Double1, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Double1, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Double1, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Double1, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Double1, HalfFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Double1, RawFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Double1, RawDouble](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec2d, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, HalfFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, RawFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec2d, RawDouble](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec3d, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, HalfFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, RawFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec3d, RawDouble](roCast) }

      intercept[IllegalArgumentException] { DataBuffer[Vec4d, SByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, UByte](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, SShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, UShort](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, SInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, UInt](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, HalfFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, RawFloat](roCast) }
      intercept[IllegalArgumentException] { DataBuffer[Vec4d, RawDouble](roCast) }


      testCastToBuffer(seq, ReadIndexBuffer[UByte](ro), true, bytes)(Descriptors.Int1UByte)
      testCastToBuffer(seq, ReadIndexBuffer[UShort](ro), true, bytes)(Descriptors.Int1UShort)
      testCastToBuffer(seq, ReadIndexBuffer[UInt](ro), true, bytes)(Descriptors.Int1UInt)
      
      testCastToBuffer(seq, ReadDataBuffer[Int1, SByte](ro), true, bytes)(Descriptors.Int1SByte)
      testCastToBuffer(seq, ReadDataBuffer[Int1, UByte](ro), true, bytes)(Descriptors.Int1UByte)
      testCastToBuffer(seq, ReadDataBuffer[Int1, SShort](ro), true, bytes)(Descriptors.Int1SShort)
      testCastToBuffer(seq, ReadDataBuffer[Int1, UShort](ro), true, bytes)(Descriptors.Int1UShort)
      testCastToBuffer(seq, ReadDataBuffer[Int1, SInt](ro), true, bytes)(Descriptors.Int1SInt)
      testCastToBuffer(seq, ReadDataBuffer[Int1, UInt](ro), true, bytes)(Descriptors.Int1UInt)
      
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
      
      testCastToBuffer(seq, ReadDataBuffer[Float1, SByte](ro), true, bytes)(Descriptors.Float1SByte)
      testCastToBuffer(seq, ReadDataBuffer[Float1, UByte](ro), true, bytes)(Descriptors.Float1UByte)
      testCastToBuffer(seq, ReadDataBuffer[Float1, SShort](ro), true, bytes)(Descriptors.Float1SShort)
      testCastToBuffer(seq, ReadDataBuffer[Float1, UShort](ro), true, bytes)(Descriptors.Float1UShort)
      testCastToBuffer(seq, ReadDataBuffer[Float1, SInt](ro), true, bytes)(Descriptors.Float1SInt)
      testCastToBuffer(seq, ReadDataBuffer[Float1, UInt](ro), true, bytes)(Descriptors.Float1UInt)
      testCastToBuffer(seq, ReadDataBuffer[Float1, HalfFloat](ro), true, bytes)(Descriptors.Float1HalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Float1, RawFloat](ro), true, bytes)(Descriptors.Float1RawFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SByte](ro), true, bytes)(Descriptors.Vec2fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UByte](ro), true, bytes)(Descriptors.Vec2fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SShort](ro), true, bytes)(Descriptors.Vec2fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UShort](ro), true, bytes)(Descriptors.Vec2fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, SInt](ro), true, bytes)(Descriptors.Vec2fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, UInt](ro), true, bytes)(Descriptors.Vec2fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, HalfFloat](ro), true, bytes)(Descriptors.Vec2fHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2f, RawFloat](ro), true, bytes)(Descriptors.Vec2fRawFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SByte](ro), true, bytes)(Descriptors.Vec3fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UByte](ro), true, bytes)(Descriptors.Vec3fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SShort](ro), true, bytes)(Descriptors.Vec3fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UShort](ro), true, bytes)(Descriptors.Vec3fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, SInt](ro), true, bytes)(Descriptors.Vec3fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, UInt](ro), true, bytes)(Descriptors.Vec3fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, HalfFloat](ro), true, bytes)(Descriptors.Vec3fHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3f, RawFloat](ro), true, bytes)(Descriptors.Vec3fRawFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SByte](ro), true, bytes)(Descriptors.Vec4fSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UByte](ro), true, bytes)(Descriptors.Vec4fUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SShort](ro), true, bytes)(Descriptors.Vec4fSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UShort](ro), true, bytes)(Descriptors.Vec4fUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, SInt](ro), true, bytes)(Descriptors.Vec4fSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, UInt](ro), true, bytes)(Descriptors.Vec4fUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, HalfFloat](ro), true, bytes)(Descriptors.Vec4fHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4f, RawFloat](ro), true, bytes)(Descriptors.Vec4fRawFloat)
      
      testCastToBuffer(seq, ReadDataBuffer[Double1, SByte](ro), true, bytes)(Descriptors.Double1SByte)
      testCastToBuffer(seq, ReadDataBuffer[Double1, UByte](ro), true, bytes)(Descriptors.Double1UByte)
      testCastToBuffer(seq, ReadDataBuffer[Double1, SShort](ro), true, bytes)(Descriptors.Double1SShort)
      testCastToBuffer(seq, ReadDataBuffer[Double1, UShort](ro), true, bytes)(Descriptors.Double1UShort)
      testCastToBuffer(seq, ReadDataBuffer[Double1, SInt](ro), true, bytes)(Descriptors.Double1SInt)
      testCastToBuffer(seq, ReadDataBuffer[Double1, UInt](ro), true, bytes)(Descriptors.Double1UInt)
      testCastToBuffer(seq, ReadDataBuffer[Double1, HalfFloat](ro), true, bytes)(Descriptors.Double1HalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Double1, RawFloat](ro), true, bytes)(Descriptors.Double1RawFloat)
      testCastToBuffer(seq, ReadDataBuffer[Double1, RawDouble](ro), true, bytes)(Descriptors.Double1RawDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SByte](ro), true, bytes)(Descriptors.Vec2dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UByte](ro), true, bytes)(Descriptors.Vec2dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SShort](ro), true, bytes)(Descriptors.Vec2dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UShort](ro), true, bytes)(Descriptors.Vec2dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, SInt](ro), true, bytes)(Descriptors.Vec2dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, UInt](ro), true, bytes)(Descriptors.Vec2dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, HalfFloat](ro), true, bytes)(Descriptors.Vec2dHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, RawFloat](ro), true, bytes)(Descriptors.Vec2dRawFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec2d, RawDouble](ro), true, bytes)(Descriptors.Vec2dRawDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SByte](ro), true, bytes)(Descriptors.Vec3dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UByte](ro), true, bytes)(Descriptors.Vec3dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SShort](ro), true, bytes)(Descriptors.Vec3dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UShort](ro), true, bytes)(Descriptors.Vec3dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, SInt](ro), true, bytes)(Descriptors.Vec3dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, UInt](ro), true, bytes)(Descriptors.Vec3dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, HalfFloat](ro), true, bytes)(Descriptors.Vec3dHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, RawFloat](ro), true, bytes)(Descriptors.Vec3dRawFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec3d, RawDouble](ro), true, bytes)(Descriptors.Vec3dRawDouble)
      
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SByte](ro), true, bytes)(Descriptors.Vec4dSByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UByte](ro), true, bytes)(Descriptors.Vec4dUByte)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SShort](ro), true, bytes)(Descriptors.Vec4dSShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UShort](ro), true, bytes)(Descriptors.Vec4dUShort)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, SInt](ro), true, bytes)(Descriptors.Vec4dSInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, UInt](ro), true, bytes)(Descriptors.Vec4dUInt)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, HalfFloat](ro), true, bytes)(Descriptors.Vec4dHalfFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, RawFloat](ro), true, bytes)(Descriptors.Vec4dRawFloat)
      testCastToBuffer(seq, ReadDataBuffer[Vec4d, RawDouble](ro), true, bytes)(Descriptors.Vec4dRawDouble)
    }
    
    for (size <- 0 to 1; extraBytes <- 0 to 8) {
      val (bytes, _) = genRandomBuffer(size*8*4*2 + extraBytes, Descriptors.Int1SByte)
      val seq = factory(bytes)

      
      testCastToView(seq, DataView[Int1, SByte](_, _, _), bytes)(Descriptors.Int1SByte)
      testCastToView(seq, DataView[Int1, UByte](_, _, _), bytes)(Descriptors.Int1UByte)
      testCastToView(seq, DataView[Int1, SShort](_, _, _), bytes)(Descriptors.Int1SShort)
      testCastToView(seq, DataView[Int1, UShort](_, _, _), bytes)(Descriptors.Int1UShort)
      testCastToView(seq, DataView[Int1, SInt](_, _, _), bytes)(Descriptors.Int1SInt)
      testCastToView(seq, DataView[Int1, UInt](_, _, _), bytes)(Descriptors.Int1UInt)
      
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
      
      testCastToView(seq, DataView[Float1, SByte](_, _, _), bytes)(Descriptors.Float1SByte)
      testCastToView(seq, DataView[Float1, UByte](_, _, _), bytes)(Descriptors.Float1UByte)
      testCastToView(seq, DataView[Float1, SShort](_, _, _), bytes)(Descriptors.Float1SShort)
      testCastToView(seq, DataView[Float1, UShort](_, _, _), bytes)(Descriptors.Float1UShort)
      testCastToView(seq, DataView[Float1, SInt](_, _, _), bytes)(Descriptors.Float1SInt)
      testCastToView(seq, DataView[Float1, UInt](_, _, _), bytes)(Descriptors.Float1UInt)
      testCastToView(seq, DataView[Float1, HalfFloat](_, _, _), bytes)(Descriptors.Float1HalfFloat)
      testCastToView(seq, DataView[Float1, RawFloat](_, _, _), bytes)(Descriptors.Float1RawFloat)
      
      testCastToView(seq, DataView[Vec2f, SByte](_, _, _), bytes)(Descriptors.Vec2fSByte)
      testCastToView(seq, DataView[Vec2f, UByte](_, _, _), bytes)(Descriptors.Vec2fUByte)
      testCastToView(seq, DataView[Vec2f, SShort](_, _, _), bytes)(Descriptors.Vec2fSShort)
      testCastToView(seq, DataView[Vec2f, UShort](_, _, _), bytes)(Descriptors.Vec2fUShort)
      testCastToView(seq, DataView[Vec2f, SInt](_, _, _), bytes)(Descriptors.Vec2fSInt)
      testCastToView(seq, DataView[Vec2f, UInt](_, _, _), bytes)(Descriptors.Vec2fUInt)
      testCastToView(seq, DataView[Vec2f, HalfFloat](_, _, _), bytes)(Descriptors.Vec2fHalfFloat)
      testCastToView(seq, DataView[Vec2f, RawFloat](_, _, _), bytes)(Descriptors.Vec2fRawFloat)
      
      testCastToView(seq, DataView[Vec3f, SByte](_, _, _), bytes)(Descriptors.Vec3fSByte)
      testCastToView(seq, DataView[Vec3f, UByte](_, _, _), bytes)(Descriptors.Vec3fUByte)
      testCastToView(seq, DataView[Vec3f, SShort](_, _, _), bytes)(Descriptors.Vec3fSShort)
      testCastToView(seq, DataView[Vec3f, UShort](_, _, _), bytes)(Descriptors.Vec3fUShort)
      testCastToView(seq, DataView[Vec3f, SInt](_, _, _), bytes)(Descriptors.Vec3fSInt)
      testCastToView(seq, DataView[Vec3f, UInt](_, _, _), bytes)(Descriptors.Vec3fUInt)
      testCastToView(seq, DataView[Vec3f, HalfFloat](_, _, _), bytes)(Descriptors.Vec3fHalfFloat)
      testCastToView(seq, DataView[Vec3f, RawFloat](_, _, _), bytes)(Descriptors.Vec3fRawFloat)
      
      testCastToView(seq, DataView[Vec4f, SByte](_, _, _), bytes)(Descriptors.Vec4fSByte)
      testCastToView(seq, DataView[Vec4f, UByte](_, _, _), bytes)(Descriptors.Vec4fUByte)
      testCastToView(seq, DataView[Vec4f, SShort](_, _, _), bytes)(Descriptors.Vec4fSShort)
      testCastToView(seq, DataView[Vec4f, UShort](_, _, _), bytes)(Descriptors.Vec4fUShort)
      testCastToView(seq, DataView[Vec4f, SInt](_, _, _), bytes)(Descriptors.Vec4fSInt)
      testCastToView(seq, DataView[Vec4f, UInt](_, _, _), bytes)(Descriptors.Vec4fUInt)
      testCastToView(seq, DataView[Vec4f, HalfFloat](_, _, _), bytes)(Descriptors.Vec4fHalfFloat)
      testCastToView(seq, DataView[Vec4f, RawFloat](_, _, _), bytes)(Descriptors.Vec4fRawFloat)
      
      testCastToView(seq, DataView[Double1, SByte](_, _, _), bytes)(Descriptors.Double1SByte)
      testCastToView(seq, DataView[Double1, UByte](_, _, _), bytes)(Descriptors.Double1UByte)
      testCastToView(seq, DataView[Double1, SShort](_, _, _), bytes)(Descriptors.Double1SShort)
      testCastToView(seq, DataView[Double1, UShort](_, _, _), bytes)(Descriptors.Double1UShort)
      testCastToView(seq, DataView[Double1, SInt](_, _, _), bytes)(Descriptors.Double1SInt)
      testCastToView(seq, DataView[Double1, UInt](_, _, _), bytes)(Descriptors.Double1UInt)
      testCastToView(seq, DataView[Double1, HalfFloat](_, _, _), bytes)(Descriptors.Double1HalfFloat)
      testCastToView(seq, DataView[Double1, RawFloat](_, _, _), bytes)(Descriptors.Double1RawFloat)
      testCastToView(seq, DataView[Double1, RawDouble](_, _, _), bytes)(Descriptors.Double1RawDouble)
      
      testCastToView(seq, DataView[Vec2d, SByte](_, _, _), bytes)(Descriptors.Vec2dSByte)
      testCastToView(seq, DataView[Vec2d, UByte](_, _, _), bytes)(Descriptors.Vec2dUByte)
      testCastToView(seq, DataView[Vec2d, SShort](_, _, _), bytes)(Descriptors.Vec2dSShort)
      testCastToView(seq, DataView[Vec2d, UShort](_, _, _), bytes)(Descriptors.Vec2dUShort)
      testCastToView(seq, DataView[Vec2d, SInt](_, _, _), bytes)(Descriptors.Vec2dSInt)
      testCastToView(seq, DataView[Vec2d, UInt](_, _, _), bytes)(Descriptors.Vec2dUInt)
      testCastToView(seq, DataView[Vec2d, HalfFloat](_, _, _), bytes)(Descriptors.Vec2dHalfFloat)
      testCastToView(seq, DataView[Vec2d, RawFloat](_, _, _), bytes)(Descriptors.Vec2dRawFloat)
      testCastToView(seq, DataView[Vec2d, RawDouble](_, _, _), bytes)(Descriptors.Vec2dRawDouble)
      
      testCastToView(seq, DataView[Vec3d, SByte](_, _, _), bytes)(Descriptors.Vec3dSByte)
      testCastToView(seq, DataView[Vec3d, UByte](_, _, _), bytes)(Descriptors.Vec3dUByte)
      testCastToView(seq, DataView[Vec3d, SShort](_, _, _), bytes)(Descriptors.Vec3dSShort)
      testCastToView(seq, DataView[Vec3d, UShort](_, _, _), bytes)(Descriptors.Vec3dUShort)
      testCastToView(seq, DataView[Vec3d, SInt](_, _, _), bytes)(Descriptors.Vec3dSInt)
      testCastToView(seq, DataView[Vec3d, UInt](_, _, _), bytes)(Descriptors.Vec3dUInt)
      testCastToView(seq, DataView[Vec3d, HalfFloat](_, _, _), bytes)(Descriptors.Vec3dHalfFloat)
      testCastToView(seq, DataView[Vec3d, RawFloat](_, _, _), bytes)(Descriptors.Vec3dRawFloat)
      testCastToView(seq, DataView[Vec3d, RawDouble](_, _, _), bytes)(Descriptors.Vec3dRawDouble)
      
      testCastToView(seq, DataView[Vec4d, SByte](_, _, _), bytes)(Descriptors.Vec4dSByte)
      testCastToView(seq, DataView[Vec4d, UByte](_, _, _), bytes)(Descriptors.Vec4dUByte)
      testCastToView(seq, DataView[Vec4d, SShort](_, _, _), bytes)(Descriptors.Vec4dSShort)
      testCastToView(seq, DataView[Vec4d, UShort](_, _, _), bytes)(Descriptors.Vec4dUShort)
      testCastToView(seq, DataView[Vec4d, SInt](_, _, _), bytes)(Descriptors.Vec4dSInt)
      testCastToView(seq, DataView[Vec4d, UInt](_, _, _), bytes)(Descriptors.Vec4dUInt)
      testCastToView(seq, DataView[Vec4d, HalfFloat](_, _, _), bytes)(Descriptors.Vec4dHalfFloat)
      testCastToView(seq, DataView[Vec4d, RawFloat](_, _, _), bytes)(Descriptors.Vec4dRawFloat)
      testCastToView(seq, DataView[Vec4d, RawDouble](_, _, _), bytes)(Descriptors.Vec4dRawDouble)

      
      testCastToReadView(seq, ReadDataView[Int1, SByte](_, _, _), bytes)(Descriptors.Int1SByte)
      testCastToReadView(seq, ReadDataView[Int1, UByte](_, _, _), bytes)(Descriptors.Int1UByte)
      testCastToReadView(seq, ReadDataView[Int1, SShort](_, _, _), bytes)(Descriptors.Int1SShort)
      testCastToReadView(seq, ReadDataView[Int1, UShort](_, _, _), bytes)(Descriptors.Int1UShort)
      testCastToReadView(seq, ReadDataView[Int1, SInt](_, _, _), bytes)(Descriptors.Int1SInt)
      testCastToReadView(seq, ReadDataView[Int1, UInt](_, _, _), bytes)(Descriptors.Int1UInt)
      
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
      
      testCastToReadView(seq, ReadDataView[Float1, SByte](_, _, _), bytes)(Descriptors.Float1SByte)
      testCastToReadView(seq, ReadDataView[Float1, UByte](_, _, _), bytes)(Descriptors.Float1UByte)
      testCastToReadView(seq, ReadDataView[Float1, SShort](_, _, _), bytes)(Descriptors.Float1SShort)
      testCastToReadView(seq, ReadDataView[Float1, UShort](_, _, _), bytes)(Descriptors.Float1UShort)
      testCastToReadView(seq, ReadDataView[Float1, SInt](_, _, _), bytes)(Descriptors.Float1SInt)
      testCastToReadView(seq, ReadDataView[Float1, UInt](_, _, _), bytes)(Descriptors.Float1UInt)
      testCastToReadView(seq, ReadDataView[Float1, HalfFloat](_, _, _), bytes)(Descriptors.Float1HalfFloat)
      testCastToReadView(seq, ReadDataView[Float1, RawFloat](_, _, _), bytes)(Descriptors.Float1RawFloat)
      
      testCastToReadView(seq, ReadDataView[Vec2f, SByte](_, _, _), bytes)(Descriptors.Vec2fSByte)
      testCastToReadView(seq, ReadDataView[Vec2f, UByte](_, _, _), bytes)(Descriptors.Vec2fUByte)
      testCastToReadView(seq, ReadDataView[Vec2f, SShort](_, _, _), bytes)(Descriptors.Vec2fSShort)
      testCastToReadView(seq, ReadDataView[Vec2f, UShort](_, _, _), bytes)(Descriptors.Vec2fUShort)
      testCastToReadView(seq, ReadDataView[Vec2f, SInt](_, _, _), bytes)(Descriptors.Vec2fSInt)
      testCastToReadView(seq, ReadDataView[Vec2f, UInt](_, _, _), bytes)(Descriptors.Vec2fUInt)
      testCastToReadView(seq, ReadDataView[Vec2f, HalfFloat](_, _, _), bytes)(Descriptors.Vec2fHalfFloat)
      testCastToReadView(seq, ReadDataView[Vec2f, RawFloat](_, _, _), bytes)(Descriptors.Vec2fRawFloat)
      
      testCastToReadView(seq, ReadDataView[Vec3f, SByte](_, _, _), bytes)(Descriptors.Vec3fSByte)
      testCastToReadView(seq, ReadDataView[Vec3f, UByte](_, _, _), bytes)(Descriptors.Vec3fUByte)
      testCastToReadView(seq, ReadDataView[Vec3f, SShort](_, _, _), bytes)(Descriptors.Vec3fSShort)
      testCastToReadView(seq, ReadDataView[Vec3f, UShort](_, _, _), bytes)(Descriptors.Vec3fUShort)
      testCastToReadView(seq, ReadDataView[Vec3f, SInt](_, _, _), bytes)(Descriptors.Vec3fSInt)
      testCastToReadView(seq, ReadDataView[Vec3f, UInt](_, _, _), bytes)(Descriptors.Vec3fUInt)
      testCastToReadView(seq, ReadDataView[Vec3f, HalfFloat](_, _, _), bytes)(Descriptors.Vec3fHalfFloat)
      testCastToReadView(seq, ReadDataView[Vec3f, RawFloat](_, _, _), bytes)(Descriptors.Vec3fRawFloat)
      
      testCastToReadView(seq, ReadDataView[Vec4f, SByte](_, _, _), bytes)(Descriptors.Vec4fSByte)
      testCastToReadView(seq, ReadDataView[Vec4f, UByte](_, _, _), bytes)(Descriptors.Vec4fUByte)
      testCastToReadView(seq, ReadDataView[Vec4f, SShort](_, _, _), bytes)(Descriptors.Vec4fSShort)
      testCastToReadView(seq, ReadDataView[Vec4f, UShort](_, _, _), bytes)(Descriptors.Vec4fUShort)
      testCastToReadView(seq, ReadDataView[Vec4f, SInt](_, _, _), bytes)(Descriptors.Vec4fSInt)
      testCastToReadView(seq, ReadDataView[Vec4f, UInt](_, _, _), bytes)(Descriptors.Vec4fUInt)
      testCastToReadView(seq, ReadDataView[Vec4f, HalfFloat](_, _, _), bytes)(Descriptors.Vec4fHalfFloat)
      testCastToReadView(seq, ReadDataView[Vec4f, RawFloat](_, _, _), bytes)(Descriptors.Vec4fRawFloat)
      
      testCastToReadView(seq, ReadDataView[Double1, SByte](_, _, _), bytes)(Descriptors.Double1SByte)
      testCastToReadView(seq, ReadDataView[Double1, UByte](_, _, _), bytes)(Descriptors.Double1UByte)
      testCastToReadView(seq, ReadDataView[Double1, SShort](_, _, _), bytes)(Descriptors.Double1SShort)
      testCastToReadView(seq, ReadDataView[Double1, UShort](_, _, _), bytes)(Descriptors.Double1UShort)
      testCastToReadView(seq, ReadDataView[Double1, SInt](_, _, _), bytes)(Descriptors.Double1SInt)
      testCastToReadView(seq, ReadDataView[Double1, UInt](_, _, _), bytes)(Descriptors.Double1UInt)
      testCastToReadView(seq, ReadDataView[Double1, HalfFloat](_, _, _), bytes)(Descriptors.Double1HalfFloat)
      testCastToReadView(seq, ReadDataView[Double1, RawFloat](_, _, _), bytes)(Descriptors.Double1RawFloat)
      testCastToReadView(seq, ReadDataView[Double1, RawDouble](_, _, _), bytes)(Descriptors.Double1RawDouble)
      
      testCastToReadView(seq, ReadDataView[Vec2d, SByte](_, _, _), bytes)(Descriptors.Vec2dSByte)
      testCastToReadView(seq, ReadDataView[Vec2d, UByte](_, _, _), bytes)(Descriptors.Vec2dUByte)
      testCastToReadView(seq, ReadDataView[Vec2d, SShort](_, _, _), bytes)(Descriptors.Vec2dSShort)
      testCastToReadView(seq, ReadDataView[Vec2d, UShort](_, _, _), bytes)(Descriptors.Vec2dUShort)
      testCastToReadView(seq, ReadDataView[Vec2d, SInt](_, _, _), bytes)(Descriptors.Vec2dSInt)
      testCastToReadView(seq, ReadDataView[Vec2d, UInt](_, _, _), bytes)(Descriptors.Vec2dUInt)
      testCastToReadView(seq, ReadDataView[Vec2d, HalfFloat](_, _, _), bytes)(Descriptors.Vec2dHalfFloat)
      testCastToReadView(seq, ReadDataView[Vec2d, RawFloat](_, _, _), bytes)(Descriptors.Vec2dRawFloat)
      testCastToReadView(seq, ReadDataView[Vec2d, RawDouble](_, _, _), bytes)(Descriptors.Vec2dRawDouble)
      
      testCastToReadView(seq, ReadDataView[Vec3d, SByte](_, _, _), bytes)(Descriptors.Vec3dSByte)
      testCastToReadView(seq, ReadDataView[Vec3d, UByte](_, _, _), bytes)(Descriptors.Vec3dUByte)
      testCastToReadView(seq, ReadDataView[Vec3d, SShort](_, _, _), bytes)(Descriptors.Vec3dSShort)
      testCastToReadView(seq, ReadDataView[Vec3d, UShort](_, _, _), bytes)(Descriptors.Vec3dUShort)
      testCastToReadView(seq, ReadDataView[Vec3d, SInt](_, _, _), bytes)(Descriptors.Vec3dSInt)
      testCastToReadView(seq, ReadDataView[Vec3d, UInt](_, _, _), bytes)(Descriptors.Vec3dUInt)
      testCastToReadView(seq, ReadDataView[Vec3d, HalfFloat](_, _, _), bytes)(Descriptors.Vec3dHalfFloat)
      testCastToReadView(seq, ReadDataView[Vec3d, RawFloat](_, _, _), bytes)(Descriptors.Vec3dRawFloat)
      testCastToReadView(seq, ReadDataView[Vec3d, RawDouble](_, _, _), bytes)(Descriptors.Vec3dRawDouble)
      
      testCastToReadView(seq, ReadDataView[Vec4d, SByte](_, _, _), bytes)(Descriptors.Vec4dSByte)
      testCastToReadView(seq, ReadDataView[Vec4d, UByte](_, _, _), bytes)(Descriptors.Vec4dUByte)
      testCastToReadView(seq, ReadDataView[Vec4d, SShort](_, _, _), bytes)(Descriptors.Vec4dSShort)
      testCastToReadView(seq, ReadDataView[Vec4d, UShort](_, _, _), bytes)(Descriptors.Vec4dUShort)
      testCastToReadView(seq, ReadDataView[Vec4d, SInt](_, _, _), bytes)(Descriptors.Vec4dSInt)
      testCastToReadView(seq, ReadDataView[Vec4d, UInt](_, _, _), bytes)(Descriptors.Vec4dUInt)
      testCastToReadView(seq, ReadDataView[Vec4d, HalfFloat](_, _, _), bytes)(Descriptors.Vec4dHalfFloat)
      testCastToReadView(seq, ReadDataView[Vec4d, RawFloat](_, _, _), bytes)(Descriptors.Vec4dRawFloat)
      testCastToReadView(seq, ReadDataView[Vec4d, RawDouble](_, _, _), bytes)(Descriptors.Vec4dRawDouble)
    }
  }

  private def testCastToBuffer[E <: MetaElement, R <: RawData](
    original: ReadDataBuffer[_, _],
    cast: ReadDataBuffer[E, R],
    readOnly: Boolean,
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[E, R]) {
    testBuffer(cast, readOnly, wrap(bytes, descriptor))(descriptor)
    assert(original.sharesStoreObject(cast))
    assert(cast.sharesStoreObject(original))
  }

  private def testCastToView[E <: MetaElement, R <: RawData](
    original: DataBuffer[_, _],
    factory: (DataBuffer[_, _], Int, Int) => DataView[E, R],
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[E, R]) {
    val data = wrap(bytes, descriptor)

    for (stride <- 1 to (descriptor.components + 1); offset <- 0 to IntMath.min(stride - 1, data.limit)) {
      val cast = factory(original, offset, stride)
      testView(cast, offset, stride, false, data)(descriptor)
      assert(original.sharesStoreObject(cast))
      assert(cast.sharesStoreObject(original))
    }

    intercept[IllegalArgumentException] { factory(original.asReadOnlySeq().asInstanceOf[DataBuffer[_, _]], 0, 1) }
  }

  private def testCastToReadView[E <: MetaElement, R <: RawData](
    original: ReadDataBuffer[_, RawData],
    factory: (ReadDataBuffer[_, _], Int, Int) => ReadDataView[E, R],
    bytes: ByteBuffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(!original.isReadOnly)
    val data = wrap(bytes, descriptor)
    val ro = original.asReadOnlySeq()

    for (stride <- 1 to (descriptor.components + 1); offset <- 0 to IntMath.min(stride - 1, data.limit)) {
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
