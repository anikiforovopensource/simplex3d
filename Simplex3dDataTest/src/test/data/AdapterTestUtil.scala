/*
 * Simplex3d, DataTest package
 * Copyright (C) 2011, Aleksey Nikiforov
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

import java.nio._
import org.scalatest._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.data._
import simplex3d.data.common._
import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AdapterTestUtil extends FunSuite {

  def testAdapter[F <: CompositeFormat, B <: Defined](adapter: DataAdapter[F, B])(
    sample: F#Accessor#Read, sampleData: DataArray[F#Component, Raw]
  )(implicit attribs: AdapterAttrib[F, B])
  {
    // Test attributes.
    assert(adapter.components == attribs.components)
    assert(adapter.formatManifest == attribs.formatManifest)
    assert(adapter.accessorManifest == attribs.accessorManifest)
    assert(adapter.boundManifest == attribs.boundManifest)

    // Test make.
    val primSize = 10*adapter.components
    for (readOnly <- List(true, false); rawManifest <- attribs.allowed) {

      type R = T forSome { type T <: B }
      val original = genRandomSeq(
        attribs.componentManifest, RawManifest.toRawType(rawManifest), primSize
      ).asInstanceOf[ReadDataArray[F#Component, R]]

      val descriptor = Descriptor[F, R](
        attribs.formatManifest, attribs.componentManifest, attribs.accessorManifest,
        attribs.components, original.rawType, original.isNormalized
      )

      val primitiveArray =
        if (readOnly) original.copyAsDataArray().asReadOnly().asInstanceOf[DataArray[F#Component, R]]
        else original.copyAsDataArray()

      val primitiveBuffer =
        if (readOnly) original.copyAsDataBuffer().asReadOnly().asInstanceOf[DataBuffer[F#Component, R]]
        else original.copyAsDataBuffer()

      val offset = 1; val stride = adapter.components + 1

      // Test mkReadData.
      {
        testArray(adapter.mkReadDataArray(primitiveArray), readOnly, original.readOnlyBuffer)(descriptor)
        testBuffer(adapter.mkReadDataBuffer(primitiveBuffer), readOnly, original.readOnlyBuffer)(descriptor)

        testView(
          adapter.mkReadDataView(primitiveBuffer, offset, stride),
          offset, stride, readOnly, original.readOnlyBuffer
        )(descriptor)
      }

      // Test mkData.
      if (readOnly) {
        intercept[IllegalArgumentException] {
          testArray(adapter.mkDataArray(primitiveArray), true, original.readOnlyBuffer)(descriptor)
        }

        intercept[IllegalArgumentException] {
          testBuffer(adapter.mkDataBuffer(primitiveBuffer), true, original.readOnlyBuffer)(descriptor)
        }

        intercept[IllegalArgumentException] {
          testView(
            adapter.mkDataView(primitiveBuffer, offset, stride),
            offset, stride, true, original.readOnlyBuffer
          )(descriptor)
        }
      }
      else {
        testArray(adapter.mkDataArray(primitiveArray), false, original.readOnlyBuffer)(descriptor)
        testBuffer(adapter.mkDataBuffer(primitiveBuffer), false, original.readOnlyBuffer)(descriptor)

        testView(
          adapter.mkDataView(primitiveBuffer, offset, stride),
          offset, stride, false, original.readOnlyBuffer
        )(descriptor)
      }
    }

    val factory = {
      type R = T forSome { type T <: B }
      val primitives = genRandomSeq(
        attribs.componentManifest, RawManifest.toRawType(attribs.componentManifest), 0
      ).asInstanceOf[ReadDataArray[F#Component, R]]
      adapter.mkReadDataArray(primitives).asInstanceOf[CompositionFactory[F, B]]
    }

    // Test raw types that are not allowed.
    for (
      raw <- RawManifest.AllDefined
      if !attribs.allowed.contains(raw) && supportsRawType(attribs.componentManifest, raw)
    ) {
      type R = T forSome { type T <: B }

      val array = genRandomSeq(
        attribs.componentManifest, RawManifest.toRawType(raw), primSize
      ).asInstanceOf[DataArray[F#Component, R]]

      val buffer = array.copyAsDataBuffer()
      val offset = 1; val stride = adapter.components + 1

      intercept[IllegalArgumentException] {
        adapter.mkReadDataArray(array)
      }
      intercept[IllegalArgumentException] {
        adapter.mkReadDataBuffer(buffer)
      }
      intercept[IllegalArgumentException] {
        adapter.mkReadDataView(buffer, offset, stride)
      }

      intercept[IllegalArgumentException] {
        adapter.mkDataArray(array)
      }
      intercept[IllegalArgumentException] {
        adapter.mkDataBuffer(buffer)
      }
      intercept[IllegalArgumentException] {
        adapter.mkDataView(buffer, offset, stride)
      }

      intercept[IllegalArgumentException] {
        factory.mkReadDataArray(array)
      }
      intercept[IllegalArgumentException] {
        factory.mkReadDataBuffer(buffer)
      }
      intercept[IllegalArgumentException] {
        factory.mkReadDataView(buffer, offset, stride)
      }

      intercept[IllegalArgumentException] {
        factory.mkDataArray(array)
      }
      intercept[IllegalArgumentException] {
        factory.mkDataBuffer(buffer)
      }
      intercept[IllegalArgumentException] {
        factory.mkDataView(buffer, offset, stride)
      }
    }

    // Test apply/update.
    assert(sampleData.formatManifest == attribs.componentManifest)

    val j = 1
    val writeBuffer = genRandomSeq(
      attribs.componentManifest, sampleData.rawType, sampleData.size + j
    ).asInstanceOf[Contiguous[F#Component, Raw]]

    assert(adapter.apply(sampleData, 0) == sample)
    assert(adapter.apply(writeBuffer, j) != sample)
    adapter.update(writeBuffer, j, sample)
    assert(adapter.apply(writeBuffer, j) == sample)
  }
}

case class AdapterAttrib[F <: Format, B <: Defined](components: Int, allowed: ClassManifest[_ <: Defined]*)(implicit
  val formatManifest: ClassManifest[F],
  val accessorManifest: ClassManifest[F#Accessor],
  val boundManifest: Manifest[B],
  val componentManifest: ClassManifest[F#Component]
)

object AdapterAttribs {
  import RawManifest._

  implicit val Quat4fAttribs = AdapterAttrib[Quat4f, DefinedFloat with SysFP](4, RFloat)
  implicit val Mat2x2fAttribs = AdapterAttrib[Mat2x2f, DefinedFloat with SysFP](4, RFloat)
  implicit val Mat2x3fAttribs = AdapterAttrib[Mat2x3f, DefinedFloat with SysFP](6, RFloat)
  implicit val Mat2x4fAttribs = AdapterAttrib[Mat2x4f, DefinedFloat with SysFP](8, RFloat)
  implicit val Mat3x2fAttribs = AdapterAttrib[Mat3x2f, DefinedFloat with SysFP](6, RFloat)
  implicit val Mat3x3fAttribs = AdapterAttrib[Mat3x3f, DefinedFloat with SysFP](9, RFloat)
  implicit val Mat3x4fAttribs = AdapterAttrib[Mat3x4f, DefinedFloat with SysFP](12, RFloat)
  implicit val Mat4x2fAttribs = AdapterAttrib[Mat4x2f, DefinedFloat with SysFP](8, RFloat)
  implicit val Mat4x3fAttribs = AdapterAttrib[Mat4x3f, DefinedFloat with SysFP](12, RFloat)
  implicit val Mat4x4fAttribs = AdapterAttrib[Mat4x4f, DefinedFloat with SysFP](16, RFloat)

  implicit val Quat4dAttribs = AdapterAttrib[Quat4d, DefinedDouble with SysFP](4, RFloat, RDouble)
  implicit val Mat2x2dAttribs = AdapterAttrib[Mat2x2d, DefinedDouble with SysFP](4, RFloat, RDouble)
  implicit val Mat2x3dAttribs = AdapterAttrib[Mat2x3d, DefinedDouble with SysFP](6, RFloat, RDouble)
  implicit val Mat2x4dAttribs = AdapterAttrib[Mat2x4d, DefinedDouble with SysFP](8, RFloat, RDouble)
  implicit val Mat3x2dAttribs = AdapterAttrib[Mat3x2d, DefinedDouble with SysFP](6, RFloat, RDouble)
  implicit val Mat3x3dAttribs = AdapterAttrib[Mat3x3d, DefinedDouble with SysFP](9, RFloat, RDouble)
  implicit val Mat3x4dAttribs = AdapterAttrib[Mat3x4d, DefinedDouble with SysFP](12, RFloat, RDouble)
  implicit val Mat4x2dAttribs = AdapterAttrib[Mat4x2d, DefinedDouble with SysFP](8, RFloat, RDouble)
  implicit val Mat4x3dAttribs = AdapterAttrib[Mat4x3d, DefinedDouble with SysFP](12, RFloat, RDouble)
  implicit val Mat4x4dAttribs = AdapterAttrib[Mat4x4d, DefinedDouble with SysFP](16, RFloat, RDouble)
}
