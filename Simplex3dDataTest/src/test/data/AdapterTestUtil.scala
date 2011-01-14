/*
 * Simplex3d, DataTest package
 * Copyright (C) 2011, Simplex3d Team
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
import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AdapterTestUtil extends FunSuite {

  def testAdapter[E <: Composite, B <: Defined](adapter: DataAdapter[E, B])(
    sample: E#Read, sampleData: DataArray[E#Component, Raw]
  )(implicit attribs: AdapterAttrib[E, B])
  {
    // Test attributes.
    assert(adapter.components == attribs.components)
    assert(adapter.elemManifest == attribs.elemManifest)
    assert(adapter.readManifest == attribs.readManifest)
    assert(adapter.boundManifest == attribs.boundManifest)

    // Test make.
    val primSize = 10*adapter.components
    for (readOnly <- List(true, false); rawManifest <- attribs.allowed) {

      type R = T forSome { type T <: B }
      val original = genRandomSeq(
        attribs.componentManifest, RawManifest.toRawType(rawManifest), primSize
      ).asInstanceOf[ReadDataArray[E#Component, R]]

      val descriptor = Descriptor[E, R](
        attribs.elemManifest, attribs.componentManifest, attribs.readManifest,
        attribs.components, original.rawType, original.normalized
      )

      val primitiveArray =
        if (readOnly) original.copyAsDataArray().asReadOnly().asInstanceOf[DataArray[E#Component, R]]
        else original.copyAsDataArray()

      val primitiveBuffer =
        if (readOnly) original.copyAsDataBuffer().asReadOnly().asInstanceOf[DataBuffer[E#Component, R]]
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
      val primitive = genRandomSeq(
        attribs.componentManifest, RawManifest.toRawType(attribs.componentManifest), 0
      ).asInstanceOf[ReadDataArray[E#Component, R]]
      adapter.mkReadDataArray(primitive).asInstanceOf[CompositionFactory[E, B]]
    }

    // Test raw types that are not allowed.
    for (
      raw <- RawManifest.AllDefined
      if !attribs.allowed.contains(raw) && supportsRawType(attribs.componentManifest, raw)
    ) {
      type R = T forSome { type T <: B }

      val array = genRandomSeq(
        attribs.componentManifest, RawManifest.toRawType(raw), primSize
      ).asInstanceOf[DataArray[E#Component, R]]

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
    assert(sampleData.elemManifest == attribs.componentManifest)

    val j = 1
    val writeBuffer = genRandomSeq(
      attribs.componentManifest, sampleData.rawType, sampleData.size + j
    ).asInstanceOf[Contiguous[E#Component, Raw]]

    assert(adapter.apply(sampleData, 0) == sample)
    assert(adapter.apply(writeBuffer, j) != sample)
    adapter.update(writeBuffer, j, sample)
    assert(adapter.apply(writeBuffer, j) == sample)
  }
}

case class AdapterAttrib[E <: Meta, B <: Defined](components: Int, allowed: ClassManifest[_ <: Defined]*)(implicit
  val elemManifest: ClassManifest[E],
  val readManifest: ClassManifest[E#Read],
  val boundManifest: Manifest[B],
  val componentManifest: ClassManifest[E#Component]
)

object AdapterAttribs {
  import RawManifest._
  
  implicit val Mat2x2fAttribs = AdapterAttrib[Mat2x2f, DefinedFloat with SystemFloatingPoint](4, RFloat)
  implicit val Mat2x3fAttribs = AdapterAttrib[Mat2x3f, DefinedFloat with SystemFloatingPoint](6, RFloat)
  implicit val Mat2x4fAttribs = AdapterAttrib[Mat2x4f, DefinedFloat with SystemFloatingPoint](8, RFloat)
  implicit val Mat3x2fAttribs = AdapterAttrib[Mat3x2f, DefinedFloat with SystemFloatingPoint](6, RFloat)
  implicit val Mat3x3fAttribs = AdapterAttrib[Mat3x3f, DefinedFloat with SystemFloatingPoint](9, RFloat)
  implicit val Mat3x4fAttribs = AdapterAttrib[Mat3x4f, DefinedFloat with SystemFloatingPoint](12, RFloat)
  implicit val Mat4x2fAttribs = AdapterAttrib[Mat4x2f, DefinedFloat with SystemFloatingPoint](8, RFloat)
  implicit val Mat4x3fAttribs = AdapterAttrib[Mat4x3f, DefinedFloat with SystemFloatingPoint](12, RFloat)
  implicit val Mat4x4fAttribs = AdapterAttrib[Mat4x4f, DefinedFloat with SystemFloatingPoint](16, RFloat)

  implicit val Mat2x2dAttribs = AdapterAttrib[Mat2x2d, DefinedDouble with SystemFloatingPoint](4, RFloat, RDouble)
  implicit val Mat2x3dAttribs = AdapterAttrib[Mat2x3d, DefinedDouble with SystemFloatingPoint](6, RFloat, RDouble)
  implicit val Mat2x4dAttribs = AdapterAttrib[Mat2x4d, DefinedDouble with SystemFloatingPoint](8, RFloat, RDouble)
  implicit val Mat3x2dAttribs = AdapterAttrib[Mat3x2d, DefinedDouble with SystemFloatingPoint](6, RFloat, RDouble)
  implicit val Mat3x3dAttribs = AdapterAttrib[Mat3x3d, DefinedDouble with SystemFloatingPoint](9, RFloat, RDouble)
  implicit val Mat3x4dAttribs = AdapterAttrib[Mat3x4d, DefinedDouble with SystemFloatingPoint](12, RFloat, RDouble)
  implicit val Mat4x2dAttribs = AdapterAttrib[Mat4x2d, DefinedDouble with SystemFloatingPoint](8, RFloat, RDouble)
  implicit val Mat4x3dAttribs = AdapterAttrib[Mat4x3d, DefinedDouble with SystemFloatingPoint](12, RFloat, RDouble)
  implicit val Mat4x4dAttribs = AdapterAttrib[Mat4x4d, DefinedDouble with SystemFloatingPoint](16, RFloat, RDouble)
}
