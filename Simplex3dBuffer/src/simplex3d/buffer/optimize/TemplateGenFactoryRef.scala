/*
 * Simplex3d, CoreBuffer module
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
package optimize

import java.nio._
import java.util.logging._
import org.objectweb.asm._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] class TemplateGenFactory[E <: Composite, R <: RawData](
  val templateArrayClass: String,
  val templateString: String,
  val fallbackFactory: DataSeq[E, R]
) extends Factory[E, R] {

  def mkDataArray(array: R#ArrayType) :DataArray[E, R] = factory.mkDataArray(array)
  def mkDataArray(size: Int) :DataArray[E, R] = factory.mkDataArray(size)

  def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[E, R] = factory.mkReadDataBuffer(byteBuffer)
  def mkDataBuffer(size: Int) :DataBuffer[E, R] = factory.mkDataBuffer(size)
  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[E, R] = factory.mkDataBuffer(byteBuffer)

  def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) :ReadDataView[E, R] =
    factory.mkReadDataView(byteBuffer, offset, stride)
  def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) :DataView[E, R] =
    factory.mkDataView(byteBuffer, offset, stride)

  override def emptyMarker() :DataSeq[E, R] = factory

  
  private val replaceString =
    fallbackFactory.rawType match {
      case SByte => "SByte"
      case UByte => "UByte"
      case SShort => "SShort"
      case UShort => "UShort"
      case SInt => "SInt"
      case UInt => "UInt"
      case HalfFloat => "HalfFloat"
      case RawFloat => "RawFloat"
      case RawDouble => "RawDouble"
    }

  val factory: DataSeq[E, R] = {
    if (!Util.enableTemplateGen) fallbackFactory
    
    else {
      try {
        genAndLoad(templateArrayClass.replace("Array", "Buffer"))
        genAndLoad(templateArrayClass.replace("Array", "View"))
        val arrayClass = genAndLoad(templateArrayClass)
        val factory = arrayClass.newInstance().asInstanceOf[DataArray[E, R]]
        testFactory(factory)
        factory
      }
      catch {
        case badcode: BytecodeCheckException =>
          Logger.getLogger(getClass.getName).log(
            Level.WARNING,
            "Wrong bytecode for '" + replaceString +
            "' from template '" + templateArrayClass + "'. Ensure that the " +
            "template is compiled without -optimize compiler flag. " +
            Util.helpMsg,
            badcode.getCause
          )
          fallbackFactory
          
        case any =>
          Logger.getLogger(getClass.getName).log(
            Level.WARNING,
            "Failed to load optimized classes for '" + replaceString +
            "' from template '" + templateArrayClass + "'." + Util.helpMsg,
            any
          )
          fallbackFactory
      }
    }
  }

  private def genAndLoad(templateClassName: String) :Class[_] = {
    val genClassName = templateClassName.replace(templateString, replaceString)

    try {
      Util.DefineClassLoader.loadClass(genClassName)
    }
    catch {
      case noclass: ClassNotFoundException =>

        val byteCode = Util.TemplateGen.genByteCode(
          templateClassName, templateString, replaceString
        )

        Util.DefineClassLoader.define(
          genClassName, byteCode, 0, byteCode.length
        )
    }
  }

  private def testFactory(factory: DataSeq[E, R]) {
    try {
      val fallback = fallbackFactory.mkDataBuffer(Util.TestData)

      testDataArray(factory.mkDataArray(1))
      testDataArray(
        factory.mkDataArray(fallbackFactory.mkDataArray(1).array)
      )

      testDataBuffer(factory.mkDataBuffer(1))
      testDataBuffer(
        factory.mkDataBuffer(ByteBuffer.allocateDirect(fallbackFactory.bytesPerRawComponent))
      )

      testDataView(
        factory.mkDataView(
          ByteBuffer.allocateDirect(fallbackFactory.bytesPerRawComponent),
          0, fallbackFactory.components
        )
      )


      testDataSeq(fallback, factory.mkDataArray(fallback.size))
      testDataSeq(fallback, factory.mkDataArray(
          fallbackFactory.mkDataArray(fallback.size).array
      ))

      testDataSeq(fallback, factory.mkDataBuffer(fallback.size))
      testDataSeq(fallback, factory.mkDataBuffer(
          ByteBuffer.allocateDirect(
            fallback.size*fallback.components*fallback.bytesPerRawComponent
          )
      ))

      val offset = 1
      val stride = 5
      testDataSeq(fallback, factory.mkDataView(
          ByteBuffer.allocateDirect(
            (offset + fallback.size*stride)*fallback.bytesPerRawComponent
          ),
          offset,
          stride
      ))
    }
    catch {
      case any => throw new BytecodeCheckException(any)
    }
  }

  private def testDataArray(testing: DataArray[E, R]) {
    assert(testing.isInstanceOf[DataArray[_, _]])

    val fb = fallbackFactory.mkDataArray(1)
    assert(fb.array.getClass == testing.array.getClass)
    assert(fb.asBuffer.getClass == testing.asBuffer.getClass)
  }

  private def testDataBuffer(testing: DataBuffer[E, R]) {
    assert(testing.isInstanceOf[DataBuffer[_, _]])

    val fb = fallbackFactory.mkDataBuffer(1)
    assert(fb.asBuffer.getClass == testing.asBuffer.getClass)
  }

  private def testDataView(testing: DataView[E, R]) {
    assert(testing.isInstanceOf[DataView[_, _]])
    
    val fb = fallbackFactory.mkDataView(
      ByteBuffer.allocateDirect(0), 0, fallbackFactory.stride
    )
    assert(fb.asBuffer.getClass == testing.asBuffer.getClass)
  }

  private def testDataSeq(fallback: DataSeq[E, R], testing: DataSeq[E, R]) {
    assert(
      fallback(0).asInstanceOf[Object].getClass ==
      testing(0).asInstanceOf[Object].getClass
    )
    assert(fallback.rawType == testing.rawType)
    assert(fallback.normalized == testing.normalized)

    testApplyUpdate(fallback, testing)
  }

  private def testApplyUpdate(fallback: DataSeq[E, R], testing: DataSeq[E, R]) {
    assert(fallback.size == testing.size)

    var i = 0; while (i < fallback.size) {
      testing(i) = fallback(i)
      i += 1
    }

    // check content
    i = 0; while (i < fallback.size) {
      assert(testing(i) == fallback(i))
      i += 1
    }

    // check backing content
    val btest = testing.backingSeq
    val btemp = fallback.backingSeq
    var c = 0
    i = testing.offset; while (i < btest.size) {
      var j = 0; while (j < testing.components) {
        assert(btest(i + j) == btemp(c))
        j += 1
        c += 1
      }
      i += testing.stride
    }
  }
}
