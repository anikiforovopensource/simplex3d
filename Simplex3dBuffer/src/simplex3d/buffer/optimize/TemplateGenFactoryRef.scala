/*
 * Simplex3d, BaseBuffer module
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

import java.util.logging._
import org.objectweb.asm._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] class TemplateGenFactoryRef[E <: Composite, R <: RawType](
  val templateArrayClass: String,
  val templateString: String,
  private val fallbackFactoryRef: CompositeFactoryRef[E, R]
) extends FactoryRef[E, R] {
  def fallbackFactory: DataSeq[E, R] = fallbackFactoryRef.factory
  
  private val replaceString =
    fallbackFactory.bindingType match {
      case RawType.SByte => "SByte"
      case RawType.UByte => "UByte"
      case RawType.SShort => "SShort"
      case RawType.UShort => "UShort"
      case RawType.SInt => "SInt"
      case RawType.UInt => "UInt"
      case RawType.HalfFloat => "HalfFloat"
      case RawType.RawFloat => "RawFloat"
      case RawType.RawDouble => "RawDouble"
    }

  lazy val factory: DataSeq[E, R] = {
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
            "template is compiled without -optimize coplier flag. " +
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
      val template = fallbackFactory.mkDataBuffer(Util.TestData)

      testDataArray(factory.mkDataArray(1))
      testDataArray(
        factory.mkDataArray(fallbackFactory.mkDataArray(1).array)
      )

      testDataBuffer(factory.mkDataBuffer(1))
      testDataBuffer(
        factory.mkDataBuffer(allocateByteBuffer(fallbackFactory.bytesPerRawComponent))
      )

      testDataView(
        factory.mkDataView(
          allocateByteBuffer(fallbackFactory.bytesPerRawComponent),
          0, fallbackFactory.components
        )
      )


      testDataSeq(template, factory.mkDataArray(template.size))
      testDataSeq(template, factory.mkDataArray(
          fallbackFactory.mkDataArray(template.size).array
      ))

      testDataSeq(template, factory.mkDataBuffer(template.size))
      testDataSeq(template, factory.mkDataBuffer(
          allocateByteBuffer(
            template.size*template.components*template.bytesPerRawComponent
          )
      ))

      val offset = 3
      val stride = 5
      testDataSeq(template, factory.mkDataView(
          allocateByteBuffer(
            (offset + template.size*stride)*template.bytesPerRawComponent
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
      allocateByteBuffer(0), 0, fallbackFactory.stride
    )
    assert(fb.asBuffer.getClass == testing.asBuffer.getClass)
  }

  private def testDataSeq(template: DataSeq[E, R], testing: DataSeq[E, R]) {
    assert(
      template(0).asInstanceOf[Object].getClass ==
      testing(0).asInstanceOf[Object].getClass
    )
    assert(template.bindingType == testing.bindingType)
    assert(template.normalized == testing.normalized)

    testApplyUpdate(template, testing)
  }

  private def testApplyUpdate(template: DataSeq[E, R], testing: DataSeq[E, R]) {
    assert(template.size == testing.size)

    var i = 0; while (i < template.size) {
      testing(i) = template(i)
      i += 1
    }

    // check content
    i = 0; while (i < template.size) {
      assert(testing(i) == template(i))
      i += 1
    }

    // check backing content
    val btest = testing.backingSeq
    val btemp = template.backingSeq
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


private[optimize] class BytecodeCheckException(e: Throwable) extends Exception(e)
