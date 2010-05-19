/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
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
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] class TemplateGenFactoryRef[T <: MetaType, D <: RawType](
  val templateArray: String,
  val templateString: String,
  val fallbackFactory: DataSeq[T, D]
) extends FactoryRef[T, D] {
  
  private final val sysprop = "simplex3d.buffer.optimize"
  private val replaceString =
    (if (fallbackFactory.normalized) "N" else "") +
    (fallbackFactory.componentBinding match {
      case Binding.SByte => "SByte"
      case Binding.UByte => "UByte"
      case Binding.SShort => "SShort"
      case Binding.UShort => "UShort"
      case Binding.SInt => "SInt"
      case Binding.UInt => "UInt"
      case Binding.HalfFloat => "HalfFloat"
      case Binding.RawFloat => "RawFloat"
      case Binding.RawDouble => "RawDouble"
    })

  private def disableWarningMsg =
    "Disable buffer optimization by setting system property '" +
    sysprop + "' to false."

  private def enableGen :Boolean = {
    val default = "true"
    (try {
      System.getProperty(sysprop, default)
    } catch {
      case se: SecurityException => default
    }).toLowerCase != "false"
  }

  private def reportMissingLib :Boolean = {
    (try {
      System.getProperty(sysprop)
    } catch {
      case se: SecurityException => "set"
    }) != null
  }

  def factory = ref
  private lazy val ref: DataSeq[T, D] = {
    if (!enableGen) fallbackFactory
    else {
      try {
        genAndLoad(templateArray.replace("Array", "Buffer"))
        genAndLoad(templateArray.replace("Array", "View"))
        val factory = genAndLoad(templateArray)
        testFactory(factory)
        factory
      }
      catch {
        case nolib: NoLibException =>
          if (reportMissingLib) {
            Logger.getLogger(getClass.getName).log(
              Level.WARNING,
              "Unable to load optimized classes due to missing asm library. " +
              disableWarningMsg,
              nolib.getCause
            )
          }
          fallbackFactory
        case badcode: BytecodeCheckException =>
          Logger.getLogger(getClass.getName).log(
            Level.WARNING,
            "Wrong bytecode for '" + replaceString +
            "' from template '" + templateArray + "'. Ensure that the " +
            "template is compiled without -optimize coplier flag. " +
            disableWarningMsg,
            badcode.getCause
          )
          fallbackFactory
        case any =>
          Logger.getLogger(getClass.getName).log(
            Level.WARNING,
            "Failed to load optimized classes for '" + replaceString +
            "' from template '" + templateArray + "'." + disableWarningMsg,
            any
          )
          fallbackFactory
      }
    }
  }

  private def genAndLoad(templateClassName: String) :DataSeq[T, D] = {
    val genClassName = templateClassName.replace(templateString, replaceString)

    val clazz =
    try {
      DefineClassLoader.get.loadClass(genClassName)
    }
    catch {
      case noclass: ClassNotFoundException =>

        val templateGen =
        try {
          Class.forName(
            "simplex3d.buffer.optimize.TemplateGen"
          ).newInstance.asInstanceOf[
            {
              def genByteCode(
                templateClassName: String,
                templateString: String,
                replaceString: String
              ) :Array[Byte]
            }
          ]
        } catch {
          case ce: NoClassDefFoundError => throw new NoLibException(ce)
        }

        val byteCode = templateGen.genByteCode(
          templateClassName, templateString, replaceString
        )

        DefineClassLoader.get.define(
          genClassName, byteCode, 0, byteCode.length
        )
    }
    val factory = clazz.newInstance.asInstanceOf[DataSeq[T, D]]
    factory
  }

  private def testFactory(factory: DataSeq[T, D]) {
    try {
      val template = fallbackFactory.mkDataBuffer(TestData.data)

      testDataArray(factory.mkDataArray(1))
      testDataArray(
        factory.mkDataArray(fallbackFactory.mkDataArray(1).array)
      )

      testDataBuffer(factory.mkDataBuffer(1))
      testDataBuffer(
        factory.mkDataBuffer(fallbackFactory.mkDataBuffer(1).byteBuffer)
      )

      testDataView(
        factory.mkDataView(fallbackFactory.mkDataBuffer(1).byteBuffer, 0, 0)
      )


      testDataSeq(template, factory.mkDataArray(template.size))
      testDataSeq(template, factory.mkDataArray(
          fallbackFactory.mkDataArray(template.size).array
      ))

      testDataSeq(template, factory.mkDataBuffer(template.size))
      testDataSeq(template, factory.mkDataBuffer(
          allocateByteBuffer(
            template.size*template.components*template.componentBytes
          )
      ))

      val offset = 3
      val stride = 5
      testDataSeq(template, factory.mkDataView(
        allocateByteBuffer(
            offset +
            template.size*(template.components + stride)*template.componentBytes
        ),
          offset,
          stride
      ))
    }
    catch {
      case any => throw new BytecodeCheckException(any)
    }
  }

  private def testDataArray(testing: DataArray[T, D]) {
    val fb = fallbackFactory.mkDataArray(1)
    assert(testing.isInstanceOf[DataArray[T, D]])
    assert(fb.array.getClass == testing.array.getClass)
    assert(fb.buffer.getClass == testing.buffer.getClass)
  }

  private def testDataBuffer(testing: DataBuffer[T, D]) {
    val fb = fallbackFactory.mkDataBuffer(1)
    assert(testing.isInstanceOf[DataBuffer[T, D]])
    assert(fb.buffer.getClass == testing.buffer.getClass)
  }

  private def testDataView(testing: DataView[T, D]) {
    val fb = fallbackFactory.mkDataView(allocateByteBuffer(8), 0, 0)
    assert(testing.isInstanceOf[DataView[T, D]])
    assert(fb.buffer.getClass == testing.buffer.getClass)
  }

  private def testDataSeq(template: DataSeq[T, D], testing: DataSeq[T, D]) {
    assert(
      template(0).asInstanceOf[Object].getClass ==
      testing(0).asInstanceOf[Object].getClass
    )
    assert(template.componentBinding == testing.componentBinding)
    assert(template.normalized == testing.normalized)

    testApplyUpdate(template, testing)
  }

  private def testApplyUpdate(template: DataSeq[T, D], testing: DataSeq[T, D]) {
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
      i += testing.step
    }
  }
}

private[optimize] object TestData {
  val data = allocateByteBuffer(20*4)

  {
    /* Positive and negative Int, Short, Byte, Float and Double.
     * Float and Double with absolute value less than 1.
     */
    val db = data.asDoubleBuffer()
    db.put(0, -55)
    db.put(1, 55)
    db.put(2, -0.55)
    db.put(3, 0.55)
    val fb = data.asFloatBuffer()
    fb.put(8, -55)
    fb.put(9, 55)
    fb.put(10, -0.55f)
    fb.put(11, 0.55f)
    val ib = data.asIntBuffer()

    /* A pattern that simulates negative Int, Short, and Byte
     * while avoiding HalfFloat.NaN.
     */
    ib.put(12, 0xAAAAAAAA)
    ib.put(13, 55)

    // pad 3 components of max width
    ib.put(14, 0)
    ib.put(15, 0)
    ib.put(16, 0)
    ib.put(17, 0)
    ib.put(18, 0)
    ib.put(19, 0)
  }
}

private[optimize] object DefineClassLoader {
  val get = new DefineClassLoader(this.getClass.getClassLoader)
}

private[optimize] class DefineClassLoader(parent: ClassLoader)
extends ClassLoader(parent) {
   def define(name: String, b: Array[Byte], off: Int, len: Int) :Class[_] =
     defineClass(name, b, off, len)
}

private[optimize] class NoLibException(e: Throwable) extends Exception(e)
private[optimize] class BytecodeCheckException(e: Throwable) extends Exception(e)