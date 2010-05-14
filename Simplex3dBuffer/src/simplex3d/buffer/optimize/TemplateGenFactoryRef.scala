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
  private def disableWarningMsg =
    "Disable this warning by setting system property '" +
    sysprop + "' to false."

  private val replaceString =
    (if (fallbackFactory.normalized) "N" else "") +
    (fallbackFactory.componentBinding match {
      case Binding.SByte => "SByte"
      case Binding.UByte => "UByte"
      case Binding.SShort => "SShort"
      case Binding.UShort => "UShort"
      case Binding.SInt => "SInt"
      case Binding.UInt => "UInt"
      case Binding.RawFloat => "RawFloat"
      case Binding.RawDouble => "RawDouble"
    })

  private lazy val ref: DataSeq[T, D] = {
    val enableGen = ((
      try {
        System.getProperty(sysprop)
      } catch {
        case se: SecurityException => "true"
      }
    ).toLowerCase != "false")

    if (!enableGen) fallbackFactory
    else {
      try {
        genAndLoad(templateArray.replace("Array", "Buffer"))
        genAndLoad(templateArray.replace("Array", "View"))
        genAndLoad(templateArray)
      }
      catch {
        case nl: NoLibException =>
          Logger.getLogger(getClass.getName).log(
            Level.WARNING,
            "Unable to load optimized classes due to missing asm library. " +
            disableWarningMsg,
            nl.getCause
          )
          fallbackFactory
        case e =>
          Logger.getLogger(getClass.getName).log(
            Level.WARNING,
            "Failed to load optimized classes for '" + replaceString +
            "' from template '" + templateArray + "'." + disableWarningMsg,
            e
          )
          fallbackFactory
      }
    }
  }

  def factory = ref

  private def genAndLoad(templateClassName: String) :DataSeq[T, D] = {
    val genClassName = templateClassName.replace(templateString, replaceString)

    val clazz =
    try {
      DefineClassLoader.get.loadClass(genClassName)
    }
    catch {
      case ex: ClassNotFoundException =>

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
    testFactory(factory)
    factory
  }

  private def testFactory(factory: DataSeq[T, D]) {

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
