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


// An empty class to make -Xno-forwarders work
private[optimize] class Util


/**
 * @author Aleksey Nikiforov (lex)
 */
private[optimize] object Util {
  private final val syspropName = "simplex3d.buffer.optimize"
  private final val UnsetOrUnknown = "unset/unknown"

  private val syspropValue :String = {
    try {
      System.getProperty(syspropName, UnsetOrUnknown)
    } catch {
      case any =>
        Logger.getLogger(getClass.getName).log(
          Level.WARNING,
          "Unable to read the system property '" + syspropName + "'.",
          any
        )
        UnsetOrUnknown
    }
  }

  private val syspropEnabled: Boolean = {
    (syspropValue == UnsetOrUnknown) || (syspropValue.toLowerCase != "false")
  }

  val helpMsg =
    "Disable buffer optimization by setting the system property '" +
    syspropName + "' to false."


  def enableTemplateGen :Boolean = {
    syspropEnabled && (TemplateGen != null) && (DefineClassLoader != null)
  }

  val TemplateGen: TemplateGen = {
    try {
      Class.forName(
        "simplex3d.buffer.optimize.TemplateGenImpl"
      ).newInstance().asInstanceOf[TemplateGen]
    } catch {
      case any =>
        if (syspropEnabled && syspropValue != UnsetOrUnknown) {
          Logger.getLogger(getClass.getName).log(
            Level.WARNING,
            "Unable to load optimized classes due to a missing asm library. " +
            helpMsg,
            any
          )
        }
        null
    }
  }

  val DefineClassLoader: DefineClassLoader = {
    try {
      new DefineClassLoader(this.getClass.getClassLoader)
    } catch {
      case any =>
        if (syspropEnabled) {
          Logger.getLogger(getClass.getName).log(
            Level.WARNING,
            "Unable to create a custom classloader to load optimized classes. "+
            helpMsg,
            any
          )
        }
        null
    }
  }


  val TestData = allocateDirectBuffer(20*4);
  {
    /* Positive and negative Int, Short, Byte, Float and Double;
     * Float and Double with absolute value less than 1.
     */
    val db = TestData.asDoubleBuffer()
    db.put(0, -55)
    db.put(1, 55)
    db.put(2, -0.55)
    db.put(3, 0.55)
    val fb = TestData.asFloatBuffer()
    fb.put(8, -55)
    fb.put(9, 55)
    fb.put(10, -0.55f)
    fb.put(11, 0.55f)
    val ib = TestData.asIntBuffer()

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

private[optimize] class DefineClassLoader(parent: ClassLoader)
extends ClassLoader(parent) {
   def define(name: String, b: Array[Byte], off: Int, len: Int) :Class[_] =
     defineClass(name, b, off, len)
}
