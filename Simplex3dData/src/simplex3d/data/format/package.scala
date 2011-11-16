/*
 * Simplex3dData - Format Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dAlgorithm.
 *
 * Simplex3dAlgorithm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dAlgorithm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.data.conversion.Double._


package object format {

  @inline private final def from2Bits(x: Int) :Double = {
    (x & 0x3)*0.33333333333333333333
  }
  @inline private final def to2Bits(x: Double) :Int = {
    val c = if (x <= 0) 0 else if (x >= 1) 1 else x // clamp
    val s = c*3 // scale
    (s + 0.5).toInt // round
  }
  
  
  @inline private final def from10Bits(x: Int) :Double = {
    (x & 0x3FF)*0.00097751710654936461
  }
  @inline private final def to10Bits(x: Double) :Int = {
    val c = if (x <= 0) 0 else if (x >= 1) 1 else x // clamp
    val s = c*1023 // scale
    (s + 0.5).toInt // round
  }
  
  
  sealed trait BGR extends CompositeFormat {
    type Accessor = Vec3
    type Component = RDouble
  }
  implicit object BGRAdapter extends DataAdapter[BGR, DefinedDouble](components = 3) {
    def apply(backing: inContiguous[RDouble, Raw], j: Int) :ConstVec3 = {
      ConstVec3(
        backing(j + 2),
        backing(j + 1),
        backing(j)
      )
    }
    def update(backing: Contiguous[RDouble, Raw], j: Int, value: ReadVec3) {
      backing(j) = value.b
      backing(j + 1) = value.g
      backing(j + 2) = value.r
    }
  }
  
  
  /** This format works with arrays retrieved from BufferedImage.
   */ 
  sealed trait ARGB8 extends CompositeFormat {
    type Accessor = Vec4
    type Component = SInt
  }
  implicit object ARGB8Adapter extends DataAdapter[ARGB8, UInt](components = 1) {
    def apply(backing: inContiguous[SInt, Raw], j: Int) :ConstVec4 = {
      val i = backing(j)
      ConstVec4(
        fromUByte((i >> 16).toByte),
        fromUByte((i >> 8).toByte),
        fromUByte(i.toByte),
        fromUByte((i >> 24).toByte)
      )
    }
    def update(backing: Contiguous[SInt, Raw], j: Int, value: ReadVec4) {
      backing(j) =
        ((toUByte(value.a) & 0xFF) << 24) |
        ((toUByte(value.r) & 0xFF) << 16) |
        ((toUByte(value.g) & 0xFF) << 8) |
        ((toUByte(value.b) & 0xFF))
    }
  }
  
  
  sealed trait RGB10A2 extends CompositeFormat {
    type Accessor = Vec4
    type Component = SInt
  }
  implicit object RGB10A2Adapter extends DataAdapter[RGB10A2, UInt](components = 1) {
    def apply(backing: inContiguous[SInt, Raw], j: Int) :ConstVec4 = {
      val i = backing(j)
      ConstVec4(
        from10Bits(i >> 22),
        from10Bits(i >> 12),
        from10Bits(i >> 2),
        from2Bits(i)
      )
    }
    def update(backing: Contiguous[SInt, Raw], j: Int, value: ReadVec4) {
      backing(j) =
        (to10Bits(value.r) << 22) |
        (to10Bits(value.g) << 12) |
        (to10Bits(value.b) << 2) |
        (to2Bits(value.a))
    }
  }
}
