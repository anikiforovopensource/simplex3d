/*
 * Simplex3dData - Core Module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data

import scala.annotation.unchecked._
import simplex3d.data.extension._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadDataSeq[F <: Format, +R <: Raw] extends ReadData[F#Accessor] with DataFactory[F, R] {
  type Read <: ReadDataSeq[F, R]
  
  type Format = F
  type Raw = R @uncheckedVariance
  
  
  final def copyAsDataArray() :DataArray[Format, Raw] = {
    val copy = mkDataArray(size)
    copy.putPrimitives(0, primitives, this.offset, this.stride, size)
    copy
  }
  final def copyAsDataBuffer() :DataBuffer[Format, Raw] = {
    val copy = mkDataBuffer(size)
    copy.putPrimitives(0, primitives, this.offset, this.stride, size)
    copy
  }
}

trait DataSeq[F <: Format, +R <: Raw] extends Data[F#Accessor] with ReadDataSeq[F, R] {
  final def putPrimitives(
    index: Int,
    src: inContiguous[F#Component, simplex3d.data.Raw],
    srcOffset: Int, srcStride: Int, count: Int
  ) {
    if ((primitives.accessorManifest ne src.accessorManifest) && (primitives.accessorManifest != src.accessorManifest))
      throw new ClassCastException(
        "ReadData[" + src.accessorManifest + "] cannot be cast to ReadData[" + primitives.accessorManifest + "]."
      )
    
    putPrimitivesImpl(index, src, srcOffset, srcStride, count)
  }

  final def putPrimitives(index: Int, src: inContiguous[F#Component, simplex3d.data.Raw]) {
    if ((primitives.accessorManifest ne src.accessorManifest) && (primitives.accessorManifest != src.accessorManifest))
      throw new ClassCastException(
        "ReadData[" + src.accessorManifest + "] cannot be cast to ReadData[" + primitives.accessorManifest + "]."
      )
    
    putPrimitivesImpl(index, src, 0, components, src.size/components)
  }

  final def putPrimitives(src: inContiguous[F#Component, simplex3d.data.Raw]) {
    if ((primitives.accessorManifest ne src.accessorManifest) && (primitives.accessorManifest != src.accessorManifest))
      throw new ClassCastException(
        "ReadData[" + src.accessorManifest + "] cannot be cast to ReadData[" + primitives.accessorManifest + "]."
      )
    
    putPrimitivesImpl(0, src, 0, components, src.size/components)
  }

  final def put(index: Int, src: inDataSeq[F, simplex3d.data.Raw], first: Int, count: Int) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "ReadDataSeq[" + src.formatManifest + ", _] cannot be cast to ReadDataSeq[" + formatManifest + ", _]."
      )

    putPrimitivesImpl(index, src.primitives, src.offset + first*src.stride, src.stride, count)
  }

  final def put(index: Int, src: inDataSeq[F, simplex3d.data.Raw]) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "ReadDataSeq[" + src.formatManifest + ", _] cannot be cast to ReadDataSeq[" + formatManifest + ", _]."
      )

    putPrimitivesImpl(index, src.primitives, src.offset, src.stride, src.size)
  }

  final def put(src: inDataSeq[F, simplex3d.data.Raw]) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "ReadDataSeq[" + src.formatManifest + ", _] cannot be cast to ReadDataSeq[" + formatManifest + ", _]."
      )

    putPrimitivesImpl(0, src.primitives, src.offset, src.stride, src.size)
  }
}

object DataSeq {
  def apply[F <: Format, R <: Raw with Tangible](
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :DataSeq[F, R] = {
    composition.mkDataArray(primitives.mkDataArray(0))
  }
}
