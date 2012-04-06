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

import java.nio._
import scala.annotation._
import simplex3d.math._
import simplex3d.data.extension._
import StoreType._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadContiguous[F <: Format, +R <: Raw]
extends ReadDataSeq[F, R] with ContiguousSrc {
  type Read <: ReadContiguous[F, R]
}

trait Contiguous[F <: Format, +R <: Raw]
extends DataSeq[F, R] with ReadContiguous[F, R] {
  
  /** This will copy a 2d sub image from the source Data object into this object.
   */
  def put2d(
    dimensions: inVec2i, offset: inVec2i,
    src: inContiguous[F, simplex3d.data.Raw], srcDimensions: inVec2i
  ) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "ReadContiguous[" + src.formatManifest + ", _] cannot be cast to ReadContiguous[" + formatManifest + ", _]."
      )
    
    put2dImpl(
      dimensions, offset,
      src, srcDimensions, Vec2i.Zero,
      srcDimensions
    )
  }
  
  /** This will copy a 2d sub image from the source Data object into this object.
   */
  def put2d(
    dimensions: inVec2i, offset: inVec2i,
    src: inContiguous[F, simplex3d.data.Raw], srcDimensions: inVec2i, srcOffset: inVec2i,
    copyDimensions: inVec2i
  ) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "ReadContiguous[" + src.formatManifest + ", _] cannot be cast to ReadContiguous[" + formatManifest + ", _]."
      )
    
    put2dImpl(
      dimensions, offset,
      src, srcDimensions, srcOffset,
      copyDimensions
    )
  }
  
  
  /** This will copy a 3d sub image from the source Data object into this object.
   */
  def put3d(
    dimensions: inVec3i, offset: inVec3i,
    src: inContiguous[F, simplex3d.data.Raw], srcDimensions: inVec3i
  ) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "ReadContiguous[" + src.formatManifest + ", _] cannot be cast to ReadContiguous[" + formatManifest + ", _]."
      )
    
    put3dImpl(
      dimensions, offset,
      src, srcDimensions, Vec3i.Zero,
      srcDimensions
    )
  }
  
  /** This will copy a 3d sub image from the source Data object into this object.
   */
  def put3d(
    dimensions: inVec3i, offset: inVec3i,
    src: inContiguous[F, simplex3d.data.Raw], srcDimensions: inVec3i, srcOffset: inVec3i,
    copyDimensions: inVec3i
  ) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "ReadContiguous[" + src.formatManifest + ", _] cannot be cast to ReadContiguous[" + formatManifest + ", _]."
      )
    
    put3dImpl(
      dimensions, offset,
      src, srcDimensions, srcOffset,
      copyDimensions
    )
  }
}


object ReadContiguous {
  def apply[F <: Format, R <: Tangible](dc: ReadContiguous[_, R])(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :ReadContiguous[F, R] = {
    val res = dc match {
      case d: DataArray[_, _] => composition.mkDataArray(primitives.mkDataArray(dc.sharedStorage.asInstanceOf[R#Array]))
      case d: DataBuffer[_, _] => composition.mkDataBuffer(primitives.mkDataBuffer(dc.sharedBuffer))
    }
    if (dc.isReadOnly) res.asReadOnly() else res
  }
}

object Contiguous {
  def apply[F <: Format, R <: Tangible](dc: Contiguous[_, R])(
    implicit composition: CompositionFactory[F, _ >: R], primitives: PrimitiveFactory[F#Component, R]
  ) :Contiguous[F, R] = {
    if (dc.isReadOnly) throw new IllegalArgumentException(
      "The data source must not be read-only."
    )
    dc match {
      case d: DataArray[_, _] => composition.mkDataArray(primitives.mkDataArray(dc.sharedStorage.asInstanceOf[R#Array]))
      case d: DataBuffer[_, _] => composition.mkDataBuffer(primitives.mkDataBuffer(dc.sharedBuffer))
    }
  }
}
