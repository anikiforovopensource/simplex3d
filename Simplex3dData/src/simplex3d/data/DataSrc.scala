/*
 * Simplex3d, CoreData module
 * Copyright (C) 2011, Aleksey Nikiforov
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


/**
 * @author Aleksey Nikiforov (lex)
 */
trait DataSrc {
  type Read <: DataSrc
  def asReadOnly() :Read
  
  type Format <: simplex3d.data.Format
  def formatManifest: ClassManifest[Format]
  def metaManifest: ClassManifest[Format#Accessor]
  
  def components: Int
  def rawType: Int
  def isNormalized: Boolean

  type BindingBuffer <: Buffer
  
  /** Binding buffer can be direct or non-direct. It can even be a mapped file.
   * If not cached a new buffer is loaded on demand, the buffer may or may not be cached after that.
   * The buffer contents may be compressed or encoded depending on the Format.
   */
  def bindingBuffer() :BindingBuffer
  def sharesStorageWith(d: DataSrc) :Boolean
  
  def byteOffset: Int
  def byteStride: Int
  def byteCapacity: Int
  
  def size: Int
  def isCached :Boolean
}

trait ContiguousSrc extends DataSrc {
  type Read <: ContiguousSrc
}

trait DirectSrc extends DataSrc {
  type Read <: DirectSrc
  
  type BindingBuffer = ByteBuffer
}
