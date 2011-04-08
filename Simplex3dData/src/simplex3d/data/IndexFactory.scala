/*
 * Simplex3d, CoreData module
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
import scala.annotation.unchecked._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait IndexFactory[+R <: Unsigned] extends DataFactory[SInt, R] {
  def mkReadIndexBuffer(byteBuffer: ByteBuffer) :ReadIndexBuffer[R] =
    mkReadDataBuffer(byteBuffer).asInstanceOf[ReadIndexBuffer[R]]

  def mkIndexArray(size: Int) :IndexArray[R] = mkDataArray(size)
  def mkIndexArray(array: R#Array @uncheckedVariance) :IndexArray[R] = mkDataArray(array)
  def mkIndexBuffer(size: Int) :IndexBuffer[R] = mkDataBuffer(size)
  def mkIndexBuffer(byteBuffer: ByteBuffer) :IndexBuffer[R] = mkDataBuffer(byteBuffer)
}
