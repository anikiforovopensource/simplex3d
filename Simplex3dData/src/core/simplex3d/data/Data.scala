/*
 * Simplex3dData - Core Module
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

import scala.collection._
import simplex3d.data.extension._


/**
 * @author Aleksey Nikiforov (lex)
 */
trait ReadData[A <: Accessor] extends ReadAbstractData[A#Const] {
  type Read <: ReadData[A]
  
  type Format <: simplex3d.data.Format { type Accessor <: A }
}

trait Data[A <: Accessor] extends AbstractData[A#Const, A#Read] with ReadData[A]
