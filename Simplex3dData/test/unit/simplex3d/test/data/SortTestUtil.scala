/*
 * Simplex3dData - Test Package
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.test.data

import org.scalatest._
import simplex3d.data._
import simplex3d.data.extension._
import simplex3d.math._
import simplex3d.math.double.functions._
import TestUtil._
import CopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SortTestUtil extends FunSuite {
  
  def testSort[F <: Format, R <: Raw](
    factory: DataFactory[F, R]
  )(implicit descriptor: Descriptor[F, R])
  {
    pending
  }
}
