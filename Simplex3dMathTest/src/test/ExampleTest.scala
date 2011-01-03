/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2011, Simplex3d Team
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test

import org.scalatest._


/**
 * @author Aleksey Nikiforov (lex)
 */
class ExampleTest extends FunSuite {

  test ("Example") {
    assert(true)
    expect (1) { 3 - 2 }

    info("example info")

    intercept[IllegalArgumentException] {
      throw new IllegalArgumentException()
    }
  }

  test("Pending test example") {
    pending
  }
}
