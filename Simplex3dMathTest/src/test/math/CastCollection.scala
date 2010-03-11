/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010 Simplex3d Team
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

package test.math

import org.scalatest._

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
class CastCollection extends FunSuite {

  // These tests pass if they compile.
  test("Cast Collection") {
    {
      val m = Vec2b(true)
      val c: ConstVec2b = m

      val tam: AnyVec[Boolean] = m
      val tac: AnyVec[Boolean] = c
      val tn: Vec[Boolean] = m
      val tc: ConstVec[Boolean] = c
    }
    {
      val m = Vec3b(true)
      val c: ConstVec3b = m

      val tam: AnyVec[Boolean] = m
      val tac: AnyVec[Boolean] = c
      val tn: Vec[Boolean] = m
      val tc: ConstVec[Boolean] = c
    }
    {
      val m = Vec4b(true)
      val c: ConstVec4b = m

      val tam: AnyVec[Boolean] = m
      val tac: AnyVec[Boolean] = c
      val tn: Vec[Boolean] = m
      val tc: ConstVec[Boolean] = c
    }
  }
}
