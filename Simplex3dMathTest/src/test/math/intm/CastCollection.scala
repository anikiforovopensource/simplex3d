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

package test.math.intm

import org.scalatest._

import simplex3d.math._
import simplex3d.math.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class CastCollection extends FunSuite {

    // These tests pass if they compile.
    test("Cast Collection") {
        {
            val m = Vec2i(1)
            val c: ConstVec2i = m

            val tam: AnyVec[Int] = m
            val tac: AnyVec[Int] = c
            val tn: Vec[Int] = m
            val tc: ConstVec[Int] = c
        }
        {
            val m = Vec3i(1)
            val c: ConstVec3i = m

            val tam: AnyVec[Int] = m
            val tac: AnyVec[Int] = c
            val tn: Vec[Int] = m
            val tc: ConstVec[Int] = c
        }
        {
            val m = Vec4i(1)
            val c: ConstVec4i = m

            val tam: AnyVec[Int] = m
            val tac: AnyVec[Int] = c
            val tn: Vec[Int] = m
            val tc: ConstVec[Int] = c
        }
    }
}
