/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
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

package test.math.floatm

import org.scalatest._

import simplex3d.math.floatm.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Quat4fTest extends FunSuite {

    test("Mutable factories") {
        var q = Quat4()
        expect(1) { q.a }
        expect(0) { q.b }
        expect(0) { q.c }
        expect(0) { q.d }

        q = Quat4(1, 2, 3, 4)
        expect(1) { q.a }
        expect(2) { q.b }
        expect(3) { q.c }
        expect(4) { q.d }

        q = Quat4(Quat4(1, 2, 3, 4))
        expect(1) { q.a }
        expect(2) { q.b }
        expect(3) { q.c }
        expect(4) { q.d }

        q = Quat4(Vec4(1, 2, 3, 4))
        expect(4) { q.a }
        expect(1) { q.b }
        expect(2) { q.c }
        expect(3) { q.d }

        q = Quat4(Mat2(1, 2, 3, 4))
        expect(1) { q.a }
        expect(2) { q.b }
        expect(3) { q.c }
        expect(4) { q.d }
    }

    test("Const conversions") {
        val a = 1f
        val b = 2f
        val c = 3f
        val d = 4f

        var q = ConstQuat4(a, b, c, d)
        expect(a) { q.a }
        expect(b) { q.b }
        expect(c) { q.c }
        expect(d) { q.d }

        q = ConstQuat4(Quat4(a, b, c, d))
        expect(a) { q.a }
        expect(b) { q.b }
        expect(c) { q.c }
        expect(d) { q.d }

        q = ConstQuat4(Vec4(b, c, d, a))
        expect(a) { q.a }
        expect(b) { q.b }
        expect(c) { q.c }
        expect(d) { q.d }

        q = ConstQuat4(Mat2(a, b, c, d))
        expect(a) { q.a }
        expect(b) { q.b }
        expect(c) { q.c }
        expect(d) { q.d }

        val t: ConstQuat4 = Quat4(a, b, c, d)
        assert(Quat4(a, b, c, d) == t)

        var con: ConstQuat4 = Quat4(a, b, c, d); var mut = Quat4()
        mut = con; assert(Quat4(a, b, c, d) == mut)

        con = Quat4(); mut = Quat4(a, b, c, d)
        con = mut; assert(Quat4(a, b, c, d) == con)
    }

    test("Equality methods") {
        val m = Quat4(4, 7, 9, 1)
        val c = ConstQuat4(4, 7, 9, 1)

        assert(m == m)
        assert(m == c)
        assert(c == m)
        assert(c == c)

        assert(m.equals(c))
        assert(!m.equals(Nil))

        assert(Quat4(1, 2, 3, 4) != Quat4(9, 2, 3, 4))
        assert(Quat4(1, 2, 3, 4) != Quat4(1, 9, 3, 4))
        assert(Quat4(1, 2, 3, 4) != Quat4(1, 2, 9, 4))
        assert(Quat4(1, 2, 3, 4) != Quat4(1, 2, 3, 9))
    }

    test("Indexed read") {
        val u = ConstQuat4(3, 4, 5, 6)

        expect(3) { u(0) }
        expect(4) { u(1) }
        expect(5) { u(2) }
        expect(6) { u(3) }

        intercept[IndexOutOfBoundsException] {
            u(4)
        }
        intercept[IndexOutOfBoundsException] {
            u(-1)
        }
    }

    test("Indexed write") {
        val u = Quat4(3, 4, 5, 6)

        u(0) = 5
        assert(Quat4(5, 4, 5, 6) == u)

        u(1) = 6
        assert(Quat4(5, 6, 5, 6) == u)

        u(2) = 7
        assert(Quat4(5, 6, 7, 6) == u)

        u(3) = 8
        assert(Quat4(5, 6, 7, 8) == u)

        intercept[IndexOutOfBoundsException] {
            u(4) = 1
        }
        intercept[IndexOutOfBoundsException] {
            u(-1) = 1
        }
    }

    test("Setters") {
        val u = Quat4()

        u := Quat4(1, 2, 3, 4)
        expect(1) { u.a }
        expect(2) { u.b }
        expect(3) { u.c }
        expect(4) { u.d }

        u.set(5, 6, 7, 8)
        expect(5) { u.a }
        expect(6) { u.b }
        expect(7) { u.c }
        expect(8) { u.d }
    }

    test("Const math") {
        val q = ConstQuat4(6, 7, 8, 9)

        assert(Quat4(-6, -7, -8, -9) == -q)

        assert(Quat4(12, 14, 16, 18) == q*2)
        assert(Quat4(12, 14, 16, 18) == 2*q)
        assert(Quat4(12, 14, 16, 18) == 2f*q)
        assert(Quat4(3, 3.5f, 4, 4.5f) == q/2)
        assert(Quat4(7/6f, 1, 7/8f, 7/9f) == 7/q)
        assert(Quat4(7/6f, 1, 7/8f, 7/9f) == 7f/q)

        val p = ConstQuat4(2, 3, 4, 5)

        assert(Quat4(8, 10, 12, 14) == q + p)
        assert(Quat4(4, 4, 4, 4) == q - p)

        assert(Quat4(-86, 36, 32, 52) == q*p)
    }

    test("Mutable math") {
        val q = Quat4()
        val i = ConstQuat4(2, 3, 4, 5)

        q := i; q *= 2; assert(Quat4(4, 6, 8, 10) == q)
        q := i; q /= 2; assert(Quat4(1, 1.5f, 2, 2.5f) == q)

        q := i; q += Quat4(3, 4, 5, 6); assert(Quat4(5, 7, 9, 11) == q)
        q := i; q += q; assert(Quat4(4, 6, 8, 10) == q)
        q := i; q -= Quat4(2, 3, 4, 5); assert(Quat4(0, 0, 0, 0) == q)
        q := i; q -= q; assert(Quat4(0, 0, 0, 0) == q)

        q := i; q *= Quat4(6, 7, 8, 9); assert(Quat4(-86, 28, 48, 44) == q)
        q := i; q *= q; assert(Quat4(-46, 12, 16, 20) == q)
    }
}
