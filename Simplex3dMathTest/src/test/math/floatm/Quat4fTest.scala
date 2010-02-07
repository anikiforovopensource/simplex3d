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

import simplex3d.math.BaseMath._
import simplex3d.math.floatm.renamed._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Quat4fTest extends FunSuite {

    test("Factories") {
        val af = 1f + 1e-5f
        val bf = 2f + 1e-5f
        val cf = 3f + 1e-5f
        val df = 4f + 1e-5f

        val ad = 1 + 1e-5
        val bd = 2 + 1e-5
        val cd = 3 + 1e-5
        val dd = 4 + 1e-5

        var q: AnyQuat4 = Quat4(af, bf, cf, df)
        expect(classOf[Quat4]) { q.getClass }
        expect(af) { q.a }
        expect(bf) { q.b }
        expect(cf) { q.c }
        expect(df) { q.d }

        q = Quat4(Quat4(af, bf, cf, df))
        expect(classOf[Quat4]) { q.getClass }
        expect(af) { q.a }
        expect(bf) { q.b }
        expect(cf) { q.c }
        expect(df) { q.d }

        q = Quat4(Vec4(bf, cf, df, af))
        expect(classOf[Quat4]) { q.getClass }
        expect(af) { q.a }
        expect(bf) { q.b }
        expect(cf) { q.c }
        expect(df) { q.d }

        q = Quat4(Mat2(bf, cf, df, af))
        expect(classOf[Quat4]) { q.getClass }
        expect(af) { q.a }
        expect(bf) { q.b }
        expect(cf) { q.c }
        expect(df) { q.d }

        q = Quat4(Quat4d(ad, bd, cd, dd))
        expect(classOf[Quat4]) { q.getClass }
        expect(float(ad)) { q.a }
        expect(float(bd)) { q.b }
        expect(float(cd)) { q.c }
        expect(float(dd)) { q.d }

        q = Quat4(Vec4d(bd, cd, dd, ad))
        expect(classOf[Quat4]) { q.getClass }
        expect(float(ad)) { q.a }
        expect(float(bd)) { q.b }
        expect(float(cd)) { q.c }
        expect(float(dd)) { q.d }

        q = Quat4(Mat2d(bd, cd, dd, ad))
        expect(classOf[Quat4]) { q.getClass }
        expect(float(ad)) { q.a }
        expect(float(bd)) { q.b }
        expect(float(cd)) { q.c }
        expect(float(dd)) { q.d }

        var p: AnyQuat4 = ConstQuat4(af, bf, cf, df)
        expect(classOf[ConstQuat4]) { p.getClass }
        expect(af) { p.a }
        expect(bf) { p.b }
        expect(cf) { p.c }
        expect(df) { p.d }

        p = ConstQuat4(Quat4(af, bf, cf, df))
        expect(classOf[ConstQuat4]) { p.getClass }
        expect(af) { p.a }
        expect(bf) { p.b }
        expect(cf) { p.c }
        expect(df) { p.d }

        p = ConstQuat4(Quat4d(ad, bd, cd, dd))
        expect(classOf[ConstQuat4]) { p.getClass }
        expect(float(ad)) { p.a }
        expect(float(bd)) { p.b }
        expect(float(cd)) { p.c }
        expect(float(dd)) { p.d }
    }

    test("Unapply") {
        val a = 1+1e-5f; val b = 2+1e-5f; val c = 3+1e-5f; val d = 4+1e-5f
        Quat4(a, b, c, d) match {
            case Quat4(qa, qb, qc, qd) =>
                if (qa != a || qb != b || qc != c || qd != d)
                    throw new AssertionError()
        }
        ConstQuat4(a, b, c, d) match {
            case Quat4(qa, qb, qc, qd) =>
                if (qa != a || qb != b || qc != c || qd != d)
                    throw new AssertionError()
        }
    }

    test("Const conversions") {
        val a = 1f + 1e-5f
        val b = 2f + 1e-5f
        val c = 3f + 1e-5f
        val d = 4f + 1e-5f
        
        val t: ConstQuat4 = Quat4(a, b, c, d)
        expect(classOf[ConstQuat4]) { t.getClass }
        assert(Quat4(a, b, c, d) == t)

        var con: ConstQuat4 = Quat4(a, b, c, d); var mut = Quat4(1, 0, 0, 0)
        expect(classOf[ConstQuat4]) { con.getClass }
        mut = con; assert(Quat4(a, b, c, d) == mut)
        expect(classOf[Quat4]) { mut.getClass }

        con = Quat4(1, 0, 0, 0); mut = Quat4(a, b, c, d)
        expect(classOf[Quat4]) { mut.getClass }
        con = mut; assert(Quat4(a, b, c, d) == con)
        expect(classOf[ConstQuat4]) { con.getClass }
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
        val u = Quat4(0, 0, 0, 0)

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
        assert(Quat4(3, 3.5f, 4, 4.5f) == q/2)

        val p = ConstQuat4(2, 3, 4, 5)

        assert(Quat4(8, 10, 12, 14) == q + p)
        assert(Quat4(4, 4, 4, 4) == q - p)

        assert(Quat4(-86, 36, 32, 52) == q*p)
    }

    test("Mutable math") {
        val q = Quat4(1, 0, 0, 0)
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
