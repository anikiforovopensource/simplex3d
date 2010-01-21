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

import simplex3d.math._
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class FloatMathTest extends FunSuite {

    test("Float functions") {
        assert(isnan(Float.NaN))
        assert(!isnan(Float.PositiveInfinity))
        assert(!isnan(Float.NegativeInfinity))
        assert(!isnan(0))

        assert(!isinf(Float.NaN))
        assert(isinf(Float.PositiveInfinity))
        assert(isinf(Float.NegativeInfinity))
        assert(!isinf(0))

        assert(!isposinf(Float.NaN))
        assert(isposinf(Float.PositiveInfinity))
        assert(!isposinf(Float.NegativeInfinity))
        assert(!isposinf(0))

        assert(!isneginf(Float.NaN))
        assert(!isneginf(Float.PositiveInfinity))
        assert(isneginf(Float.NegativeInfinity))
        assert(!isneginf(0))

        assert(isnan(radians(Float.NaN)))
        assert(isposinf(radians(Float.PositiveInfinity)))
        assert(isneginf(radians(Float.NegativeInfinity)))
        expect(0) { radians(0) }
        expect(Pi) { radians(180) }
        expect(-2*Pi) { radians(-360) }

        assert(isnan(degrees(Float.NaN)))
        assert(isposinf(degrees(Float.PositiveInfinity)))
        assert(isneginf(degrees(Float.NegativeInfinity)))
        expect(0) { degrees(0) }
        expect(180) { degrees(Pi) }
        expect(-360) { degrees(-2*Pi) }

        assert(isnan(sin(Float.NaN)))
        assert(isnan(sin(Float.PositiveInfinity)))
        assert(isnan(sin(Float.NegativeInfinity)))
        assert(approxEqual(0, sin(0), 1e-6f))
        assert(approxEqual(0.5f, sin(Pi/6), 1e-6f))
        assert(approxEqual(inversesqrt(2), sin(Pi/4), 1e-6f))
        assert(approxEqual(sqrt(3)/2, sin(Pi/3), 1e-6f))
        assert(approxEqual(1, sin(Pi/2), 1e-6f))
        assert(approxEqual(0, sin(Pi), 1e-6f))
        assert(approxEqual(-1, sin(3*Pi/2), 1e-6f))
        assert(approxEqual(0, sin(2*Pi), 1e-6f))

        assert(isnan(cos(Float.NaN)))
        assert(isnan(cos(Float.PositiveInfinity)))
        assert(isnan(cos(Float.NegativeInfinity)))
        assert(approxEqual(1, cos(0), 1e-6f))
        assert(approxEqual(sqrt(3)/2, cos(Pi/6), 1e-6f))
        assert(approxEqual(inversesqrt(2), cos(Pi/4), 1e-6f))
        assert(approxEqual(0.5f, cos(Pi/3), 1e-6f))
        assert(approxEqual(0, cos(Pi/2), 1e-6f))
        assert(approxEqual(-1, cos(Pi), 1e-6f))
        assert(approxEqual(0, cos(3*Pi/2), 1e-6f))
        assert(approxEqual(1, cos(2*Pi), 1e-6f))

        assert(isnan(tan(Float.NaN)))
        assert(isnan(tan(Float.PositiveInfinity)))
        assert(isnan(tan(Float.NegativeInfinity)))
        assert(approxEqual(0, tan(0), 1e-6f))
        assert(approxEqual(inversesqrt(3), tan(Pi/6), 1e-6f))
        assert(approxEqual(1, tan(Pi/4), 1e-6f))
        assert(approxEqual(sqrt(3), tan(Pi/3), 1e-6f))

        assert(isnan(asin(Float.NaN)))
        assert(isnan(asin(Float.PositiveInfinity)))
        assert(isnan(asin(Float.NegativeInfinity)))
        assert(approxEqual(0, asin(0), 1e-6f))
        assert(approxEqual(Pi/6, asin(0.5f), 1e-6f))
        assert(approxEqual(Pi/4, asin(inversesqrt(2)), 1e-6f))
        assert(approxEqual(Pi/3, asin(sqrt(3)/2), 1e-6f))
        assert(approxEqual(Pi/2, asin(1), 1e-6f))

        assert(isnan(acos(Float.NaN)))
        assert(isnan(acos(Float.PositiveInfinity)))
        assert(isnan(acos(Float.NegativeInfinity)))
        assert(approxEqual(0, acos(1), 1e-6f))
        assert(approxEqual(Pi/6, acos(sqrt(3)/2), 1e-6f))
        assert(approxEqual(Pi/4, acos(inversesqrt(2)), 1e-6f))
        assert(approxEqual(Pi/3, acos(0.5f), 1e-6f))
        assert(approxEqual(Pi/2, acos(0), 1e-6f))

        assert(isnan(atan(Float.NaN)))
        assert(approxEqual(Pi/2, atan(Float.PositiveInfinity), 1e-6f))
        assert(approxEqual(-Pi/2, atan(Float.NegativeInfinity), 1e-6f))
        assert(approxEqual(0, atan(0), 1e-6f))
        assert(approxEqual(Pi/6, atan(inversesqrt(3)), 1e-6f))
        assert(approxEqual(Pi/4, atan(1), 1e-6f))
        assert(approxEqual(Pi/3, atan(sqrt(3)), 1e-6f))

        assert(isnan(atan(Float.NaN, 1)))
        assert(approxEqual(Pi/2, atan(Float.PositiveInfinity, 1), 1e-6f))
        assert(approxEqual(-Pi/2, atan(Float.NegativeInfinity, 1), 1e-6f))
        assert(isnan(atan(1, Float.NaN)))
        assert(approxEqual(0, atan(1, Float.PositiveInfinity), 1e-6f))
        assert(approxEqual(Pi, atan(1, Float.NegativeInfinity), 1e-6f))
        assert(isnan(atan(Float.NaN, Float.NaN)))
        assert(isnan(atan(Float.NaN, Float.PositiveInfinity)))
        assert(isnan(atan(Float.NaN, Float.NegativeInfinity)))
        assert(isnan(atan(Float.PositiveInfinity, Float.NaN)))
        assert(isnan(atan(Float.NegativeInfinity, Float.NaN)))
        assert(approxEqual(Pi/4, atan(Float.PositiveInfinity, Float.PositiveInfinity), 1e-6f))
        assert(approxEqual(3*Pi/4, atan(Float.PositiveInfinity, Float.NegativeInfinity), 1e-6f))
        assert(approxEqual(-Pi/4, atan(Float.NegativeInfinity, Float.PositiveInfinity), 1e-6f))
        assert(approxEqual(-3*Pi/4, atan(Float.NegativeInfinity, Float.NegativeInfinity), 1e-6f))
        assert(approxEqual(0, atan(0, 1), 1e-6f))
        assert(approxEqual(Pi/6, atan(tan(Pi/6), 1), 1e-6f))
        assert(approxEqual(Pi/4, atan(1, 1), 1e-6f))
        assert(approxEqual(Pi/3, atan(tan(Pi/3), 1), 1e-6f))
        assert(approxEqual(Pi/2, atan(1, 0), 1e-6f))

        assert(isnan(sinh(Float.NaN)))
        assert(isposinf(sinh(Float.PositiveInfinity)))
        assert(isneginf(sinh(Float.NegativeInfinity)))
        assert(approxEqual(-3.62686040784701876767f, sinh(-2), 1e-6f))
        assert(approxEqual(-1.17520119364380145688f, sinh(-1), 1e-6f))
        assert(approxEqual(0, sinh(0), 1e-6f))
        assert(approxEqual(1.17520119364380145688f, sinh(1), 1e-6f))
        assert(approxEqual(3.62686040784701876767f, sinh(2), 1e-6f))

        assert(isnan(cosh(Float.NaN)))
        assert(isposinf(cosh(Float.PositiveInfinity)))
        assert(isposinf(cosh(Float.NegativeInfinity)))
        assert(approxEqual(3.76219569108363145956f, cosh(-2), 1e-6f))
        assert(approxEqual(1.54308063481524377848f, cosh(-1), 1e-6f))
        assert(approxEqual(1, cosh(0), 1e-6f))
        assert(approxEqual(1.54308063481524377848f, cosh(1), 1e-6f))
        assert(approxEqual(3.76219569108363145956f, cosh(2), 1e-6f))

        assert(isnan(tanh(Float.NaN)))
        assert(1 == (tanh(Float.PositiveInfinity)))
        assert(-1 == (tanh(Float.NegativeInfinity)))
        assert(approxEqual(-0.96402758007581688395f, tanh(-2), 1e-6f))
        assert(approxEqual(-0.76159415595576488812f, tanh(-1), 1e-6f))
        assert(approxEqual(0, tanh(0), 1e-6f))
        assert(approxEqual(0.76159415595576488812f, tanh(1), 1e-6f))
        assert(approxEqual(0.96402758007581688395f, tanh(2), 1e-6f))

        assert(isnan(asinh(Float.NaN)))
        assert(isposinf(asinh(Float.PositiveInfinity)))
        assert(isneginf(asinh(Float.NegativeInfinity)))
        assert(approxEqual(-2, asinh(-3.62686040784701876767f), 1e-6f))
        assert(approxEqual(-1, asinh(-1.17520119364380145688f), 1e-6f))
        assert(approxEqual(0, asinh(0), 1e-6f))
        assert(approxEqual(1, asinh(1.17520119364380145688f), 1e-6f))
        assert(approxEqual(2, asinh(3.62686040784701876767f), 1e-6f))

        assert(isnan(acosh(Float.NaN)))
        assert(isposinf(acosh(Float.PositiveInfinity)))
        assert(isnan(acosh(Float.NegativeInfinity)))
        assert(approxEqual(0, acosh(1), 1e-6f))
        assert(approxEqual(1, acosh(1.54308063481524377848f), 1e-6f))
        assert(approxEqual(2, acosh(3.76219569108363145956f), 1e-6f))

        assert(isnan(atanh(Float.NaN)))
        assert(isnan(atanh(Float.PositiveInfinity)))
        assert(isnan(atanh(Float.NegativeInfinity)))
        assert(approxEqual(-2, atanh(-0.96402758007581688395f), 1e-6f))
        assert(approxEqual(-1, atanh(-0.76159415595576488812f), 1e-6f))
        assert(approxEqual(0, atanh(0), 1e-6f))
        assert(approxEqual(1, atanh(0.76159415595576488812f), 1e-6f))
        assert(approxEqual(2, atanh(0.96402758007581688395f), 1e-6f))

        assert(isnan(pow(Float.NaN, 2)))
        assert(isposinf(pow(Float.PositiveInfinity, 2)))
        assert(isposinf(pow(Float.NegativeInfinity, 2)))
        assert(isneginf(pow(Float.NegativeInfinity, 3)))
        assert(isnan(pow(2, Float.NaN)))
        assert(isposinf(pow(2, Float.PositiveInfinity)))
        assert(0 == (pow(2, Float.NegativeInfinity)))
        assert(isnan(pow(Float.NaN, Float.NaN)))
        assert(isnan(pow(Float.NaN, Float.PositiveInfinity)))
        assert(isnan(pow(Float.NaN, Float.NegativeInfinity)))
        assert(isnan(pow(Float.PositiveInfinity, Float.NaN)))
        assert(isnan(pow(Float.NegativeInfinity, Float.NaN)))
        assert(isposinf(pow(Float.PositiveInfinity, Float.PositiveInfinity)))
        assert(0 == (pow(Float.PositiveInfinity, Float.NegativeInfinity)))
        assert(isposinf(pow(Float.NegativeInfinity, Float.PositiveInfinity)))
        assert(0 == (pow(Float.NegativeInfinity, Float.NegativeInfinity)))
        assert(approxEqual(2, pow(4, 0.5f), 1e-6f))
        assert(approxEqual(4, pow(2, 2), 1e-6f))

        assert(isnan(exp(Float.NaN)))
        assert(isposinf(exp(Float.PositiveInfinity)))
        assert(0 == (exp(Float.NegativeInfinity)))
        assert(approxEqual(E, exp(1), 1e-6f))
        assert(approxEqual(E*E, exp(2), 1e-6f))

        assert(isnan(log(Float.NaN)))
        assert(isposinf(log(Float.PositiveInfinity)))
        assert(isnan(log(Float.NegativeInfinity)))
        assert(approxEqual(0, log(1), 1e-6f))
        assert(approxEqual(2, log(E*E), 1e-6f))

        assert(isnan(exp2(Float.NaN)))
        assert(isposinf(exp2(Float.PositiveInfinity)))
        assert(0 == (exp2(Float.NegativeInfinity)))
        assert(approxEqual(2, exp2(1), 1e-6f))
        assert(approxEqual(4, exp2(2), 1e-6f))

        assert(isnan(log2(Float.NaN)))
        assert(isposinf(log2(Float.PositiveInfinity)))
        assert(isnan(log2(Float.NegativeInfinity)))
        assert(approxEqual(0, log2(1), 1e-6f))
        assert(approxEqual(2, log2(4), 1e-6f))

        assert(isnan(sqrt(Float.NaN)))
        assert(isposinf(sqrt(Float.PositiveInfinity)))
        assert(isnan(sqrt(Float.NegativeInfinity)))
        assert(approxEqual(1.4142135623730950488f, sqrt(2), 1e-6f))
        assert(approxEqual(2, sqrt(4), 1e-6f))

        assert(isnan(inversesqrt(Float.NaN)))
        assert(0 == (inversesqrt(Float.PositiveInfinity)))
        assert(isnan(inversesqrt(Float.NegativeInfinity)))
        assert(approxEqual(1/1.4142135623730950488f, inversesqrt(2), 1e-6f))
        assert(approxEqual(0.5f, inversesqrt(4), 1e-6f))

        assert(isnan(abs(Float.NaN)))
        assert(isposinf(abs(Float.PositiveInfinity)))
        assert(isposinf(abs(Float.NegativeInfinity)))
        assert(1 == abs(-1))
        assert(0 == abs(0))
        assert(1 == abs(1))

        assert(isnan(sign(Float.NaN)))
        assert(1 == (sign(Float.PositiveInfinity)))
        assert(-1 == (sign(Float.NegativeInfinity)))
        assert(-1 == sign(-1))
        assert(0 == sign(0))
        assert(1 == sign(1))

        assert(isnan(floor(Float.NaN)))
        assert(isposinf(floor(Float.PositiveInfinity)))
        assert(isneginf(floor(Float.NegativeInfinity)))
        assert(-1 == floor(-1))
        assert(-1 == floor(-0.5f))
        assert(0 == floor(0))
        assert(1 == floor(1.5f))
        assert(1 == floor(1))

        assert(isnan(trunc(Float.NaN)))
        assert(isposinf(trunc(Float.PositiveInfinity)))
        assert(isneginf(trunc(Float.NegativeInfinity)))
        assert(-1 == trunc(-1))
        assert(-1 == trunc(-1.5f))
        assert(0 == trunc(0))
        assert(1 == trunc(1.5f))
        assert(1 == trunc(1))

        assert(isnan(round(Float.NaN)))
        assert(isposinf(round(Float.PositiveInfinity)))
        assert(isneginf(round(Float.NegativeInfinity)))
        assert(-1 == round(-1))
        assert(-1 == round(-1.4f))
        assert(-2 == round(-1.6f))
        assert(0 == round(0))
        assert(1 == round(1.4f))
        assert(1 == round(1))
        assert(2 == round(1.6f))

        assert(isnan(roundEven(Float.NaN)))
        assert(isposinf(roundEven(Float.PositiveInfinity)))
        assert(isneginf(roundEven(Float.NegativeInfinity)))
        assert(-1 == roundEven(-1))
        assert(-1 == roundEven(-1.4f))
        assert(-2 == roundEven(-1.5f))
        assert(-2 == roundEven(-1.6f))
        assert(-2 == roundEven(-2.5f))
        assert(0 == roundEven(0))
        assert(1 == roundEven(1))
        assert(1 == roundEven(1.4f))
        assert(2 == roundEven(1.5f))
        assert(2 == roundEven(1.6f))
        assert(2 == roundEven(2.5f))

        assert(isnan(ceil(Float.NaN)))
        assert(isposinf(ceil(Float.PositiveInfinity)))
        assert(isneginf(ceil(Float.NegativeInfinity)))
        assert(-1 == ceil(-1))
        assert(-1 == ceil(-1.1f))
        assert(0 == ceil(0))
        assert(1 == ceil(1))
        assert(1 == ceil(0.1f))

        assert(isnan(fract(Float.NaN)))
        assert(0 == (fract(Float.PositiveInfinity)))
        assert(0 == (fract(Float.NegativeInfinity)))
        assert(0.9f == fract(-1.1f))
        assert(0 == fract(-1))
        assert(0 == fract(0))
        assert(0 == fract(1))
        assert(0.25f == fract(1.25f))

        assert(isnan(mod(2, Float.NaN)))
        assert(isnan(mod(2, Float.PositiveInfinity)))
        assert(isnan(mod(2, Float.NegativeInfinity)))
        assert(isnan(mod(Float.NaN, 2)))
        assert(isnan(mod(Float.PositiveInfinity, 2)))
        assert(isnan(mod(Float.NegativeInfinity, 2)))
        assert(0.25f == mod(10.25f, 2.5f))
        assert(-0.25f == mod(-10.25f, -2.5f))
        assert(2.25f == mod(-10.25f, 2.5f))
        assert(-2.25f == mod(10.25f, -2.5f))
        assert(0 == mod(0, 2.5f))

        assert(isnan(min(Float.NaN, 2)))
        assert(2 == (min(Float.PositiveInfinity, 2)))
        assert(isneginf(min(Float.NegativeInfinity, 2)))
        assert(isnan(min(2, Float.NaN)))
        assert(2 == (min(2, Float.PositiveInfinity)))
        assert(isneginf(min(2, Float.NegativeInfinity)))
        assert(1 == min(1, 2))
        assert(1 == min(2, 1))
        assert(1 == min(1, 1))

        assert(isnan(max(Float.NaN, 2)))
        assert(isposinf(max(Float.PositiveInfinity, 2)))
        assert(2 == (max(Float.NegativeInfinity, 2)))
        assert(isnan(max(2, Float.NaN)))
        assert(isposinf(max(2, Float.PositiveInfinity)))
        assert(2 == (max(2, Float.NegativeInfinity)))
        assert(2 == max(1, 2))
        assert(2 == max(2, 1))
        assert(2 == max(2, 2))

        assert(isnan(clamp(Float.NaN, 1, 3)))
        assert(3 == (clamp(Float.PositiveInfinity, 1, 3)))
        assert(1 == (clamp(Float.NegativeInfinity, 1, 3)))
        assert(1 == clamp(0, 1, 3))
        assert(1 == clamp(1, 1, 3))
        assert(2 == clamp(2, 1, 3))
        assert(3 == clamp(3, 1, 3))
        assert(3 == clamp(4, 1, 3))

        assert(isnan(mix(Float.NaN, 4, 0.25f)))
        assert(isposinf(mix(Float.PositiveInfinity, 4, 0.25f)))
        assert(isneginf(mix(Float.NegativeInfinity, 4, 0.25f)))
        assert(isnan(mix(0, Float.NaN, 0.25f)))
        assert(isposinf(mix(0, Float.PositiveInfinity, 0.25f)))
        assert(isneginf(mix(0, Float.NegativeInfinity, 0.25f)))
        assert(isnan(mix(0, 4, Float.NaN)))
        assert(isnan(mix(0, 4, Float.PositiveInfinity)))
        assert(isnan(mix(0, 4, Float.NegativeInfinity)))
        assert(0 == mix(0, 4, 0))
        assert(1 == mix(0, 4, 0.25f))
        assert(2 == mix(0, 4, 0.5f))
        assert(3 == mix(0, 4, 0.75f))
        assert(4 == mix(0, 4, 1))
        
        assert(isnan(step(2, Float.NaN)))
        assert(1 == (step(2, Float.PositiveInfinity)))
        assert(0 == (step(2, Float.NegativeInfinity)))
        assert(0 == step(2, 1))
        assert(1 == step(2, 2))
        assert(1 == step(2, 3))

        assert(isnan(smoothstep(1, 2, Float.NaN)))
        assert(1 == (smoothstep(1, 2, Float.PositiveInfinity)))
        assert(0 == (smoothstep(1, 2, Float.NegativeInfinity)))
        assert(0 == smoothstep(1, 2, 1))
        assert(approxEqual(0.104f, smoothstep(1, 2, 1.2f), 1e-6f))
        assert(0.5f == smoothstep(1, 2, 1.5f))
        assert(approxEqual(0.784f, smoothstep(1, 2, 1.7f), 1e-6f))
        assert(1 == smoothstep(1, 2, 2))
        assert(1 == smoothstep(1, 2, 3))

        assert(0 == length(0))
        assert(1 == length(1))
        assert(1 == length(-1))
        assert(2.5f == length(2.5f))
        assert(2.5f == length(-2.5f))

        assert(2 == distance(-1, 1))
        assert(2.5f == distance(2, 4.5f))
        assert(2 == distance(0, -2))

        assert(2 == dot(-1, -2))
        assert(0 == dot(0, 2))
        assert(-5.2f == dot(-2, 2.6f))
        assert(12 == dot(4, 3))

        assert(-1 == normalize(-2.5f))
        assert(-1 == normalize(-1))
        assert(-1 == normalize(-0.3f))
        assert(0 == normalize(0))
        assert(1 == normalize(0.5f))
        assert(1 == normalize(1))
        assert(1 == normalize(3.5f))

        assert(-2.2f == faceforward(2.2f, 3, 2))
        assert(-2.2f == faceforward(2.2f, 0, 2))
        assert(2.2f == faceforward(2.2f, 3, -2))
        
        assert(-1 == reflect(1, 1))
        assert(-2.5f == reflect(2.5f, 1))
        assert(1 == reflect(-1, 1))
        assert(2.5f == reflect(-2.5f, 1))
        assert(-1 == reflect(1, -1))
        assert(-2.5f == reflect(2.5f, -1))
        assert(1 == reflect(-1, -1))
        assert(2.5f == reflect(-2.5f, -1))

        assert(approxEqual(-1, refract(1, 1, 0.3f), 1e-6f))
        assert(approxEqual(1, refract(1, -1, 0.3f), 1e-6f))
        assert(approxEqual(-1, refract(-1, 1, 0.3f), 1e-6f))
        assert(approxEqual(1, refract(-1, -1, 0.3f), 1e-6f))
    }

    test("Numeric vec functions") {
//    def modf(u: AnyVec2f, i: Vec2f) :Vec2f = {
//        val s = sign(u)
//        val a = abs(u)
//        i := s*floor(a)
//        s*fract(a)
//    }
//    def length(u: AnyVec2f) :Float = sqrt(u.x*u.x + u.y*u.y)
//    def distance(u: AnyVec2f, v: AnyVec2f) :Float = length(u - v)
//    def dot(u: AnyVec2f, v: AnyVec2f) :Float = u.x * v.x + u.y * v.y
//    def normalize(u: AnyVec2f) :Vec2f = u*inversesqrt(u.x*u.x + u.y*u.y)
//
//    def faceforward(n: AnyVec2f, i: AnyVec2f, nref: AnyVec2f) :Vec2f = {
//        if (dot(nref, i) < 0) Vec2f(n) else -n
//    }
//
//    def reflect(i: AnyVec2f, n: AnyVec2f) :Vec2f = {
//        i - n*2*dot(n, i)
//    }
//    def refract(i: AnyVec2f, n: AnyVec2f, eta: Float) :Vec2f = {
//        val dotni = dot(n, i)
//        val k = 1 - eta*eta*(1 - dotni*dotni)
//        if (k < 0) Vec2f(0) else i*eta - n*(eta*dotni + sqrt(k))
//    }
    }
}
