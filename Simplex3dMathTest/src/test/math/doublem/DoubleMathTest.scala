/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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

package test.math.doublem

import org.scalatest._

import simplex3d.math._
import simplex3d.math.doublem.renamed._
import simplex3d.math.doublem.DoubleMath._
import Double.{
  NaN => nan,
  PositiveInfinity => posinf,
  NegativeInfinity => neginf
}


/**
 * @author Aleksey Nikiforov (lex)
 */
class DoubleMathTest extends FunSuite {

  test("Double functions") {
    assert(isnan(nan))
    assert(!isnan(posinf))
    assert(!isnan(neginf))
    assert(!isnan(0))

    assert(!isinf(nan))
    assert(isinf(posinf))
    assert(isinf(neginf))
    assert(!isinf(0))

    assert(!isposinf(nan))
    assert(isposinf(posinf))
    assert(!isposinf(neginf))
    assert(!isposinf(0))

    assert(!isneginf(nan))
    assert(!isneginf(posinf))
    assert(isneginf(neginf))
    assert(!isneginf(0))

    assert(isnan(radians(nan)))
    assert(isposinf(radians(posinf)))
    assert(isneginf(radians(neginf)))
    expect(0) { radians(0) }
    expect(Pi) { radians(180) }
    expect(-2*Pi) { radians(-360) }

    assert(isnan(degrees(nan)))
    assert(isposinf(degrees(posinf)))
    assert(isneginf(degrees(neginf)))
    expect(0) { degrees(0) }
    expect(180) { degrees(Pi) }
    expect(-360) { degrees(-2*Pi) }

    assert(isnan(sin(nan)))
    assert(isnan(sin(posinf)))
    assert(isnan(sin(neginf)))
    assert(approxEqual(0, sin(0), 1e-15))
    assert(approxEqual(0.5, sin(Pi/6), 1e-15))
    assert(approxEqual(inversesqrt(2), sin(Pi/4), 1e-15))
    assert(approxEqual(sqrt(3)/2, sin(Pi/3), 1e-15))
    assert(approxEqual(1, sin(Pi/2), 1e-15))
    assert(approxEqual(0, sin(Pi), 1e-15))
    assert(approxEqual(-1, sin(3*Pi/2), 1e-15))
    assert(approxEqual(0, sin(2*Pi), 1e-15))

    assert(isnan(cos(nan)))
    assert(isnan(cos(posinf)))
    assert(isnan(cos(neginf)))
    assert(approxEqual(1, cos(0), 1e-15))
    assert(approxEqual(sqrt(3)/2, cos(Pi/6), 1e-15))
    assert(approxEqual(inversesqrt(2), cos(Pi/4), 1e-15))
    assert(approxEqual(0.5, cos(Pi/3), 1e-15))
    assert(approxEqual(0, cos(Pi/2), 1e-15))
    assert(approxEqual(-1, cos(Pi), 1e-15))
    assert(approxEqual(0, cos(3*Pi/2), 1e-15))
    assert(approxEqual(1, cos(2*Pi), 1e-15))

    assert(isnan(tan(nan)))
    assert(isnan(tan(posinf)))
    assert(isnan(tan(neginf)))
    assert(approxEqual(0, tan(0), 1e-15))
    assert(approxEqual(inversesqrt(3), tan(Pi/6), 1e-15))
    assert(approxEqual(1, tan(Pi/4), 1e-15))
    assert(approxEqual(sqrt(3), tan(Pi/3), 1e-15))

    assert(isnan(asin(nan)))
    assert(isnan(asin(posinf)))
    assert(isnan(asin(neginf)))
    assert(approxEqual(0, asin(0), 1e-15))
    assert(approxEqual(Pi/6, asin(0.5), 1e-15))
    assert(approxEqual(Pi/4, asin(inversesqrt(2)), 1e-15))
    assert(approxEqual(Pi/3, asin(sqrt(3)/2), 1e-15))
    assert(approxEqual(Pi/2, asin(1), 1e-15))

    assert(isnan(acos(nan)))
    assert(isnan(acos(posinf)))
    assert(isnan(acos(neginf)))
    assert(approxEqual(0, acos(1), 1e-15))
    assert(approxEqual(Pi/6, acos(sqrt(3)/2), 1e-15))
    assert(approxEqual(Pi/4, acos(inversesqrt(2)), 1e-15))
    assert(approxEqual(Pi/3, acos(0.5), 1e-15))
    assert(approxEqual(Pi/2, acos(0), 1e-15))

    assert(isnan(atan(nan)))
    assert(approxEqual(Pi/2, atan(posinf), 1e-15))
    assert(approxEqual(-Pi/2, atan(neginf), 1e-15))
    assert(approxEqual(0, atan(0), 1e-15))
    assert(approxEqual(Pi/6, atan(inversesqrt(3)), 1e-15))
    assert(approxEqual(Pi/4, atan(1), 1e-15))
    assert(approxEqual(Pi/3, atan(sqrt(3)), 1e-15))

    assert(isnan(atan(nan, 1)))
    assert(approxEqual(Pi/2, atan(posinf, 1), 1e-15))
    assert(approxEqual(-Pi/2, atan(neginf, 1), 1e-15))
    assert(isnan(atan(1, nan)))
    assert(approxEqual(0, atan(1, posinf), 1e-15))
    assert(approxEqual(Pi, atan(1, neginf), 1e-15))
    assert(isnan(atan(nan, nan)))
    assert(isnan(atan(nan, posinf)))
    assert(isnan(atan(nan, neginf)))
    assert(isnan(atan(posinf, nan)))
    assert(isnan(atan(neginf, nan)))
    assert(approxEqual(Pi/4, atan(posinf, posinf), 1e-15))
    assert(approxEqual(3*Pi/4, atan(posinf, neginf), 1e-15))
    assert(approxEqual(-Pi/4, atan(neginf, posinf), 1e-15))
    assert(approxEqual(-3*Pi/4, atan(neginf, neginf), 1e-15))
    assert(approxEqual(0, atan(0, 1), 1e-15))
    assert(approxEqual(Pi/6, atan(tan(Pi/6), 1), 1e-15))
    assert(approxEqual(Pi/4, atan(1, 1), 1e-15))
    assert(approxEqual(Pi/3, atan(tan(Pi/3), 1), 1e-15))
    assert(approxEqual(Pi/2, atan(1, 0), 1e-15))

    assert(isnan(sinh(nan)))
    assert(isposinf(sinh(posinf)))
    assert(isneginf(sinh(neginf)))
    assert(approxEqual(-3.62686040784701876767, sinh(-2), 1e-15))
    assert(approxEqual(-1.17520119364380145688, sinh(-1), 1e-15))
    assert(approxEqual(0, sinh(0), 1e-15))
    assert(approxEqual(1.17520119364380145688, sinh(1), 1e-15))
    assert(approxEqual(3.62686040784701876767, sinh(2), 1e-15))

    assert(isnan(cosh(nan)))
    assert(isposinf(cosh(posinf)))
    assert(isposinf(cosh(neginf)))
    assert(approxEqual(3.76219569108363145956, cosh(-2), 1e-15))
    assert(approxEqual(1.54308063481524377848, cosh(-1), 1e-15))
    assert(approxEqual(1, cosh(0), 1e-15))
    assert(approxEqual(1.54308063481524377848, cosh(1), 1e-15))
    assert(approxEqual(3.76219569108363145956, cosh(2), 1e-15))

    assert(isnan(tanh(nan)))
    assert(1 == (tanh(posinf)))
    assert(-1 == (tanh(neginf)))
    assert(approxEqual(-0.96402758007581688395, tanh(-2), 1e-15))
    assert(approxEqual(-0.76159415595576488812, tanh(-1), 1e-15))
    assert(approxEqual(0, tanh(0), 1e-15))
    assert(approxEqual(0.76159415595576488812, tanh(1), 1e-15))
    assert(approxEqual(0.96402758007581688395, tanh(2), 1e-15))

    assert(isnan(asinh(nan)))
    assert(isposinf(asinh(posinf)))
    assert(isneginf(asinh(neginf)))
    assert(approxEqual(-100, asinh(sinh(-100)), 1e-15))
    assert(approxEqual(-2, asinh(-3.62686040784701876767), 1e-15))
    assert(approxEqual(-1, asinh(-1.17520119364380145688), 1e-15))
    assert(approxEqual(0, asinh(0), 1e-15))
    assert(approxEqual(1, asinh(1.17520119364380145688), 1e-15))
    assert(approxEqual(2, asinh(3.62686040784701876767), 1e-15))
    assert(approxEqual(100, asinh(sinh(100)), 1e-15))

    assert(isnan(acosh(nan)))
    assert(isposinf(acosh(posinf)))
    assert(isnan(acosh(neginf)))
    assert(isnan(acosh(-1)))
    assert(isnan(acosh(0)))
    assert(isnan(acosh(0.5)))
    assert(approxEqual(0, acosh(1), 1e-15))
    assert(approxEqual(1, acosh(1.54308063481524377848), 1e-15))
    assert(approxEqual(2, acosh(3.76219569108363145956), 1e-15))
    assert(approxEqual(100, acosh(cosh(100)), 1e-15))

    assert(isnan(atanh(nan)))
    assert(isnan(atanh(posinf)))
    assert(isnan(atanh(neginf)))
    assert(approxEqual(-2, atanh(-0.96402758007581688395), 1e-15))
    assert(approxEqual(-1, atanh(-0.76159415595576488812), 1e-15))
    assert(approxEqual(0, atanh(0), 1e-15))
    assert(approxEqual(1, atanh(0.76159415595576488812), 1e-15))
    assert(approxEqual(2, atanh(0.96402758007581688395), 1e-15))

    assert(isnan(pow(nan, 2)))
    assert(isposinf(pow(posinf, 2)))
    assert(isposinf(pow(neginf, 2)))
    assert(isneginf(pow(neginf, 3)))
    assert(isnan(pow(2, nan)))
    assert(isposinf(pow(2, posinf)))
    assert(0 == (pow(2, neginf)))
    assert(isnan(pow(nan, nan)))
    assert(isnan(pow(nan, posinf)))
    assert(isnan(pow(nan, neginf)))
    assert(isnan(pow(posinf, nan)))
    assert(isnan(pow(neginf, nan)))
    assert(isposinf(pow(posinf, posinf)))
    assert(0 == (pow(posinf, neginf)))
    assert(isposinf(pow(neginf, posinf)))
    assert(0 == (pow(neginf, neginf)))
    assert(isnan(pow(-2.4, 1.3)))
    assert(4 == (pow(-2, 2)))
    assert(approxEqual(2, pow(4, 0.5), 1e-15))
    assert(approxEqual(4, pow(2, 2), 1e-15))

    assert(isnan(exp(nan)))
    assert(isposinf(exp(posinf)))
    assert(0 == (exp(neginf)))
    assert(approxEqual(E, exp(1), 1e-15))
    assert(approxEqual(E*E, exp(2), 1e-15))

    assert(isnan(log(nan)))
    assert(isposinf(log(posinf)))
    assert(isnan(log(neginf)))
    assert(approxEqual(0, log(1), 1e-15))
    assert(approxEqual(2, log(E*E), 1e-15))

    assert(isnan(exp2(nan)))
    assert(isposinf(exp2(posinf)))
    assert(0 == (exp2(neginf)))
    assert(approxEqual(2, exp2(1), 1e-15))
    assert(approxEqual(4, exp2(2), 1e-15))

    assert(isnan(log2(nan)))
    assert(isposinf(log2(posinf)))
    assert(isnan(log2(neginf)))
    assert(approxEqual(0, log2(1), 1e-15))
    assert(approxEqual(2, log2(4), 1e-15))

    assert(isnan(sqrt(nan)))
    assert(isposinf(sqrt(posinf)))
    assert(isnan(sqrt(neginf)))
    assert(approxEqual(1.4142135623730950488, sqrt(2), 1e-15))
    assert(approxEqual(2, sqrt(4), 1e-15))

    assert(isnan(inversesqrt(nan)))
    assert(0 == (inversesqrt(posinf)))
    assert(isnan(inversesqrt(neginf)))
    assert(approxEqual(1/1.4142135623730950488, inversesqrt(2), 1e-15))
    assert(approxEqual(0.5, inversesqrt(4), 1e-15))

    assert(isnan(abs(nan)))
    assert(isposinf(abs(posinf)))
    assert(isposinf(abs(neginf)))
    assert(1 == abs(-1))
    assert(0 == abs(0))
    assert(1 == abs(1))

    assert(isnan(sign(nan)))
    assert(1 == (sign(posinf)))
    assert(-1 == (sign(neginf)))
    assert(-1 == sign(-1))
    assert(0 == sign(0))
    assert(1 == sign(1))

    assert(isnan(floor(nan)))
    assert(isposinf(floor(posinf)))
    assert(isneginf(floor(neginf)))
    assert(-1 == floor(-1))
    assert(-1 == floor(-0.5))
    assert(0 == floor(0))
    assert(1 == floor(1.5))
    assert(1 == floor(1))

    assert(isnan(trunc(nan)))
    assert(isposinf(trunc(posinf)))
    assert(isneginf(trunc(neginf)))
    assert(-1 == trunc(-1))
    assert(-1 == trunc(-1.5))
    assert(0 == trunc(0))
    assert(1 == trunc(1.5))
    assert(1 == trunc(1))

    assert(isnan(round(nan)))
    assert(isposinf(round(posinf)))
    assert(isneginf(round(neginf)))
    assert(-1 == round(-1))
    assert(-1 == round(-1.4))
    assert(-2 == round(-1.6))
    assert(0 == round(0))
    assert(1 == round(1.4))
    assert(1 == round(1))
    assert(2 == round(1.6))

    assert(isnan(roundEven(nan)))
    assert(isposinf(roundEven(posinf)))
    assert(isneginf(roundEven(neginf)))
    assert(-1 == roundEven(-1))
    assert(-1 == roundEven(-1.4))
    assert(-2 == roundEven(-1.5))
    assert(-2 == roundEven(-1.6))
    assert(-2 == roundEven(-2.5))
    assert(0 == roundEven(0))
    assert(1 == roundEven(1))
    assert(1 == roundEven(1.4))
    assert(2 == roundEven(1.5))
    assert(2 == roundEven(1.6))
    assert(2 == roundEven(2.5))

    assert(isnan(ceil(nan)))
    assert(isposinf(ceil(posinf)))
    assert(isneginf(ceil(neginf)))
    assert(-1 == ceil(-1))
    assert(-1 == ceil(-1.1))
    assert(0 == ceil(0))
    assert(1 == ceil(1))
    assert(1 == ceil(0.1))

    assert(isnan(fract(nan)))
    assert(0 == (fract(posinf)))
    assert(0 == (fract(neginf)))
    assert(approxEqual(0.9, fract(-1.1), 1e-15))
    assert(0 == fract(-1))
    assert(0 == fract(0))
    assert(0 == fract(1))
    assert(0.25 == fract(1.25))

    assert(isnan(mod(2, nan)))
    assert(isnan(mod(2, posinf)))
    assert(isnan(mod(2, neginf)))
    assert(isnan(mod(nan, 2)))
    assert(isnan(mod(posinf, 2)))
    assert(isnan(mod(neginf, 2)))
    assert(0.25 == mod(10.25, 2.5))
    assert(-0.25 == mod(-10.25, -2.5))
    assert(2.25 == mod(-10.25, 2.5))
    assert(-2.25 == mod(10.25, -2.5))
    assert(0 == mod(0, 2.5))

    assert(isnan(min(nan, 2)))
    assert(2 == (min(posinf, 2)))
    assert(isneginf(min(neginf, 2)))
    assert(isnan(min(2, nan)))
    assert(2 == (min(2, posinf)))
    assert(isneginf(min(2, neginf)))
    assert(1 == min(1, 2))
    assert(1 == min(2, 1))
    assert(1 == min(1, 1))

    assert(isnan(max(nan, 2)))
    assert(isposinf(max(posinf, 2)))
    assert(2 == (max(neginf, 2)))
    assert(isnan(max(2, nan)))
    assert(isposinf(max(2, posinf)))
    assert(2 == (max(2, neginf)))
    assert(2 == max(1, 2))
    assert(2 == max(2, 1))
    assert(2 == max(2, 2))

    assert(isnan(clamp(nan, 1, 3)))
    assert(3 == (clamp(posinf, 1, 3)))
    assert(1 == (clamp(neginf, 1, 3)))
    assert(1 == clamp(0, 1, 3))
    assert(1 == clamp(1, 1, 3))
    assert(2 == clamp(2, 1, 3))
    assert(3 == clamp(3, 1, 3))
    assert(3 == clamp(4, 1, 3))

    assert(isnan(mix(nan, 4, 0.25)))
    assert(isposinf(mix(posinf, 4, 0.25)))
    assert(isneginf(mix(neginf, 4, 0.25)))
    assert(isnan(mix(0, nan, 0.25)))
    assert(isposinf(mix(0, posinf, 0.25)))
    assert(isneginf(mix(0, neginf, 0.25)))
    assert(isnan(mix(0, 4, nan)))
    assert(isnan(mix(0, 4, posinf)))
    assert(isnan(mix(0, 4, neginf)))
    assert(0 == mix(0, 4, 0))
    assert(1 == mix(0, 4, 0.25))
    assert(2 == mix(0, 4, 0.5))
    assert(3 == mix(0, 4, 0.75))
    assert(4 == mix(0, 4, 1))
    
    assert(isnan(step(2, nan)))
    assert(1 == (step(2, posinf)))
    assert(0 == (step(2, neginf)))
    assert(0 == step(2, 1))
    assert(1 == step(2, 2))
    assert(1 == step(2, 3))

    assert(isnan(smoothstep(1, 2, nan)))
    assert(1 == (smoothstep(1, 2, posinf)))
    assert(0 == (smoothstep(1, 2, neginf)))
    assert(0 == smoothstep(1, 2, 1))
    assert(approxEqual(0.104, smoothstep(1, 2, 1.2), 1e-15))
    assert(0.5 == smoothstep(1, 2, 1.5))
    assert(approxEqual(0.784, smoothstep(1, 2, 1.7), 1e-15))
    assert(1 == smoothstep(1, 2, 2))
    assert(1 == smoothstep(1, 2, 3))

    assert(0 == length(0))
    assert(1 == length(1))
    assert(1 == length(-1))
    assert(2.5 == length(2.5))
    assert(2.5 == length(-2.5))

    assert(2 == distance(-1, 1))
    assert(2.5 == distance(2, 4.5))
    assert(2 == distance(0, -2))

    assert(2 == dot(-1, -2))
    assert(0 == dot(0, 2))
    assert(-5.2 == dot(-2, 2.6))
    assert(12 == dot(4, 3))

    assert(-1 == normalize(-2.5))
    assert(-1 == normalize(-1))
    assert(-1 == normalize(-0.3))
    assert(0 == normalize(0))
    assert(1 == normalize(0.5))
    assert(1 == normalize(1))
    assert(1 == normalize(3.5))

    assert(-2.2 == faceforward(2.2, 3, 2))
    assert(-2.2 == faceforward(2.2, 0, 2))
    assert(2.2 == faceforward(2.2, 3, -2))
    
    assert(-1 == reflect(1, 1))
    assert(-2.5 == reflect(2.5, 1))
    assert(1 == reflect(-1, 1))
    assert(2.5 == reflect(-2.5, 1))
    assert(-1 == reflect(1, -1))
    assert(-2.5 == reflect(2.5, -1))
    assert(1 == reflect(-1, -1))
    assert(2.5 == reflect(-2.5, -1))

    assert(approxEqual(-1, refract(1, 1, 0.3), 1e-15))
    assert(approxEqual(1, refract(1, -1, 0.3), 1e-15))
    assert(approxEqual(-1, refract(-1, 1, 0.3), 1e-15))
    assert(approxEqual(1, refract(-1, -1, 0.3), 1e-15))
  }
}
