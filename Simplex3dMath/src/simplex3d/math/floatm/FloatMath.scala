/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009-2010 Simplex3d Team
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.BaseMath._
import java.lang.{StrictMath => SMath}
import ext.toxi.math.noise.SimplexNoise


/**
 * @author Aleksey Nikiforov (lex)
 */
object FloatMath {

    // Constants
    val FloatEpsilon: Float = 1.19209e-7f;
    val Pi: Float = float(Math.Pi)
    val E: Float = float(Math.E)

    private val DegToRad: Float = Pi / 180
    private val RadToDeg: Float = 180 / Pi
    private val InvLog2: Double = 1/SMath.log(2)

    // Have to be careful with large offsets due to the loss in precision.
    // With noise args as double these values should be ok
    private val of1: Double = 10000.0
    private val of2: Double = 20000.0
    private val of3: Double = 30000.0

    // Float functions
    def radians(x: Float) :Float = x*DegToRad
    def degrees(x: Float) :Float = x*RadToDeg

    def sin(w: Float) :Float = float(SMath.sin(w))
    def cos(w: Float) :Float = float(SMath.cos(w))
    def tan(w: Float) :Float = float(SMath.tan(w))

    def asin(w: Float) :Float = float(SMath.asin(w))
    def acos(w: Float) :Float = float(SMath.acos(w))
    def atan(y: Float, x: Float) :Float = float(SMath.atan2(y, x))
    def atan(w: Float) :Float = float(SMath.atan(w))

    def sinh(x: Float) :Float = float(SMath.sinh(x))
    def cosh(x: Float) :Float = float(SMath.cosh(x))
    def tanh(x: Float) :Float = float(SMath.tanh(x))

    def asinh(x: Float) :Float = {
        if (isneginf(x)) x
        else float(SMath.log(x + SMath.sqrt(x*x + 1)))
    }
    def acosh(x: Float) :Float = float(SMath.log(x + SMath.sqrt(x*x - 1)))
    def atanh(x: Float) :Float = float(0.5*SMath.log((1 + x)/(1 - x)))

    def pow(x: Float, y: Float) :Float = float(SMath.pow(x, y))
    def exp(x: Float) :Float = float(SMath.exp(x))
    def log(x: Float) :Float = float(SMath.log(x))

    def exp2(x: Float) :Float = float(SMath.pow(2, x))
    def log2(x: Float) :Float = float(SMath.log(x)*InvLog2)

    def sqrt(s: Float) :Float = float(SMath.sqrt(s))
    def inversesqrt(s: Float) :Float = float(1/SMath.sqrt(s))

    def abs(x: Float) :Float = { if (x > 0) x else -x }
    def sign(x: Float) :Float = {
        if (x > 0) 1f
        else if (x < 0) -1f
        else if (x == 0) 0f
        else x
    }
    def floor(x: Float) :Float = {
        if (x > Int.MaxValue || x < Int.MinValue) x
        else {
            val i = int(x)
            if (x > 0 || x == i) i else if(isnan(x)) x else i - 1
        }
    }
    def trunc(x: Float) :Float = {
        if (x > Int.MaxValue || x < Int.MinValue || isnan(x)) x
        else int(x)
    }
    def round(x: Float) :Float = {
        if (x > Int.MaxValue || x < Int.MinValue) x
        else if (x >= 0) int(x + 0.5f)
        else if (isnan(x)) x
        else int(x - 0.5f)
    }
    def roundEven(x: Float) :Float = float(SMath.rint(x))
    def ceil(x: Float) :Float = {
        if (x > Int.MaxValue) x
        else if (x < Int.MinValue) x
        else {
            val i = int(x)
            if (x < 0 || x == i) i else if (isnan(x)) x else i + 1
        }
    }
    /**
     * Equivalent to <code>x - floor(x)</code>
     */
    def fract(x: Float) :Float = {
        if (isinf(x)) 0
        else x - floor(x)
    }
    /**
     * Equivalent to <code>x - y*floor(x/y)</code>
     */
    def mod(x: Float, y: Float) :Float = {
        if (isinf(x)) Float.NaN
        else x - y*floor(x/y)
    }
    
    //not supported: lack of pointers
    //def modf(x: Float, i: Float) :Float = 0

    def min(x: Float, y: Float) :Float = if (y < x || isnan(y)) y else x
    def max(x: Float, y: Float) :Float = if (y > x || isnan(y)) y else x
    def clamp(x: Float, minVal: Float, maxVal: Float) :Float = {
        if (x <= minVal) minVal
        else if (x >= maxVal) maxVal
        else x
    }

    def mix(x: Float, y: Float, a: Float) :Float = x*(1 - a) + y*a
    def step(edge: Float, x: Float) :Float = if (x < edge) 0 else if (isnan(x)) x else 1
    def smoothstep(edge0: Float, edge1: Float, x: Float) :Float = {
        if (x <= edge0) 0
        else if (x >= edge1) 1
        else {
            val t = (x - edge0)/(edge1 - edge0)
            t*t*(3 - 2*t)
        }
    }

    def isnan(x: Float) :Boolean = java.lang.Float.isNaN(x)
    def isinf(x: Float) :Boolean = java.lang.Float.isInfinite(x)

    def length(x: Float) :Float = abs(x)
    def distance(x: Float, y: Float) :Float = abs(x - y)
    def dot(x: Float, y: Float) :Float = x*y
    def normalize(x: Float) :Float = sign(x)

    /**
     * This function flips the normal vector n to face the direction opposite
     * to incident vector i.
     * 
     * @param n normal
     * @param i incident vector
     * @param nref reference normal
     * @return n if (i*nref) < 0, -n otherwise
     */
    def faceforward(n: Float, i: Float, nref: Float) :Float = {
        if (i*nref < 0) n else -n
    }

    /**
     * This function reflects the incident vector i with respect to normal
     * vector n.
     *
     * @param n normal, must be normalized to achieve the desired result
     * @param i incident vector
     * @return reflection vector
     */
    def reflect(i: Float, n: Float) :Float = {
        i - 2*(n*i)*n
    }

    /**
     * This function refracts the incident vector i with respect to normal
     * vector n using the ratio of indices of refraction eta.
     *
     * @param n normal, must be normalized to achieve the desired result
     * @param i incident vector, must be normalized to achieve
     *          the desired result
     * @param eta the ratio of indices of refration
     * @return refraction vector
     */
    def refract(i: Float, n: Float, eta: Float) :Float = {
        val ni = n*i
        val k = 1 - eta*eta*(1 - ni*ni)
        if (k < 0) 0 else eta*i - (eta*ni + sqrt(k))*n
    }

    /**
     * meaningful return values for x withing [-1E9, +1E9]
     */
    def noise1(x: Float) :Float = float(SimplexNoise.noise(x, 0))
    def noise2(x: Float) :Vec2f = {
        new Vec2f(
            float(SimplexNoise.noise(x, 0)),
            float(SimplexNoise.noise(x, of1))
        )
    }
    def noise3(x: Float) :Vec3f = {
        new Vec3f(
            float(SimplexNoise.noise(x, 0)),
            float(SimplexNoise.noise(x, of1)),
            float(SimplexNoise.noise(x, of2))
        )
    }
    def noise4(x: Float) :Vec4f = {
        new Vec4f(
            float(SimplexNoise.noise(x, 0)),
            float(SimplexNoise.noise(x, of1)),
            float(SimplexNoise.noise(x, of2)),
            float(SimplexNoise.noise(x, of3))
        )
    }

    // Vec2f functions
    def radians(u: AnyVec2f) :Vec2f = new Vec2f(radians(u.x), radians(u.y))
    def degrees(u: AnyVec2f) :Vec2f = new Vec2f(degrees(u.x), degrees(u.y))

    def sin(u: AnyVec2f) :Vec2f = new Vec2f(sin(u.x), sin(u.y))
    def cos(u: AnyVec2f) :Vec2f = new Vec2f(cos(u.x), cos(u.y))
    def tan(u: AnyVec2f) :Vec2f = new Vec2f(tan(u.x), tan(u.y))

    def asin(u: AnyVec2f) :Vec2f = new Vec2f(asin(u.x), asin(u.y))
    def acos(u: AnyVec2f) :Vec2f = new Vec2f(acos(u.x), acos(u.y))
    def atan(uy: AnyVec2f, ux: AnyVec2f) :Vec2f = {
        new Vec2f(atan(uy.x, ux.x), atan(uy.y, ux.y))
    }
    def atan(u: AnyVec2f) :Vec2f = new Vec2f(atan(u.x), atan(u.y))

    def sinh(u: AnyVec2f) :Vec2f = new Vec2f(sinh(u.x), sinh(u.y))
    def cosh(u: AnyVec2f) :Vec2f = new Vec2f(cosh(u.x), cosh(u.y))
    def tanh(u: AnyVec2f) :Vec2f = new Vec2f(tanh(u.x), tanh(u.y))

    def asinh(u: AnyVec2f) :Vec2f = new Vec2f(asinh(u.x), asinh(u.y))
    def acosh(u: AnyVec2f) :Vec2f = new Vec2f(acosh(u.x), acosh(u.y))
    def atanh(u: AnyVec2f) :Vec2f = new Vec2f(atanh(u.x), atanh(u.y))

    def pow(u: AnyVec2f, v: AnyVec2f) :Vec2f = {
        new Vec2f(pow(u.x, v.x), pow(u.y, v.y))
    }
    def exp(u: AnyVec2f) :Vec2f = new Vec2f(exp(u.x), exp(u.y))
    def log(u: AnyVec2f) :Vec2f = new Vec2f(log(u.x), log(u.y))

    def exp2(u: AnyVec2f) :Vec2f = new Vec2f(exp2(u.x), exp2(u.y))
    def log2(u: AnyVec2f) :Vec2f = new Vec2f(log2(u.x), log2(u.y))

    def sqrt(u: AnyVec2f) :Vec2f = new Vec2f(sqrt(u.x), sqrt(u.y))
    def inversesqrt(u: AnyVec2f) :Vec2f = {
        new Vec2f(inversesqrt(u.x), inversesqrt(u.y))
    }

    def abs(u: AnyVec2f) :Vec2f = new Vec2f(abs(u.x), abs(u.y))
    def sign(u: AnyVec2f) :Vec2f = new Vec2f(sign(u.x), sign(u.y))
    def floor(u: AnyVec2f) :Vec2f = new Vec2f(floor(u.x), floor(u.y))
    def trunc(u: AnyVec2f) :Vec2f = new Vec2f(trunc(u.x), trunc(u.y))
    def round(u: AnyVec2f) :Vec2f = new Vec2f(round(u.x), round(u.y))
    def roundEven(u: AnyVec2f) :Vec2f = {
        new Vec2f(roundEven(u.x), roundEven(u.y))
    }
    def ceil(u: AnyVec2f) :Vec2f = new Vec2f(ceil(u.x), ceil(u.y))
    def fract(u: AnyVec2f) :Vec2f = new Vec2f(fract(u.x), fract(u.y))
    def mod(u: AnyVec2f, s: Float) :Vec2f = new Vec2f(mod(u.x, s), mod(u.y, s))
    def mod(u: AnyVec2f, v: AnyVec2f) :Vec2f = {
        new Vec2f(mod(u.x, v.x), mod(u.y, v.y))
    }
    def modf(u: AnyVec2f, i: Vec2f) :Vec2f = {
        i := trunc(u)
        u - i
    }

    def min(u: AnyVec2f, s: Float) :Vec2f = new Vec2f(min(u.x, s), min(u.y, s))
    def min(u: AnyVec2f, v: AnyVec2f) :Vec2f = {
        new Vec2f(min(u.x, v.x), min(u.y, v.y))
    }
    def max(u: AnyVec2f, s: Float) :Vec2f = new Vec2f(max(u.x, s), max(u.y, s))
    def max(u: AnyVec2f, v: AnyVec2f) :Vec2f = {
        new Vec2f(max(u.x, v.x), max(u.y, v.y))
    }
    def clamp(u: AnyVec2f, minVal: Float, maxVal: Float) :Vec2f = {
        new Vec2f(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
    }
    def clamp(u: AnyVec2f, minVal: AnyVec2f, maxVal: AnyVec2f) :Vec2f = {
        new Vec2f(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y)
        )
    }

    def mix(u: AnyVec2f, v: AnyVec2f, a: Float) :Vec2f = {
        val b = 1 - a
        new Vec2f(b*u.x + a*v.x, b*u.y + a*v.y)
    }
    def mix(u: AnyVec2f, v: AnyVec2f, a: AnyVec2f) :Vec2f = {
        new Vec2f(mix(u.x, v.x, a.x), mix(u.y, v.y, a.y))
    }
    def mix(u: AnyVec2f, v: AnyVec2f, a: AnyVec2b) :Vec2f = {
        new Vec2f(
            if (a.x) v.x else u.x,
            if (a.y) v.y else u.y
        )
    }

    def step(edge: Float, u: AnyVec2f) :Vec2f = {
        new Vec2f(step(edge, u.x), step(edge, u.y))
    }
    def step(edge: AnyVec2f, u: AnyVec2f) :Vec2f = {
        new Vec2f(step(edge.x, u.x), step(edge.y, u.y))
    }
    def smoothstep(edge0: Float, edge1: Float, u: AnyVec2f) :Vec2f = {
        new Vec2f(
            smoothstep(edge0, edge1, u.x),
            smoothstep(edge0, edge1, u.y)
        )
    }
    def smoothstep(edge0: AnyVec2f, edge1: AnyVec2f, u: AnyVec2f) :Vec2f = {
        new Vec2f(
            smoothstep(edge0.x, edge1.x, u.x),
            smoothstep(edge0.y, edge1.y, u.y)
        )
    }

    def isnan(u: AnyVec2f) :Vec2b = new Vec2b(isnan(u.x), isnan(u.y))
    def isinf(u: AnyVec2f) :Vec2b = new Vec2b(isinf(u.x), isinf(u.y))

    def length(u: AnyVec2f) :Float = sqrt(u.x*u.x + u.y*u.y)
    def distance(u: AnyVec2f, v: AnyVec2f) :Float = length(u - v)
    def dot(u: AnyVec2f, v: AnyVec2f) :Float = u.x * v.x + u.y * v.y
    def normalize(u: AnyVec2f) :Vec2f = u*inversesqrt(u.x*u.x + u.y*u.y)

    def faceforward(n: AnyVec2f, i: AnyVec2f, nref: AnyVec2f) :Vec2f = {
        if (dot(nref, i) < 0) Vec2f(n) else -n
    }

    def reflect(i: AnyVec2f, n: AnyVec2f) :Vec2f = {
        i - n*2*dot(n, i)
    }
    def refract(i: AnyVec2f, n: AnyVec2f, eta: Float) :Vec2f = {
        val dotni = dot(n, i)
        val k = 1 - eta*eta*(1 - dotni*dotni)
        if (k < 0) Vec2f(0) else i*eta - n*(eta*dotni + sqrt(k))
    }

    def lessThan(u: AnyVec2f, v: AnyVec2f) :Vec2b = {
        new Vec2b(
            u.x < v.x,
            u.y < v.y
        )
    }
    def lessThanEqual(u: AnyVec2f, v: AnyVec2f) :Vec2b = {
        new Vec2b(
            u.x <= v.x,
            u.y <= v.y
        )
    }
    def greaterThan(u: AnyVec2f, v: AnyVec2f) :Vec2b = {
        new Vec2b(
            u.x > v.x,
            u.y > v.y
        )
    }
    def greaterThanEqual(u: AnyVec2f, v: AnyVec2f) :Vec2b = {
        new Vec2b(
            u.x >= v.x,
            u.y >= v.y
        )
    }
    def equal(u: AnyVec2f, v: AnyVec2f) :Vec2b = {
        new Vec2b(
            u.x == v.x,
            u.y == v.y
        )
    }
    def notEqual(u: AnyVec2f, v: AnyVec2f) :Vec2b = {
        new Vec2b(
            u.x != v.x,
            u.y != v.y
        )
    }

    def noise1(u: AnyVec2f) :Float = {
        float(SimplexNoise.noise(u.x, u.y))
    }
    def noise2(u: AnyVec2f) :Vec2f = {
        new Vec2f(
            float(SimplexNoise.noise(u.x, u.y)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1))
        )
    }
    def noise3(u: AnyVec2f) :Vec3f = {
        new Vec3f(
            float(SimplexNoise.noise(u.x, u.y)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1)),
            float(SimplexNoise.noise(u.x + of2, u.y + of2))
        )
    }
    def noise4(u: AnyVec2f) :Vec4f = {
        new Vec4f(
            float(SimplexNoise.noise(u.x, u.y)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1)),
            float(SimplexNoise.noise(u.x + of2, u.y + of2)),
            float(SimplexNoise.noise(u.x + of3, u.y + of3))
        )
    }


    // Vec3f functions
    def radians(u: AnyVec3f) :Vec3f = {
        new Vec3f(radians(u.x), radians(u.y), radians(u.z))
    }
    def degrees(u: AnyVec3f) :Vec3f = {
        new Vec3f(degrees(u.x), degrees(u.y), degrees(u.z))
    }

    def sin(u: AnyVec3f) :Vec3f = new Vec3f(sin(u.x), sin(u.y), sin(u.z))
    def cos(u: AnyVec3f) :Vec3f = new Vec3f(cos(u.x), cos(u.y), cos(u.z))
    def tan(u: AnyVec3f) :Vec3f = new Vec3f(tan(u.x), tan(u.y), tan(u.z))

    def asin(u: AnyVec3f) :Vec3f = new Vec3f(asin(u.x), asin(u.y), asin(u.z))
    def acos(u: AnyVec3f) :Vec3f = new Vec3f(acos(u.x), acos(u.y), acos(u.z))
    def atan(uy: AnyVec3f, ux: AnyVec3f) :Vec3f = {
        new Vec3f(atan(uy.x, ux.x), atan(uy.y, ux.y), atan(uy.z, ux.z))
    }
    def atan(u: AnyVec3f) :Vec3f = new Vec3f(atan(u.x), atan(u.y), atan(u.z))

    def sinh(u: AnyVec3f) :Vec3f = new Vec3f(sinh(u.x), sinh(u.y), sinh(u.z))
    def cosh(u: AnyVec3f) :Vec3f = new Vec3f(cosh(u.x), cosh(u.y), cosh(u.z))
    def tanh(u: AnyVec3f) :Vec3f = new Vec3f(tanh(u.x), tanh(u.y), tanh(u.z))

    def asinh(u: AnyVec3f) :Vec3f = {
        new Vec3f(asinh(u.x), asinh(u.y), asinh(u.z))
    }
    def acosh(u: AnyVec3f) :Vec3f = {
        new Vec3f(acosh(u.x), acosh(u.y), acosh(u.z))
    }
    def atanh(u: AnyVec3f) :Vec3f = {
        new Vec3f(atanh(u.x), atanh(u.y), atanh(u.z))
    }

    def pow(u: AnyVec3f, v: AnyVec3f) :Vec3f = {
        new Vec3f(pow(u.x, v.x), pow(u.y, v.y), pow(u.z, v.z))
    }
    def exp(u: AnyVec3f) :Vec3f = new Vec3f(exp(u.x), exp(u.y), exp(u.z))
    def log(u: AnyVec3f) :Vec3f = new Vec3f(log(u.x), log(u.y), log(u.z))

    def exp2(u: AnyVec3f) :Vec3f = new Vec3f(exp2(u.x), exp2(u.y), exp2(u.z))
    def log2(u: AnyVec3f) :Vec3f = new Vec3f(log2(u.x), log2(u.y), log2(u.z))

    def sqrt(u: AnyVec3f) :Vec3f = new Vec3f(sqrt(u.x), sqrt(u.y), sqrt(u.z))
    def inversesqrt(u: AnyVec3f) :Vec3f = {
        new Vec3f(inversesqrt(u.x), inversesqrt(u.y), inversesqrt(u.z))
    }

    def abs(u: AnyVec3f) :Vec3f = new Vec3f(abs(u.x), abs(u.y), abs(u.z))
    def sign(u: AnyVec3f) :Vec3f = new Vec3f(sign(u.x), sign(u.y), sign(u.z))
    def floor(u: AnyVec3f) :Vec3f = {
        new Vec3f(floor(u.x), floor(u.y), floor(u.z))
    }
    def trunc(u: AnyVec3f) :Vec3f = {
        new Vec3f(trunc(u.x), trunc(u.y), trunc(u.z))
    }
    def round(u: AnyVec3f) :Vec3f = {
        new Vec3f(round(u.x), round(u.y), round(u.z))
    }
    def roundEven(u: AnyVec3f) :Vec3f = {
        new Vec3f(roundEven(u.x), roundEven(u.y), roundEven(u.z))
    }
    def ceil(u: AnyVec3f) :Vec3f = new Vec3f(ceil(u.x), ceil(u.y), ceil(u.z))
    def fract(u: AnyVec3f) :Vec3f = {
        new Vec3f(fract(u.x), fract(u.y), fract(u.z))
    }
    def mod(u: AnyVec3f, s: Float) :Vec3f = {
        new Vec3f(mod(u.x, s), mod(u.y, s), mod(u.z, s))
    }
    def mod(u: AnyVec3f, v: AnyVec3f) :Vec3f = {
        new Vec3f(mod(u.x, v.x), mod(u.y, v.y), mod(u.z, v.z))
    }
    def modf(u: AnyVec3f, i: Vec3f) :Vec3f = {
        i := trunc(u)
        u - i
    }

    def min(u: AnyVec3f, s: Float) :Vec3f = {
        new Vec3f(min(u.x, s), min(u.y, s), min(u.z, s))
    }
    def min(u: AnyVec3f, v: AnyVec3f) :Vec3f = {
        new Vec3f(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
    }
    def max(u: AnyVec3f, s: Float) :Vec3f = {
        new Vec3f(max(u.x, s), max(u.y, s), max(u.z, s))
    }
    def max(u: AnyVec3f, v: AnyVec3f) :Vec3f = {
        new Vec3f(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
    }
    def clamp(u: AnyVec3f, minVal: Float, maxVal: Float) :Vec3f = {
        new Vec3f(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec3f, minVal: AnyVec3f, maxVal: AnyVec3f) :Vec3f = {
        new Vec3f(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z)
        )
    }

    def mix(u: AnyVec3f, v: AnyVec3f, a: Float) :Vec3f = {
        val b = 1 - a
        new Vec3f(b*u.x + a*v.x, b*u.y + a*v.y, b*u.z + a*v.z)
    }
    def mix(u: AnyVec3f, v: AnyVec3f, a: AnyVec3f) :Vec3f = {
        new Vec3f(mix(u.x, v.x, a.x), mix(u.y, v.y, a.y), mix(u.z, v.z, a.z))
    }
    def mix(u: AnyVec3f, v: AnyVec3f, a: AnyVec3b) :Vec3f = {
        new Vec3f(
            if (a.x) v.x else u.x,
            if (a.y) v.y else u.y,
            if (a.z) v.z else u.z
        )
    }

    def step(edge: Float, u: AnyVec3f) :Vec3f = {
        new Vec3f(step(edge, u.x), step(edge, u.y), step(edge, u.z))
    }
    def step(edge: AnyVec3f, u: AnyVec3f) :Vec3f = {
        new Vec3f(step(edge.x, u.x), step(edge.y, u.y), step(edge.z, u.z))
    }
    def smoothstep(edge0: Float, edge1: Float, u: AnyVec3f) :Vec3f = {
        new Vec3f(
            smoothstep(edge0, edge1, u.x),
            smoothstep(edge0, edge1, u.y),
            smoothstep(edge0, edge1, u.z)
        )
    }
    def smoothstep(edge0: AnyVec3f, edge1: AnyVec3f, u: AnyVec3f) :Vec3f = {
        new Vec3f(
            smoothstep(edge0.x, edge1.x, u.x),
            smoothstep(edge0.y, edge1.y, u.y),
            smoothstep(edge0.z, edge1.z, u.z)
        )
    }

    def isnan(u: AnyVec3f) :Vec3b = {
        new Vec3b(isnan(u.x), isnan(u.y), isnan(u.z))
    }
    def isinf(u: AnyVec3f) :Vec3b = {
        new Vec3b(isinf(u.x), isinf(u.y), isinf(u.z))
    }

    def length(u: AnyVec3f) :Float = sqrt(u.x*u.x + u.y*u.y + u.z*u.z)
    def distance(u: AnyVec3f, v: AnyVec3f) :Float = length(u - v)
    def dot(u: AnyVec3f, v: AnyVec3f) :Float = u.x*v.x + u.y*v.y + u.z*v.z
    def cross(u: AnyVec3f, v: AnyVec3f) :Vec3f = {
        new Vec3f(
            u.y*v.z-v.y*u.z,
            u.z*v.x-v.z*u.x,
            u.x*v.y-v.x*u.y
        )
    }
    def normalize(u: AnyVec3f) :Vec3f = {
        u*inversesqrt(u.x*u.x + u.y*u.y + u.z*u.z)
    }

    def faceforward(n: AnyVec3f, i: AnyVec3f, nref: AnyVec3f) :Vec3f = {
        if (dot(nref, i) < 0) Vec3f(n) else -n
    }

    def reflect(i: AnyVec3f, n: AnyVec3f) :Vec3f = {
        i - n*2*dot(n, i)
    }
    def refract(i: AnyVec3f, n: AnyVec3f, eta: Float) :Vec3f = {
        val dotni = dot(n, i)
        val k = 1 - eta*eta*(1 - dotni*dotni)
        if (k < 0) Vec3f(0) else i*eta - n*(eta*dotni + sqrt(k))
    }

    def lessThan(u: AnyVec3f, v: AnyVec3f) :Vec3b = {
        new Vec3b(
            u.x < v.x,
            u.y < v.y,
            u.z < v.z
        )
    }
    def lessThanEqual(u: AnyVec3f, v: AnyVec3f) :Vec3b = {
        new Vec3b(
            u.x <= v.x,
            u.y <= v.y,
            u.z <= v.z
        )
    }
    def greaterThan(u: AnyVec3f, v: AnyVec3f) :Vec3b = {
        new Vec3b(
            u.x > v.x,
            u.y > v.y,
            u.z > v.z
        )
    }
    def greaterThanEqual(u: AnyVec3f, v: AnyVec3f) :Vec3b = {
        new Vec3b(
            u.x >= v.x,
            u.y >= v.y,
            u.z >= v.z
        )
    }
    def equal(u: AnyVec3f, v: AnyVec3f) :Vec3b = {
        new Vec3b(
            u.x == v.x,
            u.y == v.y,
            u.z == v.z
        )
    }
    def notEqual(u: AnyVec3f, v: AnyVec3f) :Vec3b = {
        new Vec3b(
            u.x != v.x,
            u.y != v.y,
            u.z != v.z
        )
    }

    def noise1(u: AnyVec3f) :Float = {
        float(SimplexNoise.noise(u.x, u.y, u.z))
    }
    def noise2(u: AnyVec3f) :Vec2f = {
        new Vec2f(
            float(SimplexNoise.noise(u.x, u.y, u.z)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1))
        )
    }
    def noise3(u: AnyVec3f) :Vec3f = {
        new Vec3f(
            float(SimplexNoise.noise(u.x, u.y, u.z)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1)),
            float(SimplexNoise.noise(u.x + of2, u.y + of2, u.z + of2))
        )
    }
    def noise4(u: AnyVec3f) :Vec4f = {
        new Vec4f(
            float(SimplexNoise.noise(u.x, u.y, u.z)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1)),
            float(SimplexNoise.noise(u.x + of2, u.y + of2, u.z + of2)),
            float(SimplexNoise.noise(u.x + of3, u.y + of3, u.z + of3))
        )
    }

    // Vec4f functions
    def radians(u: AnyVec4f) :Vec4f = {
        new Vec4f(radians(u.x), radians(u.y), radians(u.z), radians(u.w))
    }
    def degrees(u: AnyVec4f) :Vec4f = {
        new Vec4f(degrees(u.x), degrees(u.y), degrees(u.z), degrees(u.w))
    }

    def sin(u: AnyVec4f) :Vec4f = {
        new Vec4f(sin(u.x), sin(u.y), sin(u.z), sin(u.w))
    }
    def cos(u: AnyVec4f) :Vec4f = {
        new Vec4f(cos(u.x), cos(u.y), cos(u.z), cos(u.w))
    }
    def tan(u: AnyVec4f) :Vec4f = {
        new Vec4f(tan(u.x), tan(u.y), tan(u.z), tan(u.w))
    }

    def asin(u: AnyVec4f) :Vec4f = {
        new Vec4f(asin(u.x), asin(u.y), asin(u.z), asin(u.w))
    }
    def acos(u: AnyVec4f) :Vec4f = {
        new Vec4f(acos(u.x), acos(u.y), acos(u.z), acos(u.w))
    }
    def atan(uy: AnyVec4f, ux: AnyVec4f) :Vec4f = {
        new Vec4f(
            atan(uy.x, ux.x),
            atan(uy.y, ux.y),
            atan(uy.z, ux.z),
            atan(uy.w, ux.w)
        )
    }
    def atan(u: AnyVec4f) :Vec4f = {
        new Vec4f(atan(u.x), atan(u.y), atan(u.z), atan(u.w))
    }

    def sinh(u: AnyVec4f) :Vec4f = {
        new Vec4f(sinh(u.x), sinh(u.y), sinh(u.z), sinh(u.w))
    }
    def cosh(u: AnyVec4f) :Vec4f = {
        new Vec4f(cosh(u.x), cosh(u.y), cosh(u.z), cosh(u.w))
    }
    def tanh(u: AnyVec4f) :Vec4f = {
        new Vec4f(tanh(u.x), tanh(u.y), tanh(u.z), tanh(u.w))
    }

    def asinh(u: AnyVec4f) :Vec4f = {
        new Vec4f(asinh(u.x), asinh(u.y), asinh(u.z), asinh(u.w))
    }
    def acosh(u: AnyVec4f) :Vec4f = {
        new Vec4f(acosh(u.x), acosh(u.y), acosh(u.z), acosh(u.w))
    }
    def atanh(u: AnyVec4f) :Vec4f = {
        new Vec4f(atanh(u.x), atanh(u.y), atanh(u.z), atanh(u.w))
    }

    def pow(u: AnyVec4f, v: AnyVec4f) :Vec4f = {
        new Vec4f(pow(u.x, v.x), pow(u.y, v.y), pow(u.z, v.z), pow(u.w, v.w))
    }
    def exp(u: AnyVec4f) :Vec4f = {
        new Vec4f(exp(u.x), exp(u.y), exp(u.z), exp(u.w))
    }
    def log(u: AnyVec4f) :Vec4f = {
        new Vec4f(log(u.x), log(u.y), log(u.z), log(u.w))
    }

    def exp2(u: AnyVec4f) :Vec4f = {
        new Vec4f(exp2(u.x), exp2(u.y), exp2(u.z), exp2(u.w))
    }
    def log2(u: AnyVec4f) :Vec4f = {
        new Vec4f(log2(u.x), log2(u.y), log2(u.z), log2(u.w))
    }

    def sqrt(u: AnyVec4f) :Vec4f = {
        new Vec4f(sqrt(u.x), sqrt(u.y), sqrt(u.z), sqrt(u.w))
    }
    def inversesqrt(u: AnyVec4f) :Vec4f = {
        new Vec4f(
            inversesqrt(u.x),
            inversesqrt(u.y),
            inversesqrt(u.z),
            inversesqrt(u.w)
        )
    }

    def abs(u: AnyVec4f) :Vec4f = {
        new Vec4f(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
    }
    def sign(u: AnyVec4f) :Vec4f = {
        new Vec4f(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
    }
    def floor(u: AnyVec4f) :Vec4f = {
        new Vec4f(floor(u.x), floor(u.y), floor(u.z), floor(u.w))
    }
    def trunc(u: AnyVec4f) :Vec4f = {
        new Vec4f(trunc(u.x), trunc(u.y), trunc(u.z), trunc(u.w))
    }
    def round(u: AnyVec4f) :Vec4f = {
        new Vec4f(round(u.x), round(u.y), round(u.z), round(u.w))
    }
    def roundEven(u: AnyVec4f) :Vec4f = {
        new Vec4f(roundEven(u.x), roundEven(u.y),
                  roundEven(u.z), roundEven(u.w))
    }
    def ceil(u: AnyVec4f) :Vec4f = {
        new Vec4f(ceil(u.x), ceil(u.y), ceil(u.z), ceil(u.w))
    }
    def fract(u: AnyVec4f) :Vec4f = {
        new Vec4f(fract(u.x), fract(u.y), fract(u.z), fract(u.w))
    }
    def mod(u: AnyVec4f, s: Float) :Vec4f = {
        new Vec4f(mod(u.x, s), mod(u.y, s), mod(u.z, s), mod(u.w, s))
    }
    def mod(u: AnyVec4f, v: AnyVec4f) :Vec4f = {
        new Vec4f(mod(u.x, v.x), mod(u.y, v.y), mod(u.z, v.z), mod(u.w, v.w))
    }
    def modf(u: AnyVec4f, i: Vec4f) :Vec4f = {
        i := trunc(u)
        u - i
    }

    def min(u: AnyVec4f, s: Float) :Vec4f = {
        new Vec4f(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
    }
    def min(u: AnyVec4f, v: AnyVec4f) :Vec4f = {
        new Vec4f(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
    }
    def max(u: AnyVec4f, s: Float) :Vec4f = {
        new Vec4f(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
    }
    def max(u: AnyVec4f, v: AnyVec4f) :Vec4f = {
        new Vec4f(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
    }
    def clamp(u: AnyVec4f, minVal: Float, maxVal: Float) :Vec4f = {
        new Vec4f(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal),
            clamp(u.w, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec4f, minVal: AnyVec4f, maxVal: AnyVec4f) :Vec4f = {
        new Vec4f(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z),
            clamp(u.w, minVal.w, maxVal.w)
        )
    }

    def mix(u: AnyVec4f, v: AnyVec4f, a: Float) :Vec4f = {
        val b = 1 - a
        new Vec4f(b*u.x + a*v.x, b*u.y + a*v.y, b*u.z + a*v.z, b*u.w + a*v.w)
    }
    def mix(u: AnyVec4f, v: AnyVec4f, a: AnyVec4f) :Vec4f = {
        new Vec4f(
            mix(u.x, v.x, a.x),
            mix(u.y, v.y, a.y),
            mix(u.z, v.z, a.z),
            mix(u.w, v.w, a.w)
        )
    }
    def mix(u: AnyVec4f, v: AnyVec4f, a: AnyVec4b) :Vec4f = {
        new Vec4f(
            if (a.x) v.x else u.x,
            if (a.y) v.y else u.y,
            if (a.z) v.z else u.z,
            if (a.w) v.w else u.w
        )
    }

    def step(edge: Float, u: AnyVec4f) :Vec4f = {
        new Vec4f(step(edge, u.x), step(edge, u.y),
              step(edge, u.z), step(edge, u.w))
    }
    def step(edge: AnyVec4f, u: AnyVec4f) :Vec4f = {
        new Vec4f(
            step(edge.x, u.x),
            step(edge.y, u.y),
            step(edge.z, u.z),
            step(edge.w, u.w)
        )
    }
    def smoothstep(edge0: Float, edge1: Float, u: AnyVec4f) :Vec4f = {
        new Vec4f(
            smoothstep(edge0, edge1, u.x),
            smoothstep(edge0, edge1, u.y),
            smoothstep(edge0, edge1, u.z),
            smoothstep(edge0, edge1, u.w)
        )
    }
    def smoothstep(edge0: AnyVec4f, edge1: AnyVec4f, u: AnyVec4f) :Vec4f = {
        new Vec4f(
            smoothstep(edge0.x, edge1.x, u.x),
            smoothstep(edge0.y, edge1.y, u.y),
            smoothstep(edge0.z, edge1.z, u.z),
            smoothstep(edge0.w, edge1.w, u.w)
        )
    }

    def isnan(u: AnyVec4f) :Vec4b = {
        new Vec4b(isnan(u.x), isnan(u.y), isnan(u.z), isnan(u.w))
    }
    def isinf(u: AnyVec4f) :Vec4b = {
        new Vec4b(isinf(u.x), isinf(u.y), isinf(u.z), isinf(u.w))
    }

    def length(u: AnyVec4f) :Float = sqrt(u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w)
    def dot(u: AnyVec4f, v: AnyVec4f) :Float = {
        u.x*v.x + u.y*v.y + u.z*v.z + u.w*v.w
    }
    def distance(u: AnyVec4f, v: AnyVec4f) :Float = length(u - v)
    def normalize(u: AnyVec4f) :Vec4f = {
        u*inversesqrt(u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w)
    }

    def faceforward(n: AnyVec4f, i: AnyVec4f, nref: AnyVec4f) :Vec4f = {
        if (dot(nref, i) < 0) Vec4f(n) else -n
    }

    def reflect(i: AnyVec4f, n: AnyVec4f) :Vec4f = {
        i - n*2*dot(n, i)
    }
    def refract(i: AnyVec4f, n: AnyVec4f, eta: Float) :Vec4f = {
        val dotni = dot(n, i)
        val k = 1 - eta*eta*(1 - dotni*dotni)
        if (k < 0) Vec4f(0) else i*eta - n*(eta*dotni + sqrt(k))
    }

    def lessThan(u: AnyVec4f, v: AnyVec4f) :Vec4b = {
        new Vec4b(
            u.x < v.x,
            u.y < v.y,
            u.z < v.z,
            u.w < v.w
        )
    }
    def lessThanEqual(u: AnyVec4f, v: AnyVec4f) :Vec4b = {
        new Vec4b(
            u.x <= v.x,
            u.y <= v.y,
            u.z <= v.z,
            u.w <= v.w
        )
    }
    def greaterThan(u: AnyVec4f, v: AnyVec4f) :Vec4b = {
        new Vec4b(
            u.x > v.x,
            u.y > v.y,
            u.z > v.z,
            u.w > v.w
        )
    }
    def greaterThanEqual(u: AnyVec4f, v: AnyVec4f) :Vec4b = {
        new Vec4b(
            u.x >= v.x,
            u.y >= v.y,
            u.z >= v.z,
            u.w >= v.w
        )
    }
    def equal(u: AnyVec4f, v: AnyVec4f) :Vec4b = {
        new Vec4b(
            u.x == v.x,
            u.y == v.y,
            u.z == v.z,
            u.w == v.w
        )
    }
    def notEqual(u: AnyVec4f, v: AnyVec4f) :Vec4b = {
        new Vec4b(
            u.x != v.x,
            u.y != v.y,
            u.z != v.z,
            u.w != v.w
        )
    }

    def noise1(u: AnyVec4f) :Float = {
        float(SimplexNoise.noise(u.x, u.y, u.z, u.w))
    }
    def noise2(u: AnyVec4f) :Vec2f = {
        new Vec2f(
          float(SimplexNoise.noise(u.x, u.y, u.z, u.w)),
          float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1, u.w + of1))
        )
    }
    def noise3(u: AnyVec4f) :Vec3f = {
        new Vec3f(
          float(SimplexNoise.noise(u.x, u.y, u.z, u.w)),
          float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1, u.w + of1)),
          float(SimplexNoise.noise(u.x + of2, u.y + of2, u.z + of2, u.w + of2))
        )
    }
    def noise4(u: AnyVec4f) :Vec4f = {
        new Vec4f(
          float(SimplexNoise.noise(u.x, u.y, u.z, u.w)),
          float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1, u.w + of1)),
          float(SimplexNoise.noise(u.x + of2, u.y + of2, u.z + of2, u.w + of2)),
          float(SimplexNoise.noise(u.x + of3, u.y + of3, u.z + of3, u.w + of3))
        )
    }

    // Mat functions
    def matrixCompMult(a: Mat2f, b: Mat2f) :Mat2f = {
        new Mat2f(
            a.m00*b.m00, a.m10*b.m10,
            a.m01*b.m01, a.m11*b.m11
        )
    }
    def matrixCompMult(a: Mat2x3f, b: Mat2x3f) :Mat2x3f = {
        new Mat2x3f(
            a.m00*b.m00, a.m10*b.m10,
            a.m01*b.m01, a.m11*b.m11,
            a.m02*b.m02, a.m12*b.m12
        )
    }
    def matrixCompMult(a: Mat2x4f, b: Mat2x4f) :Mat2x4f = {
        new Mat2x4f(
            a.m00*b.m00, a.m10*b.m10,
            a.m01*b.m01, a.m11*b.m11,
            a.m02*b.m02, a.m12*b.m12,
            a.m03*b.m03, a.m13*b.m13
        )
    }
    def matrixCompMult(a: Mat3x2f, b: Mat3x2f) :Mat3x2f = {
        new Mat3x2f(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
            a.m01*b.m01, a.m11*b.m11, a.m21*b.m21
        )
    }
    def matrixCompMult(a: Mat3f, b: Mat3f) :Mat3f = {
        new Mat3f(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
            a.m01*b.m01, a.m11*b.m11, a.m21*b.m21,
            a.m02*b.m02, a.m12*b.m12, a.m22*b.m22
        )
    }
    def matrixCompMult(a: Mat3x4f, b: Mat3x4f) :Mat3x4f = {
        new Mat3x4f(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
            a.m01*b.m01, a.m11*b.m11, a.m21*b.m21,
            a.m02*b.m02, a.m12*b.m12, a.m22*b.m22,
            a.m03*b.m03, a.m13*b.m13, a.m23*b.m23
        )
    }
    def matrixCompMult(a: Mat4x2f, b: Mat4x2f) :Mat4x2f = {
        new Mat4x2f(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
            a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31
        )
    }
    def matrixCompMult(a: Mat4x3f, b: Mat4x3f) :Mat4x3f = {
        new Mat4x3f(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
            a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31,
            a.m02*b.m02, a.m12*b.m12, a.m22*b.m22, a.m32*b.m32
        )
    }
    def matrixCompMult(a: Mat4f, b: Mat4f) :Mat4f = {
        new Mat4f(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
            a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31,
            a.m02*b.m02, a.m12*b.m12, a.m22*b.m22, a.m32*b.m32,
            a.m03*b.m03, a.m13*b.m13, a.m23*b.m23, a.m33*b.m33
        )
    }

    def outerProduct(c: AnyVec2f, r: AnyVec2f) :Mat2f = {
        new Mat2f(
            c.x*r.x, c.y*r.x,
            c.x*r.y, c.y*r.y
        )
    }
    def outerProduct(c: AnyVec2f, r: AnyVec3f) :Mat2x3f = {
        new Mat2x3f(
            c.x*r.x, c.y*r.x,
            c.x*r.y, c.y*r.y,
            c.x*r.z, c.y*r.z
        )
    }
    def outerProduct(c: AnyVec2f, r: AnyVec4f) :Mat2x4f = {
        new Mat2x4f(
            c.x*r.x, c.y*r.x,
            c.x*r.y, c.y*r.y,
            c.x*r.z, c.y*r.z,
            c.x*r.w, c.y*r.w
        )
    }
    def outerProduct(c: AnyVec3f, r: AnyVec2f) :Mat3x2f = {
        new Mat3x2f(
            c.x*r.x, c.y*r.x, c.z*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y
        )
    }
    def outerProduct(c: AnyVec3f, r: AnyVec3f) :Mat3f = {
        new Mat3f(
            c.x*r.x, c.y*r.x, c.z*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y,
            c.x*r.z, c.y*r.z, c.z*r.z
        )
    }
    def outerProduct(c: AnyVec3f, r: AnyVec4f) :Mat3x4f = {
        new Mat3x4f(
            c.x*r.x, c.y*r.x, c.z*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y,
            c.x*r.z, c.y*r.z, c.z*r.z,
            c.x*r.w, c.y*r.w, c.z*r.w
        )
    }
    def outerProduct(c: AnyVec4f, r: AnyVec2f) :Mat4x2f = {
        new Mat4x2f(
            c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y
        )
    }
    def outerProduct(c: AnyVec4f, r: AnyVec3f) :Mat4x3f = {
        new Mat4x3f(
            c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y,
            c.x*r.z, c.y*r.z, c.z*r.z, c.w*r.z
        )
    }
    def outerProduct(c: AnyVec4f, r: AnyVec4f) :Mat4f = {
        new Mat4f(
            c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y,
            c.x*r.z, c.y*r.z, c.z*r.z, c.w*r.z,
            c.x*r.w, c.y*r.w, c.z*r.w, c.w*r.w
        )
    }

    def transpose(a: AnyMat2f) :Mat2f = {
        new Mat2f(
            a.m00, a.m01,
            a.m10, a.m11
        )
    }
    def transpose(a: AnyMat3x2f) :Mat2x3f = {
        new Mat2x3f(
            a.m00, a.m01,
            a.m10, a.m11,
            a.m20, a.m21
        )
    }
    def transpose(a: AnyMat4x2f) :Mat2x4f = {
        new Mat2x4f(
            a.m00, a.m01,
            a.m10, a.m11,
            a.m20, a.m21,
            a.m30, a.m31
        )
    }
    def transpose(a: AnyMat2x3f) :Mat3x2f = {
        new Mat3x2f(
            a.m00, a.m01, a.m02,
            a.m10, a.m11, a.m12
        )
    }
    def transpose(a: AnyMat3f) :Mat3f = {
        new Mat3f(
            a.m00, a.m01, a.m02,
            a.m10, a.m11, a.m12,
            a.m20, a.m21, a.m22
        )
    }
    def transpose(a: AnyMat4x3f) :Mat3x4f = {
        new Mat3x4f(
            a.m00, a.m01, a.m02,
            a.m10, a.m11, a.m12,
            a.m20, a.m21, a.m22,
            a.m30, a.m31, a.m32
        )
    }
    def transpose(a: AnyMat2x4f) :Mat4x2f = {
        new Mat4x2f(
            a.m00, a.m01, a.m02, a.m03,
            a.m10, a.m11, a.m12, a.m13
        )
    }
    def transpose(a: AnyMat3x4f) :Mat4x3f = {
        new Mat4x3f(
            a.m00, a.m01, a.m02, a.m03,
            a.m10, a.m11, a.m12, a.m13,
            a.m20, a.m21, a.m22, a.m23
        )
    }
    def transpose(a: AnyMat4f) :Mat4f = {
        new Mat4f(
            a.m00, a.m01, a.m02, a.m03,
            a.m10, a.m11, a.m12, a.m13,
            a.m20, a.m21, a.m22, a.m23,
            a.m30, a.m31, a.m32, a.m33
        )
    }

    def determinant(m: AnyMat2f) :Float = m.m00*m.m11 - m.m01*m.m10

    def determinant(m: AnyMat3f) :Float = {
        import m._

        val c0 = m11*m22 - m12*m21
        val c1 = m12*m20 - m10*m22
        val c2 = m10*m21 - m11*m20

        m00*c0 + m01*c1 + m02*c2
    }

    def determinant(m: AnyMat4f) :Float = {
        import m._

        val fA0 = m00*m11 - m01*m10
        val fA1 = m00*m12 - m02*m10
        val fA2 = m00*m13 - m03*m10
        val fA3 = m01*m12 - m02*m11
        val fA4 = m01*m13 - m03*m11
        val fA5 = m02*m13 - m03*m12
        val fB0 = m20*m31 - m21*m30
        val fB1 = m20*m32 - m22*m30
        val fB2 = m20*m33 - m23*m30
        val fB3 = m21*m32 - m22*m31
        val fB4 = m21*m33 - m23*m31
        val fB5 = m22*m33 - m23*m32

        fA0*fB5 - fA1*fB4 + fA2*fB3 + fA3*fB2 - fA4*fB1 + fA5*fB0
    }

    /**
     * If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat2f) :Mat2f = {
        val detInv = 1/determinant(m)
        new Mat2f(
            m.m11*detInv, -m.m10*detInv,
            -m.m01*detInv, m.m00*detInv
        )
    }

    /**
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using Transform3f.inverse(scale, rotation, translation).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat3f) :Mat3f = {
        import m._

        val c0 = m11*m22 - m12*m21
        val c1 = m12*m20 - m10*m22
        val c2 = m10*m21 - m11*m20

        val det = m00*c0 + m01*c1 + m02*c2

        val mat = new Mat3f(
            c0,
            c1,
            c2,

            m02*m21 - m01*m22,
            m00*m22 - m02*m20,
            m01*m20 - m00*m21,

            m01*m12 - m02*m11,
            m02*m10 - m00*m12,
            m00*m11 - m01*m10
        )

        mat /= det
        mat
    }

    /**
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using Transform3f.inverse(scale, rotation, translation).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat4f) :Mat4f = {
        import m._

        val fA0 = m00*m11 - m01*m10
        val fA1 = m00*m12 - m02*m10
        val fA2 = m00*m13 - m03*m10
        val fA3 = m01*m12 - m02*m11
        val fA4 = m01*m13 - m03*m11
        val fA5 = m02*m13 - m03*m12
        val fB0 = m20*m31 - m21*m30
        val fB1 = m20*m32 - m22*m30
        val fB2 = m20*m33 - m23*m30
        val fB3 = m21*m32 - m22*m31
        val fB4 = m21*m33 - m23*m31
        val fB5 = m22*m33 - m23*m32

        val det = fA0*fB5 - fA1*fB4 + fA2*fB3 + fA3*fB2 - fA4*fB1 + fA5*fB0

        val mat = new Mat4f(
             m11*fB5 - m12*fB4 + m13*fB3,
            -m10*fB5 + m12*fB2 - m13*fB1,
             m10*fB4 - m11*fB2 + m13*fB0,
            -m10*fB3 + m11*fB1 - m12*fB0,

            -m01*fB5 + m02*fB4 - m03*fB3,
             m00*fB5 - m02*fB2 + m03*fB1,
            -m00*fB4 + m01*fB2 - m03*fB0,
             m00*fB3 - m01*fB1 + m02*fB0,

             m31*fA5 - m32*fA4 + m33*fA3,
            -m30*fA5 + m32*fA2 - m33*fA1,
             m30*fA4 - m31*fA2 + m33*fA0,
            -m30*fA3 + m31*fA1 - m32*fA0,

            -m21*fA5 + m22*fA4 - m23*fA3,
             m20*fA5 - m22*fA2 + m23*fA1,
            -m20*fA4 + m21*fA2 - m23*fA0,
             m20*fA3 - m21*fA1 + m22*fA0
        )

        mat /= det
        mat
    }

    // *** Extra Math functions ************************************************

    def isposinf(x: Float) :Boolean = isinf(x) && x > 0
    def isneginf(x: Float) :Boolean = isinf(x) && x < 0

    def isposinf(u: AnyVec2f) :Vec2b = new Vec2b(isposinf(u.x), isposinf(u.y))
    def isneginf(u: AnyVec2f) :Vec2b = new Vec2b(isneginf(u.x), isneginf(u.y))

    def isposinf(u: AnyVec3f) :Vec3b = {
        new Vec3b(isposinf(u.x), isposinf(u.y), isposinf(u.z))
    }
    def isneginf(u: AnyVec3f) :Vec3b = {
        new Vec3b(isneginf(u.x), isneginf(u.y), isneginf(u.z))
    }

    def isposinf(u: AnyVec4f) :Vec4b = {
        new Vec4b(isposinf(u.x), isposinf(u.y), isposinf(u.z), isposinf(u.w))
    }
    def isneginf(u: AnyVec4f) :Vec4b = {
        new Vec4b(isneginf(u.x), isneginf(u.y), isneginf(u.z), isneginf(u.w))
    }

    // Lerp
    def lerp(x: Float, y: Float, a: Float) = mix(x, y, a)
    def lerp(u: AnyVec2f, v: AnyVec2f, a: Float) = mix(u, v, a)
    def lerp(u: AnyVec3f, v: AnyVec3f, a: Float) = mix(u, v, a)
    def lerp(u: AnyVec4f, v: AnyVec4f, a: Float) = mix(u, v, a)

    def lerp(m: Mat2f, n: Mat2f, a: Float) :Mat2f = {
        val b = 1 - a

        new Mat2f(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11
        )
    }
    def lerp(m: Mat2x3f, n: Mat2x3f, a: Float) :Mat2x3f = {
        val b = 1 - a

        new Mat2x3f(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12
        )
    }
    def lerp(m: Mat2x4f, n: Mat2x4f, a: Float) :Mat2x4f = {
        val b = 1 - a

        new Mat2x4f(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12,
            b*m.m03 + a*n.m03, b*m.m13 + a*n.m13
        )
    }
    def lerp(m: Mat3x2f, n: Mat3x2f, a: Float) :Mat3x2f = {
        val b = 1 - a

        new Mat3x2f(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21
        )
    }
    def lerp(m: Mat3f, n: Mat3f, a: Float) :Mat3f = {
        val b = 1 - a

        new Mat3f(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22
        )
    }
    def lerp(m: Mat3x4f, n: Mat3x4f, a: Float) :Mat3x4f = {
        val b = 1 - a

        new Mat3x4f(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22,
            b*m.m03 + a*n.m03, b*m.m13 + a*n.m13, b*m.m23 + a*n.m23
        )
    }
    def lerp(m: Mat4x2f, n: Mat4x2f, a: Float) :Mat4x2f = {
      val b = 1 - a

      new Mat4x2f(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31
      )
    }
    def lerp(m: Mat4x3f, n: Mat4x3f, a: Float) :Mat4x3f = {
      val b = 1 - a

      new Mat4x3f(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
        b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32
      )
    }
    def lerp(m: Mat4f, n: Mat4f, a: Float) :Mat4f = {
      val b = 1 - a

      new Mat4f(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
        b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32,
        b*m.m03 +a*n.m03, b*m.m13 +a*n.m13, b*m.m23 +a*n.m23, b*m.m33 +a*n.m33
      )
    }

    def lengthSquare(x: Float) :Float = x*x
    def lengthSquare(u: AnyVec2f) :Float = u.x*u.x + u.y*u.y
    def lengthSquare(u: AnyVec3f) :Float = u.x*u.x + u.y*u.y + u.z*u.z
    def lengthSquare(u: AnyVec4f) :Float = u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w

    def hasErrors(x: Float) :Boolean = isinf(x) || isnan(x)
    def hasErrors(u: AnyVec2f) :Boolean = u.hasErrors
    def hasErrors(u: AnyVec3f) :Boolean = u.hasErrors
    def hasErrors(u: AnyVec4f) :Boolean = u.hasErrors
    def hasErrors(q: AnyQuat4f) :Boolean = q.hasErrors
    def hasErrors(m: AnyMat2f) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat2x3f) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat2x4f) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3x2f) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3f) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3x4f) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4x2f) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4x3f) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4f) :Boolean = m.hasErrors

    def approxEqual(x: Float, y: Float, absDelta: Float) :Boolean = {
        abs(x - y) < absDelta
    }
    def approxEqual(u: AnyVec2f, v: AnyVec2f, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta
    }
    def approxEqual(u: AnyVec3f, v: AnyVec3f, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta &&
        abs(v.z - u.z) < absDelta
    }
    def approxEqual(u: AnyVec4f, v: AnyVec4f, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta &&
        abs(v.z - u.z) < absDelta &&
        abs(v.w - u.w) < absDelta
    }
    def approxEqual(p: AnyQuat4f, q: AnyQuat4f, absDelta: Float) :Boolean = {
        abs(p.a - q.a) < absDelta &&
        abs(p.b - q.b) < absDelta &&
        abs(p.c - q.c) < absDelta &&
        abs(p.d - q.d) < absDelta
    }
    def approxEqual(m: AnyMat2f, n: AnyMat2f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta
        )
    }
    def approxEqual(m: AnyMat2x3f, n: AnyMat2x3f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta
        )
    }
    def approxEqual(m: AnyMat2x4f, n: AnyMat2x4f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&

            abs(n.m03 - m.m03) < absDelta &&
            abs(n.m13 - m.m13) < absDelta
        )
    }

    def approxEqual(m: AnyMat3x2f, n: AnyMat3x2f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta
        )
    }
    def approxEqual(m: AnyMat3f, n: AnyMat3f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&
            abs(n.m22 - m.m22) < absDelta
        )
    }
    def approxEqual(m: AnyMat3x4f, n: AnyMat3x4f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&
            abs(n.m22 - m.m22) < absDelta &&

            abs(n.m03 - m.m03) < absDelta &&
            abs(n.m13 - m.m13) < absDelta &&
            abs(n.m23 - m.m23) < absDelta
        )
    }
    def approxEqual(m: AnyMat4x2f, n: AnyMat4x2f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&
            abs(n.m30 - m.m30) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&
            abs(n.m31 - m.m31) < absDelta
        )
    }
    def approxEqual(m: AnyMat4x3f, n: AnyMat4x3f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&
            abs(n.m30 - m.m30) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&
            abs(n.m31 - m.m31) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&
            abs(n.m22 - m.m22) < absDelta &&
            abs(n.m32 - m.m32) < absDelta
        )
    }
    def approxEqual(m: AnyMat4f, n: AnyMat4f, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&
            abs(n.m30 - m.m30) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta &&
            abs(n.m31 - m.m31) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta &&
            abs(n.m22 - m.m22) < absDelta &&
            abs(n.m32 - m.m32) < absDelta &&

            abs(n.m03 - m.m03) < absDelta &&
            abs(n.m13 - m.m13) < absDelta &&
            abs(n.m23 - m.m23) < absDelta &&
            abs(n.m33 - m.m33) < absDelta
        )
    }

    /**
     * This method is equivalent to casting the matrix as 3x3, inverting it
     * and then casting the result back to 2x3.<br/>
     *
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using Transform2f.inverse(scale, rotation, translation).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat2x3f) :Mat2x3f = {
        import m._

        val det = m00*m11 - m01*m10

        val mat = new Mat2x3f(
            m11, -m10,
            -m01, m00,
            m01*m12 - m02*m11, m02*m10 - m00*m12
        )

        mat /= det
        mat
    }

    /**
     * This method is equivalent to casting the matrix as 4x4, inverting it
     * and then casting the result back to 3x4.<br/>
     *
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using Transform3f.inverse(scale, rotation, translation).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat3x4f) :Mat3x4f = {
        import m._

        val fA0 = m00*m11 - m01*m10
        val fA1 = m00*m12 - m02*m10
        val fA2 = m00*m13 - m03*m10
        val fA3 = m01*m12 - m02*m11
        val fA4 = m01*m13 - m03*m11
        val fA5 = m02*m13 - m03*m12

        val det = fA0*m22 - fA1*m21 + fA3*m20

        val mat = new Mat3x4f(
             m11*m22 - m12*m21,
            -m10*m22 + m12*m20,
             m10*m21 - m11*m20,

            -m01*m22 + m02*m21,
             m00*m22 - m02*m20,
            -m00*m21 + m01*m20,

             fA3,
            -fA1,
             fA0,

            -m21*fA5 + m22*fA4 - m23*fA3,
             m20*fA5 - m22*fA2 + m23*fA1,
            -m20*fA4 + m21*fA2 - m23*fA0
        )

        mat /= det
        mat
    }

    // Quaternion
    def normSquare(q: AnyQuat4f) :Float = q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d
    def norm(q: AnyQuat4f) :Float = {
        sqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
    }
    def conjugate(q: AnyQuat4f) :Quat4f = new Quat4f(q.a, -q.b, -q.c, -q.d)

    def normalize(q: AnyQuat4f) :Quat4f = {
        q*inversesqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
    }

    /**
     * This method is here for completness. Normally you should work with
     * unit quaternions (<code>norm(q) == 1</code>), and in this case
     * <code>inverse(q) == conjugate(q)</code>.
     */
    def inverse(q: AnyQuat4f) :Quat4f = conjugate(q)/normSquare(q)

    def slerp(p: AnyQuat4f, q: AnyQuat4f, a: Float) :Quat4f = {
        if (approxEqual(p, q, 1e-5f)) return Quat4f(q)

        var cosTheta = p.a*q.a + p.b*q.b + p.c*q.c+ p.d*q.d
        var negate = false
        if (cosTheta < 0) {
            cosTheta = -cosTheta
            negate = true
        }

        var t = a
        var s = 1 - t

        if (cosTheta < 0.95f) {
            val theta = acos(cosTheta)
            val invSinTheta = 1/sin(theta)

            t = sin(t*theta)*invSinTheta
            s = sin(s*theta)*invSinTheta
            if (negate) t = -t
        }

        new Quat4f(
            s*p.a + t*q.a,
            s*p.b + t*q.b,
            s*p.c + t*q.c,
            s*p.d + t*q.d
        )
    }

    /**
     * The result is undefined for quaternions with non-unit norm.
     */
    def rotate(u: AnyVec3f, q: Quat4f) = {
        import q._

        val t1 = a*b
        val t2 = a*c
        val t3 = a*d
        val t4 = -b*b
        val t5 = b*c
        val t6 = b*d
        val t7 = -c*c
        val t8 = c*d
        val t9 = -d*d

        new Vec3f(
            2*((t7 + t9)*u.x + (t5 - t3)*u.y + (t2 + t6)*u.z) + u.x,
            2*((t3 + t5)*u.x + (t4 + t9)*u.y + (t8 - t1)*u.z) + u.y,
            2*((t6 - t2)*u.x + (t1 + t8)*u.y + (t4 + t7)*u.z) + u.z
        )
    }

    // Rotation
    /**
     * This method creates a 2d transformation matrix that rotates a vector
     * counterclockwise by the specified angle.
     */
    def rotationMat(angle: Float) :Mat2f = {
        val cosA = cos(angle)
        val sinA = sin(angle)

        new Mat2f(
             cosA, sinA,
            -sinA, cosA
        )
    }

    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation.
     */
    def rotationAngle(m: Mat2f) :Float = {
        acos((m.m00 + m.m11)*0.5f)
    }

    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation.
     */
    def quaternion(m: Mat3f) :Quat4f = {
        import m._

        val result = new Quat4f(0, 0, 0, 0)
        val trace = m00 + m11 + m22

        if (trace > 0) {
            val t = trace + 1
            val s = inversesqrt(t)*0.5f
            result.a = s*t
            result.d = (m10 - m01)*s
            result.c = (m02 - m20)*s
            result.b = (m21 - m12)*s
        }
        else if (m00 > m11 && m00 > m22) {
            val t = m00 - m11 - m22 + 1
            val s = inversesqrt(t)*0.5f
            result.b = s*t
            result.c = (m10 + m01)*s
            result.d = (m02 + m20)*s
            result.a = (m21 - m12)*s
        }
        else if (m11 > m22) {
            val t = -m00 + m11 - m22 + 1
            val s = inversesqrt(t)*0.5f
            result.c = s*t
            result.b = (m10 + m01)*s
            result.a = (m02 - m20)*s
            result.d = (m21 + m12)*s
        }
        else {
            val t = -m00 - m11 + m22 + 1
            val s = inversesqrt(t)*0.5f
            result.d = s*t
            result.a = (m10 - m01)*s
            result.b = (m02 + m20)*s
            result.c = (m21 + m12)*s
        }

        result
    }
    /**
     * The result is undefined for axis with non-unit length.
     */
    def quaternion(angle: Float, axis: AnyVec3f) :Quat4f = {
        val s = sin(angle/2)
        new Quat4f(cos(angle/2), s*axis.x, s*axis.y, s*axis.z)
    }

    /**
     * The result is undefined for quaternions with non-unit norm.
     */
    def rotationMat(q: AnyQuat4f) :Mat3f = {
        import q._

        val tb = 2*b*b
        val tc = 1 - 2*c*c
        val td = 2*d*d
        val bc = 2*b*c
        val da = 2*d*a
        val bd = 2*b*d
        val ca = 2*c*a
        val cd = 2*c*d
        val ba = 2*b*a

        new Mat3f(
            tc - td, bc + da, bd - ca,
            bc - da, 1 - tb - td, cd + ba,
            bd + ca, cd - ba, tc - tb
        )
    }
    /**
     * The result is undefined for axis with non-unit length.
     */
    def rotationMat(angle: Float, axis: AnyVec3f) :Mat3f = {
        import axis._

        val sinA = sin(angle)
        val cosA = cos(angle)
        val c = 1 - cosA
        val sx = sinA*x
        val sy = sinA*y
        val sz = sinA*z
        val temp = c*x
        val cxy = temp*y
        val cxz = temp*z
        val cyz = c*y*z

        new Mat3f(
            cosA + c*x*x, cxy + sz, cxz - sy,
            cxy - sz, cosA + c*y*y, cyz + sx,
            cxz + sy, cyz - sx, cosA + c*z*z
        )
    }

    /**
     * The result is undefined for quaternions with non-unit norm.
     * If quaternion represents 0 degree rotation, then rotation
     * axis is not defined, in this case the UnitX axis is chosen.
     */
    def angleAxis(q: AnyQuat4f, axisResult: Vec3f) :Float = {
        import q._

        if (approxEqual(abs(a), 1, 1e-6f)) {
            axisResult.set(1, 0, 0)
            return 0
        }

        val t = inversesqrt(1 - a*a)
        axisResult.x = b*t
        axisResult.y = c*t
        axisResult.z = d*t

        2*acos(a)
    }
    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation. If matrix represents 0 degree rotation,
     * then rotation axis is not defined, in this case the UnitX axis is chosen.
     */
    def angleAxis(m: Mat3f, axisResult: Vec3f) :Float = {
        import m._

        val cosAngle = (m00 + m11 + m22 - 1)*0.5f

        if (approxEqual(cosAngle, 1, 1e-5f)) {
            axisResult.set(1, 0, 0)
            return 0
        }
        else if (approxEqual(cosAngle, -1, 1e-5f)) {
            if (m00 > m11 && m00 > m22) {
                val r = sqrt((m00 + 1)*0.5f)
                val t = 1/(4*r)
                axisResult.x = r
                axisResult.y = (m01 + m10)*t
                axisResult.z = (m02 + m20)*t
            }
            else if (m11 > m22) {
                val r = sqrt((m11 + 1)*0.5f)
                val t = 1/(4*r)
                axisResult.y = r
                axisResult.x = (m01 + m10)*t
                axisResult.z = (m12 + m21)*t
            }
            else {
                val r = sqrt((m22 + 1)*0.5f)
                val t = 1/(4*r)
                axisResult.z = r
                axisResult.x = (m02 + m20)*t
                axisResult.y = (m12 + m21)*t
            }
            return Pi
        }

        val t0 = (m21 - m12)
        val t1 = (m02 - m20)
        val t2 = (m10 - m01)
        val t = inversesqrt(t0*t0 + t1*t1 + t2*t2)
        axisResult.x = (m21 - m12)*t
        axisResult.y = (m02 - m20)*t
        axisResult.z = (m10 - m01)*t

        acos(cosAngle)
    }

    def lookAt(direction: AnyVec3f, up: AnyVec3f) :Mat3f = {
        val zaxis = normalize(direction)
        val xaxis = normalize(cross(up, zaxis))
        val yaxis = cross(zaxis, xaxis)
        new Mat3f(
            xaxis.x, xaxis.y, xaxis.z,
            yaxis.x, yaxis.y, yaxis.z,
            zaxis.x, zaxis.y, zaxis.z
        )
    }

    // Projection
    def perspective(fieldOfView: Float, aspectRatio: Float,
                    near: Float, far: Float)
    :Mat4f =
    {
        val focus = 1/tan(fieldOfView * 0.5f)
        val n_f = 1/(near - far)

        new Mat4f(
            focus/aspectRatio, 0, 0, 0,
            0, focus, 0, 0,
            0, 0, (near + far)*n_f, -1,
            0, 0, 2*near*far*n_f, 0
        )
    }

    def ortho(left: Float, right: Float,
              bottom: Float, top: Float,
              near: Float, far: Float)
    :Mat4f =
    {
        val r_l = 1/(right - left);
        val t_b = 1/(top - bottom);
        val f_n = 1/(far - near);

        new Mat4f(
            2*r_l, 0, 0, 0,
            0, 2*t_b, 0, 0,
            0, 0, -2*f_n, 0,
            -(right + left)*r_l, -(top + bottom)*t_b, -(far + near)*f_n, 1
        )
    }

    // Transform
    def transform(scale: AnyVec3f,
                  rotation: AnyMat3f,
                  translation: AnyVec3f)
    :Transform3f =
    {
        import scale.{x => sx, y => sy, z => sz}
        import rotation._
        import translation.{x => tx, y => ty, z => tz}

        new TransformMat3x4f(new Mat3x4f(
            m00*sx, m10*sx, m20*sx,
            m01*sy, m11*sy, m21*sy,
            m02*sz, m12*sz, m22*sz,
            tx, ty, tz
        ))
    }

    /**
     * @param rotation Must be an orthogonal matrix (matrix that represents
     * an unscaled rotation) to achieve the desired result.
     */
    def inverseTransform(scale: AnyVec3f,
                         rotation: AnyMat3f,
                         translation: AnyVec3f)
    :Transform3f =
    {
        import translation.{x => tx, y => ty, z => tz}

        val sx = 1/scale.x
        val sy = 1/scale.y
        val sz = 1/scale.z

        val m00 = rotation.m00*sx
        val m10 = rotation.m01*sy
        val m20 = rotation.m02*sz
        val m01 = rotation.m10*sx
        val m11 = rotation.m11*sy
        val m21 = rotation.m12*sz
        val m02 = rotation.m20*sx
        val m12 = rotation.m21*sy
        val m22 = rotation.m22*sz

        new TransformMat3x4f(new Mat3x4f(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            -m00*tx - m01*ty - m02*tz,
            -m10*tx - m11*ty - m12*tz,
            -m20*tx - m21*ty - m22*tz
        ))
    }
}
