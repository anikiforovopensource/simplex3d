/*
 * Simplex3D, Math module
 * Copyright (C) 2009 Simplex3D team
 *
 * This file is part of Simplex3d.
 *
 * Simplex3d is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3d is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math.floatvec

import simplex3d.math.BaseMath._
import simplex3d.math.intvec._
import java.lang.{Math => JMath, StrictMath => SMath}
import toxi.math.noise.SimplexNoise


/**
 * @author Aleksey Nikiforov (lex)
 */
object FloatMath {
    // Implicits
    implicit def fmfloatToExtFloat(s: Float) = new ExtendedFloat(s)
    implicit def fmintToExtInt(s: Int) = new ExtendedInt(s)
    implicit def vec2iToVec2(u: AnyVec2i) :Vec2 = Vec2(u)
    implicit def vec3iToVec3(u: AnyVec3i) :Vec3 = Vec3(u)
    implicit def vec4iToVec4(u: AnyVec4i) :Vec4 = Vec4(u)

    // Random
    def nextVec2() :Vec2 = Vec2(nextFloat, nextFloat)
    def nextVec3() :Vec3 = Vec3(nextFloat, nextFloat, nextFloat)
    def nextVec4() :Vec4 = Vec4(nextFloat, nextFloat, nextFloat, nextFloat)

    // Constants
    val FloatEpsilon: Float = 1.19209E-7f;
    val Pi: Float = float(Math.Pi)
    val E: Float = float(Math.E)

    private val DegToRad = Pi / 180
    private val RadToDeg = 180 / Pi
    private val InvLog2 = 1/SMath.log(2)

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

    def asinh(x: Float) :Float = float(SMath.log(x + SMath.sqrt(x*x + 1)))
    def acosh(x: Float) :Float = float(SMath.log(x + SMath.sqrt(x*x - 1)))
    def atanh(x: Float) :Float = float(0.5*SMath.log((1 + x)/(1 - x)))

    def pow(x: Float, y: Float) :Float = float(SMath.pow(x, y))
    def exp(x: Float) :Float = float(SMath.exp(x))
    def log(x: Float) :Float = float(SMath.log(x))

    def exp2(x: Float) :Float = float(SMath.pow(2, x))
    def log2(x: Float) :Float = float(SMath.log(x)*InvLog2)

    def sqrt(s: Float) :Float = float(SMath.sqrt(s))
    def inversesqrt(s: Float) :Float = float(1/SMath.sqrt(s))

    def abs(x: Float) :Float = { if (x < 0) -x else x }
    def sign(x: Float) :Float = if (x == 0) 0 else if (x > 0) 1 else -1
    def floor(x: Float) :Float = {
        if (x >= 0) int(x) else int(x) - 1
    }
    def trunc(x: Float) :Float = int(x)
    def round(x: Float) :Float = {
        if (x >= 0) (int) (x + 0.5f)
        else (int) (x - 0.5f)
    }
    def roundEven(x: Float) :Float = float(SMath.rint(x))
    def ceil(x: Float) :Float = {
        if (x == float(int(x))) x else int(x) + 1
    }
    def fract(x: Float) :Float = x - floor(x)
    def mod(x: Float, y: Float) :Float = x - y*floor(x/y)
    //def modf(x: Float, i: Float) :Float = 0 //not supported: lack of pointers

    def min(x: Float, y: Float) :Float = if (y < x) y else x
    def max(x: Float, y: Float) :Float = if (y > x) y else x
    def clamp(x: Float, minVal: Float, maxVal: Float) :Float = {
        if (x < minVal) minVal
        else if (x > maxVal) maxVal
        else x
    }

    def mix(x: Float, y: Float, a: Float) :Float = x*(1 - a) + y*a
    def step(edge: Float, x: Float) :Float = if (x < edge) 0 else 1
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
    def faceforward(n: Float, i: Float, nref: Float) :Float = {
        if (i*nref < 0) n else -n
    }

    def reflect(i: Float, n: Float) :Float = {
        i - 2*(n*i)*n
    }
    def refract(i: Float, n: Float, eta: Float) :Float = {
        val ni = n*i
        val k = 1 - eta*eta*(1 - ni*ni)
        if (k < 0) 0 else eta*i - (eta*ni + sqrt(k))*n
    }

    def noise1(x: Float) :Float = float(SimplexNoise.noise(x, 0))
    def noise2(x: Float) :Vec2 = {
        Vec2(
            float(SimplexNoise.noise(x, 0)),
            float(SimplexNoise.noise(x, of1))
        )
    }
    def noise3(x: Float) :Vec3 = {
        Vec3(
            float(SimplexNoise.noise(x, 0)),
            float(SimplexNoise.noise(x, of1)),
            float(SimplexNoise.noise(x, of2))
        )
    }
    def noise4(x: Float) :Vec4 = {
        Vec4(
            float(SimplexNoise.noise(x, 0)),
            float(SimplexNoise.noise(x, of1)),
            float(SimplexNoise.noise(x, of2)),
            float(SimplexNoise.noise(x, of3))
        )
    }

    // Vec2 functions
    def radians(u: AnyVec2) :Vec2 = Vec2(radians(u.x), radians(u.y))
    def degrees(u: AnyVec2) :Vec2 = Vec2(degrees(u.x), degrees(u.y))

    def sin(u: AnyVec2) :Vec2 = Vec2(sin(u.x), sin(u.y))
    def cos(u: AnyVec2) :Vec2 = Vec2(cos(u.x), cos(u.y))
    def tan(u: AnyVec2) :Vec2 = Vec2(tan(u.x), tan(u.y))

    def asin(u: AnyVec2) :Vec2 = Vec2(asin(u.x), asin(u.y))
    def acos(u: AnyVec2) :Vec2 = Vec2(acos(u.x), acos(u.y))
    def atan(uy: AnyVec2, ux: AnyVec2) :Vec2 = {
        Vec2(atan(uy.x, ux.x), atan(uy.y, ux.y))
    }
    def atan(u: AnyVec2) :Vec2 = Vec2(atan(u.x), atan(u.y))

    def sinh(u: AnyVec2) :Vec2 = Vec2(sinh(u.x), sinh(u.y))
    def cosh(u: AnyVec2) :Vec2 = Vec2(cosh(u.x), cosh(u.y))
    def tanh(u: AnyVec2) :Vec2 = Vec2(tanh(u.x), tanh(u.y))

    def asinh(u: AnyVec2) :Vec2 = Vec2(asinh(u.x), asinh(u.y))
    def acosh(u: AnyVec2) :Vec2 = Vec2(acosh(u.x), acosh(u.y))
    def atanh(u: AnyVec2) :Vec2 = Vec2(atanh(u.x), atanh(u.y))

    def pow(u: AnyVec2, v: AnyVec2) :Vec2 = {
        Vec2(pow(u.x, v.x), pow(u.y, v.y))
    }
    def exp(u: AnyVec2) :Vec2 = Vec2(exp(u.x), exp(u.y))
    def log(u: AnyVec2) :Vec2 = Vec2(log(u.x), log(u.y))

    def exp2(u: AnyVec2) :Vec2 = Vec2(exp2(u.x), exp2(u.y))
    def log2(u: AnyVec2) :Vec2 = Vec2(log2(u.x), log2(u.y))

    def sqrt(u: AnyVec2) :Vec2 = Vec2(sqrt(u.x), sqrt(u.y))
    def inversesqrt(u: AnyVec2) :Vec2 = Vec2(inversesqrt(u.x), inversesqrt(u.y))

    def abs(u: AnyVec2) :Vec2 = Vec2(abs(u.x), abs(u.y))
    def sign(u: AnyVec2) :Vec2 = Vec2(sign(u.x), sign(u.y))
    def floor(u: AnyVec2) :Vec2 = Vec2(floor(u.x), floor(u.y))
    def trunc(u: AnyVec2) :Vec2 = Vec2(trunc(u.x), trunc(u.y))
    def round(u: AnyVec2) :Vec2 = Vec2(round(u.x), round(u.y))
    def roundEven(u: AnyVec2) :Vec2 = Vec2(roundEven(u.x), roundEven(u.y))
    def ceil(u: AnyVec2) :Vec2 = Vec2(ceil(u.x), ceil(u.y))
    def fract(u: AnyVec2) :Vec2 = Vec2(fract(u.x), fract(u.y))
    def mod(u: AnyVec2, s: Float) :Vec2 = Vec2(mod(u.x, s), mod(u.y, s))
    def mod(u: AnyVec2, v: AnyVec2) :Vec2 = Vec2(mod(u.x, v.x), mod(u.y, v.y))
    def modf(u: AnyVec2, i: Vec2) :Vec2 = {
        val s = sign(u)
        val a = abs(u)
        i := s*floor(a)
        s*fract(a)
    }

    def min(u: AnyVec2, s: Float) :Vec2 = Vec2(min(u.x, s), min(u.y, s))
    def min(u: AnyVec2, v: AnyVec2) :Vec2 = Vec2(min(u.x, v.x), min(u.y, v.y))
    def max(u: AnyVec2, s: Float) :Vec2 = Vec2(max(u.x, s), max(u.y, s))
    def max(u: AnyVec2, v: AnyVec2) :Vec2 = Vec2(max(u.x, v.x), max(u.y, v.y))
    def clamp(u: AnyVec2, minVal: Float, maxVal: Float) :Vec2 = {
        Vec2(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
    }
    def clamp(u: AnyVec2, minVal: AnyVec2, maxVal: AnyVec2) :Vec2 = {
        Vec2(clamp(u.x, minVal.x, maxVal.x), clamp(u.y, minVal.y, maxVal.y))
    }

    def mix(u: AnyVec2, v: AnyVec2, a: Float) :Vec2 = {
        val b = 1 - a
        Vec2(b*u.x + a*v.x, b*u.y + a*v.y)
    }
    def mix(u: AnyVec2, v: AnyVec2, a: AnyVec2) :Vec2 = {
        Vec2(mix(u.x, v.x, a.x), mix(u.y, v.y, a.y))
    }
    def mix(u: AnyVec2, v: AnyVec2, a: AnyVec2b) :Vec2 = {
        Vec2(
            if (a.x) v.x else u.x,
            if (a.y) v.y else u.y
        )
    }

    def step(edge: Float, u: AnyVec2) :Vec2 = {
        Vec2(step(edge, u.x), step(edge, u.y))
    }
    def step(edge: AnyVec2, u: AnyVec2) :Vec2 = {
        Vec2(step(edge.x, u.x), step(edge.y, u.y))
    }
    def smoothstep(edge0: Float, edge1: Float, u: AnyVec2) :Vec2 = {
        Vec2(
            smoothstep(edge0, edge1, u.x),
            smoothstep(edge0, edge1, u.y)
        )
    }
    def smoothstep(edge0: AnyVec2, edge1: AnyVec2, u: AnyVec2) :Vec2 = {
        Vec2(
            smoothstep(edge0.x, edge1.x, u.x),
            smoothstep(edge0.y, edge1.y, u.y)
        )
    }

    def isnan(u: AnyVec2) :Vec2b = Vec2b(isnan(u.x), isnan(u.y))
    def isinf(u: AnyVec2) :Vec2b = Vec2b(isinf(u.x), isinf(u.y))
    
    def length(u: AnyVec2) :Float = sqrt(u.x*u.x + u.y*u.y)
    def distance(u: AnyVec2, v: AnyVec2) :Float = length(u - v)
    def dot(u: AnyVec2, v: AnyVec2) :Float = u.x * v.x + u.y * v.y
    def normalize(u: AnyVec2) :Vec2 = u/length(u)

    def faceforward(n: AnyVec2, i: AnyVec2, nref: AnyVec2) :Vec2 = {
        if (dot(nref, i) < 0) Vec2(n) else -n
    }

    def reflect(i: AnyVec2, n: AnyVec2) :Vec2 = {
        i - n*2*dot(n, i)
    }
    def refract(i: AnyVec2, n: AnyVec2, eta: Float) :Vec2 = {
        val dotni = dot(n, i)
        val k = 1 - eta*eta*(1 - dotni*dotni)
        if (k < 0) Vec2(0) else i*eta - n*(eta*dotni + sqrt(k))
    }

    def lessThan(u: AnyVec2, v: AnyVec2) :Vec2b = {
        Vec2b(
            u.x < v.x,
            u.y < v.y
        )
    }
    def lessThanEqual(u: AnyVec2, v: AnyVec2) :Vec2b = {
        Vec2b(
            u.x <= v.x,
            u.y <= v.y
        )
    }
    def greaterThan(u: AnyVec2, v: AnyVec2) :Vec2b = {
        Vec2b(
            u.x > v.x,
            u.y > v.y
        )
    }
    def greaterThanEqual(u: AnyVec2, v: AnyVec2) :Vec2b = {
        Vec2b(
            u.x >= v.x,
            u.y >= v.y
        )
    }
    def equal(u: AnyVec2, v: AnyVec2) :Vec2b = {
        Vec2b(
            u.x == v.x,
            u.y == v.y
        )
    }
    def notEqual(u: AnyVec2, v: AnyVec2) :Vec2b = {
        Vec2b(
            u.x != v.x,
            u.y != v.y
        )
    }

    def noise1(u: AnyVec2) :Float = {
        float(SimplexNoise.noise(u.x, u.y))
    }
    def noise2(u: AnyVec2) :Vec2 = {
        Vec2(
            float(SimplexNoise.noise(u.x, u.y)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1))
        )
    }
    def noise3(u: AnyVec2) :Vec3 = {
        Vec3(
            float(SimplexNoise.noise(u.x, u.y)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1)),
            float(SimplexNoise.noise(u.x + of2, u.y + of2))
        )
    }
    def noise4(u: AnyVec2) :Vec4 = {
        Vec4(
            float(SimplexNoise.noise(u.x, u.y)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1)),
            float(SimplexNoise.noise(u.x + of2, u.y + of2)),
            float(SimplexNoise.noise(u.x + of3, u.y + of3))
        )
    }


    // Vec3 functions
    def radians(u: AnyVec3) :Vec3 = {
        Vec3(radians(u.x), radians(u.y), radians(u.z))
    }
    def degrees(u: AnyVec3) :Vec3 = {
        Vec3(degrees(u.x), degrees(u.y), degrees(u.z))
    }

    def sin(u: AnyVec3) :Vec3 = Vec3(sin(u.x), sin(u.y), sin(u.z))
    def cos(u: AnyVec3) :Vec3 = Vec3(cos(u.x), cos(u.y), cos(u.z))
    def tan(u: AnyVec3) :Vec3 = Vec3(tan(u.x), tan(u.y), tan(u.z))

    def asin(u: AnyVec3) :Vec3 = Vec3(asin(u.x), asin(u.y), asin(u.z))
    def acos(u: AnyVec3) :Vec3 = Vec3(acos(u.x), acos(u.y), acos(u.z))
    def atan(uy: AnyVec3, ux: AnyVec3) :Vec3 = {
        Vec3(atan(uy.x, ux.x), atan(uy.y, ux.y), atan(uy.z, ux.z))
    }
    def atan(u: AnyVec3) :Vec3 = Vec3(atan(u.x), atan(u.y), atan(u.z))

    def sinh(u: AnyVec3) :Vec3 = Vec3(sinh(u.x), sinh(u.y), sinh(u.z))
    def cosh(u: AnyVec3) :Vec3 = Vec3(cosh(u.x), cosh(u.y), cosh(u.z))
    def tanh(u: AnyVec3) :Vec3 = Vec3(tanh(u.x), tanh(u.y), tanh(u.z))

    def asinh(u: AnyVec3) :Vec3 = Vec3(asinh(u.x), asinh(u.y), asinh(u.z))
    def acosh(u: AnyVec3) :Vec3 = Vec3(acosh(u.x), acosh(u.y), acosh(u.z))
    def atanh(u: AnyVec3) :Vec3 = Vec3(atanh(u.x), atanh(u.y), atanh(u.z))

    def pow(u: AnyVec3, v: AnyVec3) :Vec3 = {
        Vec3(pow(u.x, v.x), pow(u.y, v.y), pow(u.z, v.z))
    }
    def exp(u: AnyVec3) :Vec3 = Vec3(exp(u.x), exp(u.y), exp(u.z))
    def log(u: AnyVec3) :Vec3 = Vec3(log(u.x), log(u.y), log(u.z))

    def exp2(u: AnyVec3) :Vec3 = Vec3(exp2(u.x), exp2(u.y), exp2(u.z))
    def log2(u: AnyVec3) :Vec3 = Vec3(log2(u.x), log2(u.y), log2(u.z))

    def sqrt(u: AnyVec3) :Vec3 = Vec3(sqrt(u.x), sqrt(u.y), sqrt(u.z))
    def inversesqrt(u: AnyVec3) :Vec3 = {
        Vec3(inversesqrt(u.x), inversesqrt(u.y), inversesqrt(u.z))
    }

    def abs(u: AnyVec3) :Vec3 = Vec3(abs(u.x), abs(u.y), abs(u.z))
    def sign(u: AnyVec3) :Vec3 = Vec3(sign(u.x), sign(u.y), sign(u.z))
    def floor(u: AnyVec3) :Vec3 = Vec3(floor(u.x), floor(u.y), floor(u.z))
    def trunc(u: AnyVec3) :Vec3 = Vec3(trunc(u.x), trunc(u.y), trunc(u.z))
    def round(u: AnyVec3) :Vec3 = Vec3(round(u.x), round(u.y), round(u.z))
    def roundEven(u: AnyVec3) :Vec3 = {
        Vec3(roundEven(u.x), roundEven(u.y), roundEven(u.z))
    }
    def ceil(u: AnyVec3) :Vec3 = Vec3(ceil(u.x), ceil(u.y), ceil(u.z))
    def fract(u: AnyVec3) :Vec3 = Vec3(fract(u.x), fract(u.y), fract(u.z))
    def mod(u: AnyVec3, s: Float) :Vec3 = {
        Vec3(mod(u.x, s), mod(u.y, s), mod(u.z, s))
    }
    def mod(u: AnyVec3, v: AnyVec3) :Vec3 = {
        Vec3(mod(u.x, v.x), mod(u.y, v.y), mod(u.z, v.z))
    }
    def modf(u: AnyVec3, i: Vec3) :Vec3 = {
        val s = sign(u)
        val a = abs(u)
        i := s*floor(a)
        s*fract(a)
    }

    def min(u: AnyVec3, s: Float) :Vec3 = {
        Vec3(min(u.x, s), min(u.y, s), min(u.z, s))
    }
    def min(u: AnyVec3, v: AnyVec3) :Vec3 = {
        Vec3(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
    }
    def max(u: AnyVec3, s: Float) :Vec3 = {
        Vec3(max(u.x, s), max(u.y, s), max(u.z, s))
    }
    def max(u: AnyVec3, v: AnyVec3) :Vec3 = {
        Vec3(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
    }
    def clamp(u: AnyVec3, minVal: Float, maxVal: Float) :Vec3 = {
        Vec3(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec3, minVal: AnyVec3, maxVal: AnyVec3) :Vec3 = {
        Vec3(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z)
        )
    }

    def mix(u: AnyVec3, v: AnyVec3, a: Float) :Vec3 = {
        val b = 1 - a
        Vec3(b*u.x + a*v.x, b*u.y + a*v.y, b*u.z + a*v.z)
    }
    def mix(u: AnyVec3, v: AnyVec3, a: AnyVec3) :Vec3 = {
        Vec3(mix(u.x, v.x, a.x), mix(u.y, v.y, a.y), mix(u.z, v.z, a.z))
    }
    def mix(u: AnyVec3, v: AnyVec3, a: AnyVec3b) :Vec3 = {
        Vec3(
            if (a.x) v.x else u.x,
            if (a.y) v.y else u.y,
            if (a.z) v.z else u.z
        )
    }

    def step(edge: Float, u: AnyVec3) :Vec3 = {
        Vec3(step(edge, u.x), step(edge, u.y), step(edge, u.z))
    }
    def step(edge: AnyVec3, u: AnyVec3) :Vec3 = {
        Vec3(step(edge.x, u.x), step(edge.y, u.y), step(edge.z, u.z))
    }
    def smoothstep(edge0: Float, edge1: Float, u: AnyVec3) :Vec3 = {
        Vec3(
            smoothstep(edge0, edge1, u.x),
            smoothstep(edge0, edge1, u.y),
            smoothstep(edge0, edge1, u.z)
        )
    }
    def smoothstep(edge0: AnyVec3, edge1: AnyVec3, u: AnyVec3) :Vec3 = {
        Vec3(
            smoothstep(edge0.x, edge1.x, u.x),
            smoothstep(edge0.y, edge1.y, u.y),
            smoothstep(edge0.z, edge1.z, u.z)
        )
    }

    def isnan(u: AnyVec3) :Vec3b = Vec3b(isnan(u.x), isnan(u.y), isnan(u.z))
    def isinf(u: AnyVec3) :Vec3b = Vec3b(isinf(u.x), isinf(u.y), isinf(u.z))

    def length(u: AnyVec3) :Float = sqrt(u.x*u.x + u.y*u.y + u.z*u.z)
    def distance(u: AnyVec3, v: AnyVec3) :Float = length(u - v)
    def dot(u: AnyVec3, v: AnyVec3) :Float = u.x*v.x + u.y*v.y + u.z*v.z
    def cross(u: AnyVec3, v: AnyVec3) :Vec3 = {
        Vec3(
            u.y*v.z-v.y*u.z,
            u.z*v.x-v.z*u.x,
            u.x*v.y-v.x*u.y
        )
    }
    def normalize(u: AnyVec3) :Vec3 = u/length(u)

    def faceforward(n: AnyVec3, i: AnyVec3, nref: AnyVec3) :Vec3 = {
        if (dot(nref, i) < 0) Vec3(n) else -n
    }

    def reflect(i: AnyVec3, n: AnyVec3) :Vec3 = {
        i - n*2*dot(n, i)
    }
    def refract(i: AnyVec3, n: AnyVec3, eta: Float) :Vec3 = {
        val dotni = dot(n, i)
        val k = 1 - eta*eta*(1 - dotni*dotni)
        if (k < 0) Vec3(0) else i*eta - n*(eta*dotni + sqrt(k))
    }

    def lessThan(u: AnyVec3, v: AnyVec3) :Vec3b = {
        Vec3b(
            u.x < v.x,
            u.y < v.y,
            u.z < v.z
        )
    }
    def lessThanEqual(u: AnyVec3, v: AnyVec3) :Vec3b = {
        Vec3b(
            u.x <= v.x,
            u.y <= v.y,
            u.z <= v.z
        )
    }
    def greaterThan(u: AnyVec3, v: AnyVec3) :Vec3b = {
        Vec3b(
            u.x > v.x,
            u.y > v.y,
            u.z > v.z
        )
    }
    def greaterThanEqual(u: AnyVec3, v: AnyVec3) :Vec3b = {
        Vec3b(
            u.x >= v.x,
            u.y >= v.y,
            u.z >= v.z
        )
    }
    def equal(u: AnyVec3, v: AnyVec3) :Vec3b = {
        Vec3b(
            u.x == v.x,
            u.y == v.y,
            u.z == v.z
        )
    }
    def notEqual(u: AnyVec3, v: AnyVec3) :Vec3b = {
        Vec3b(
            u.x != v.x,
            u.y != v.y,
            u.z != v.z
        )
    }

    def noise1(u: AnyVec3) :Float = {
        float(SimplexNoise.noise(u.x, u.y, u.z))
    }
    def noise2(u: AnyVec3) :Vec2 = {
        Vec2(
            float(SimplexNoise.noise(u.x, u.y, u.z)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1))
        )
    }
    def noise3(u: AnyVec3) :Vec3 = {
        Vec3(
            float(SimplexNoise.noise(u.x, u.y, u.z)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1)),
            float(SimplexNoise.noise(u.x + of2, u.y + of2, u.z + of2))
        )
    }
    def noise4(u: AnyVec3) :Vec4 = {
        Vec4(
            float(SimplexNoise.noise(u.x, u.y, u.z)),
            float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1)),
            float(SimplexNoise.noise(u.x + of2, u.y + of2, u.z + of2)),
            float(SimplexNoise.noise(u.x + of3, u.y + of3, u.z + of3))
        )
    }

    // Vec4 functions
    def radians(u: AnyVec4) :Vec4 = {
        Vec4(radians(u.x), radians(u.y), radians(u.z), radians(u.w))
    }
    def degrees(u: AnyVec4) :Vec4 = {
        Vec4(degrees(u.x), degrees(u.y), degrees(u.z), degrees(u.w))
    }

    def sin(u: AnyVec4) :Vec4 = Vec4(sin(u.x), sin(u.y), sin(u.z), sin(u.w))
    def cos(u: AnyVec4) :Vec4 = Vec4(cos(u.x), cos(u.y), cos(u.z), cos(u.w))
    def tan(u: AnyVec4) :Vec4 = Vec4(tan(u.x), tan(u.y), tan(u.z), tan(u.w))

    def asin(u: AnyVec4) :Vec4 = {
        Vec4(asin(u.x), asin(u.y), asin(u.z), asin(u.w))
    }
    def acos(u: AnyVec4) :Vec4 = {
        Vec4(acos(u.x), acos(u.y), acos(u.z), acos(u.w))
    }
    def atan(uy: AnyVec4, ux: AnyVec4) :Vec4 = {
        Vec4(
            atan(uy.x, ux.x),
            atan(uy.y, ux.y),
            atan(uy.z, ux.z),
            atan(uy.w, ux.w)
        )
    }
    def atan(u: AnyVec4) :Vec4 = {
        Vec4(atan(u.x), atan(u.y), atan(u.z), atan(u.w))
    }

    def sinh(u: AnyVec4) :Vec4 = {
        Vec4(sinh(u.x), sinh(u.y), sinh(u.z), sinh(u.w))
    }
    def cosh(u: AnyVec4) :Vec4 = {
        Vec4(cosh(u.x), cosh(u.y), cosh(u.z), cosh(u.w))
    }
    def tanh(u: AnyVec4) :Vec4 = {
        Vec4(tanh(u.x), tanh(u.y), tanh(u.z), tanh(u.w))
    }

    def asinh(u: AnyVec4) :Vec4 = {
        Vec4(asinh(u.x), asinh(u.y), asinh(u.z), asinh(u.w))
    }
    def acosh(u: AnyVec4) :Vec4 = {
        Vec4(acosh(u.x), acosh(u.y), acosh(u.z), acosh(u.w))
    }
    def atanh(u: AnyVec4) :Vec4 = {
        Vec4(atanh(u.x), atanh(u.y), atanh(u.z), atanh(u.w))
    }

    def pow(u: AnyVec4, v: AnyVec4) :Vec4 = {
        Vec4(pow(u.x, v.x), pow(u.y, v.y), pow(u.z, v.z), pow(u.w, v.w))
    }
    def exp(u: AnyVec4) :Vec4 = Vec4(exp(u.x), exp(u.y), exp(u.z), exp(u.w))
    def log(u: AnyVec4) :Vec4 = Vec4(log(u.x), log(u.y), log(u.z), log(u.w))

    def exp2(u: AnyVec4) :Vec4 = {
        Vec4(exp2(u.x), exp2(u.y), exp2(u.z), exp2(u.w))
    }
    def log2(u: AnyVec4) :Vec4 = {
        Vec4(log2(u.x), log2(u.y), log2(u.z), log2(u.w))
    }

    def sqrt(u: AnyVec4) :Vec4 = {
        Vec4(sqrt(u.x), sqrt(u.y), sqrt(u.z), sqrt(u.w))
    }
    def inversesqrt(u: AnyVec4) :Vec4 = {
        Vec4(
            inversesqrt(u.x),
            inversesqrt(u.y),
            inversesqrt(u.z),
            inversesqrt(u.w)
        )
    }

    def abs(u: AnyVec4) :Vec4 = Vec4(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
    def sign(u: AnyVec4) :Vec4 = {
        Vec4(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
    }
    def floor(u: AnyVec4) :Vec4 = {
        Vec4(floor(u.x), floor(u.y), floor(u.z), floor(u.w))
    }
    def trunc(u: AnyVec4) :Vec4 = {
        Vec4(trunc(u.x), trunc(u.y), trunc(u.z), trunc(u.w))
    }
    def round(u: AnyVec4) :Vec4 = {
        Vec4(round(u.x), round(u.y), round(u.z), round(u.w))
    }
    def roundEven(u: AnyVec4) :Vec4 = {
        Vec4(roundEven(u.x), roundEven(u.y), roundEven(u.z), roundEven(u.w))
    }
    def ceil(u: AnyVec4) :Vec4 = {
        Vec4(ceil(u.x), ceil(u.y), ceil(u.z), ceil(u.w))
    }
    def fract(u: AnyVec4) :Vec4 = {
        Vec4(fract(u.x), fract(u.y), fract(u.z), fract(u.w))
    }
    def mod(u: AnyVec4, s: Float) :Vec4 = {
        Vec4(mod(u.x, s), mod(u.y, s), mod(u.z, s), mod(u.w, s))
    }
    def mod(u: AnyVec4, v: AnyVec4) :Vec4 = {
        Vec4(mod(u.x, v.x), mod(u.y, v.y), mod(u.z, v.z), mod(u.w, v.w))
    }
    def modf(u: AnyVec4, i: Vec4) :Vec4 = {
        val s = sign(u)
        val a = abs(u)
        i := s*floor(a)
        s*fract(a)
    }

    def min(u: AnyVec4, s: Float) :Vec4 = {
        Vec4(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
    }
    def min(u: AnyVec4, v: AnyVec4) :Vec4 = {
        Vec4(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
    }
    def max(u: AnyVec4, s: Float) :Vec4 = {
        Vec4(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
    }
    def max(u: AnyVec4, v: AnyVec4) :Vec4 = {
        Vec4(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
    }
    def clamp(u: AnyVec4, minVal: Float, maxVal: Float) :Vec4 = {
        Vec4(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal),
            clamp(u.w, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec4, minVal: AnyVec4, maxVal: AnyVec4) :Vec4 = {
        Vec4(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z),
            clamp(u.w, minVal.w, maxVal.w)
        )
    }

    def mix(u: AnyVec4, v: AnyVec4, a: Float) :Vec4 = {
        val b = 1 - a
        Vec4(b*u.x + a*v.x, b*u.y + a*v.y, b*u.z + a*v.z, b*u.w + a*v.w)
    }
    def mix(u: AnyVec4, v: AnyVec4, a: AnyVec4) :Vec4 = {
        Vec4(
            mix(u.x, v.x, a.x),
            mix(u.y, v.y, a.y),
            mix(u.z, v.z, a.z),
            mix(u.w, v.w, a.w)
        )
    }
    def mix(u: AnyVec4, v: AnyVec4, a: AnyVec4b) :Vec4 = {
        Vec4(
            if (a.x) v.x else u.x,
            if (a.y) v.y else u.y,
            if (a.z) v.z else u.z,
            if (a.w) v.w else u.w
        )
    }

    def step(edge: Float, u: AnyVec4) :Vec4 = {
        Vec4(step(edge, u.x), step(edge, u.y), step(edge, u.z), step(edge, u.w))
    }
    def step(edge: AnyVec4, u: AnyVec4) :Vec4 = {
        Vec4(
            step(edge.x, u.x),
            step(edge.y, u.y),
            step(edge.z, u.z),
            step(edge.w, u.w)
        )
    }
    def smoothstep(edge0: Float, edge1: Float, u: AnyVec4) :Vec4 = {
        Vec4(
            smoothstep(edge0, edge1, u.x),
            smoothstep(edge0, edge1, u.y),
            smoothstep(edge0, edge1, u.z),
            smoothstep(edge0, edge1, u.w)
        )
    }
    def smoothstep(edge0: AnyVec4, edge1: AnyVec4, u: AnyVec4) :Vec4 = {
        Vec4(
            smoothstep(edge0.x, edge1.x, u.x),
            smoothstep(edge0.y, edge1.y, u.y),
            smoothstep(edge0.z, edge1.z, u.z),
            smoothstep(edge0.w, edge1.w, u.w)
        )
    }

    def isnan(u: AnyVec4) :Vec4b = {
        Vec4b(isnan(u.x), isnan(u.y), isnan(u.z), isnan(u.w))
    }
    def isinf(u: AnyVec4) :Vec4b = {
        Vec4b(isinf(u.x), isinf(u.y), isinf(u.z), isinf(u.w))
    }

    def length(u: AnyVec4) :Float = sqrt(u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w)
    def dot(u: AnyVec4, v: AnyVec4) :Float = {
        u.x*v.x + u.y*v.y + u.z*v.z + u.w*v.w
    }
    def distance(u: AnyVec4, v: AnyVec4) :Float = length(u - v)
    def normalize(u: AnyVec4) :Vec4 = u/length(u)

    def faceforward(n: AnyVec4, i: AnyVec4, nref: AnyVec4) :Vec4 = {
        if (dot(nref, i) < 0) Vec4(n) else -n
    }

    def reflect(i: AnyVec4, n: AnyVec4) :Vec4 = {
        i - n*2*dot(n, i)
    }
    def refract(i: AnyVec4, n: AnyVec4, eta: Float) :Vec4 = {
        val dotni = dot(n, i)
        val k = 1 - eta*eta*(1 - dotni*dotni)
        if (k < 0) Vec4(0) else i*eta - n*(eta*dotni + sqrt(k))
    }

    def lessThan(u: AnyVec4, v: AnyVec4) :Vec4b = {
        Vec4b(
            u.x < v.x,
            u.y < v.y,
            u.z < v.z,
            u.w < v.w
        )
    }
    def lessThanEqual(u: AnyVec4, v: AnyVec4) :Vec4b = {
        Vec4b(
            u.x <= v.x,
            u.y <= v.y,
            u.z <= v.z,
            u.w <= v.w
        )
    }
    def greaterThan(u: AnyVec4, v: AnyVec4) :Vec4b = {
        Vec4b(
            u.x > v.x,
            u.y > v.y,
            u.z > v.z,
            u.w > v.w
        )
    }
    def greaterThanEqual(u: AnyVec4, v: AnyVec4) :Vec4b = {
        Vec4b(
            u.x >= v.x,
            u.y >= v.y,
            u.z >= v.z,
            u.w >= v.w
        )
    }
    def equal(u: AnyVec4, v: AnyVec4) :Vec4b = {
        Vec4b(
            u.x == v.x,
            u.y == v.y,
            u.z == v.z,
            u.w == v.w
        )
    }
    def notEqual(u: AnyVec4, v: AnyVec4) :Vec4b = {
        Vec4b(
            u.x != v.x,
            u.y != v.y,
            u.z != v.z,
            u.w != v.w
        )
    }

    def noise1(u: AnyVec4) :Float = {
        float(SimplexNoise.noise(u.x, u.y, u.z, u.w))
    }
    def noise2(u: AnyVec4) :Vec2 = {
        Vec2(
          float(SimplexNoise.noise(u.x, u.y, u.z, u.w)),
          float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1, u.w + of1))
        )
    }
    def noise3(u: AnyVec4) :Vec3 = {
        Vec3(
          float(SimplexNoise.noise(u.x, u.y, u.z, u.w)),
          float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1, u.w + of1)),
          float(SimplexNoise.noise(u.x + of2, u.y + of2, u.z + of2, u.w + of2))
        )
    }
    def noise4(u: AnyVec4) :Vec4 = {
        Vec4(
          float(SimplexNoise.noise(u.x, u.y, u.z, u.w)),
          float(SimplexNoise.noise(u.x + of1, u.y + of1, u.z + of1, u.w + of1)),
          float(SimplexNoise.noise(u.x + of2, u.y + of2, u.z + of2, u.w + of2)),
          float(SimplexNoise.noise(u.x + of3, u.y + of3, u.z + of3, u.w + of3))
        )
    }

    // Mat functions
    def matrixCompMult(a: Mat2, b: Mat2) :Mat2 = {
        Mat2(
            a.m00*b.m00, a.m10*b.m10,
            a.m01*b.m01, b.m11*b.m11
        )
    }
    def matrixCompMult(a: Mat2x3, b: Mat2x3) :Mat2x3 = {
        Mat2x3(
            a.m00*b.m00, a.m10*b.m10,
            a.m01*b.m01, b.m11*b.m11,
            a.m02*b.m02, b.m12*b.m12
        )
    }
    def matrixCompMult(a: Mat2x4, b: Mat2x4) :Mat2x4 = {
        Mat2x4(
            a.m00*b.m00, a.m10*b.m10,
            a.m01*b.m01, b.m11*b.m11,
            a.m02*b.m02, b.m12*b.m12,
            a.m03*b.m03, b.m13*b.m13
        )
    }
    def matrixCompMult(a: Mat3x2, b: Mat3x2) :Mat3x2 = {
        Mat3x2(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
            a.m01*b.m01, b.m11*b.m11, a.m21*b.m21
        )
    }
    def matrixCompMult(a: Mat3, b: Mat3) :Mat3 = {
        Mat3(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
            a.m01*b.m01, b.m11*b.m11, a.m21*b.m21,
            a.m02*b.m02, b.m12*b.m12, a.m22*b.m22
        )
    }
    def matrixCompMult(a: Mat3x4, b: Mat3x4) :Mat3x4 = {
        Mat3x4(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
            a.m01*b.m01, b.m11*b.m11, a.m21*b.m21,
            a.m02*b.m02, b.m12*b.m12, a.m22*b.m22,
            a.m03*b.m03, b.m13*b.m13, a.m23*b.m23
        )
    }
    def matrixCompMult(a: Mat4x2, b: Mat4x2) :Mat4x2 = {
        Mat4x2(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
            a.m01*b.m01, b.m11*b.m11, a.m21*b.m21, a.m31*b.m31
        )
    }
    def matrixCompMult(a: Mat4x3, b: Mat4x3) :Mat4x3 = {
        Mat4x3(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
            a.m01*b.m01, b.m11*b.m11, a.m21*b.m21, a.m31*b.m31,
            a.m02*b.m02, b.m12*b.m12, a.m22*b.m22, a.m32*b.m32
        )
    }
    def matrixCompMult(a: Mat4, b: Mat4) :Mat4 = {
        Mat4(
            a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
            a.m01*b.m01, b.m11*b.m11, a.m21*b.m21, a.m31*b.m31,
            a.m02*b.m02, b.m12*b.m12, a.m22*b.m22, a.m32*b.m32,
            a.m03*b.m03, b.m13*b.m13, a.m23*b.m23, a.m33*b.m33
        )
    }

    def outerProduct(c: AnyVec2, r: AnyVec2) :Mat2 = {
        Mat2(
            c.x*r.x, c.y*r.x,
            c.x*r.y, c.y*r.y
        )
    }
    def outerProduct(c: AnyVec2, r: AnyVec3) :Mat2x3 = {
        Mat2x3(
            c.x*r.x, c.y*r.x,
            c.x*r.y, c.y*r.y,
            c.x*r.z, c.y*r.z
        )
    }
    def outerProduct(c: AnyVec2, r: AnyVec4) :Mat2x4 = {
        Mat2x4(
            c.x*r.x, c.y*r.x,
            c.x*r.y, c.y*r.y,
            c.x*r.z, c.y*r.z,
            c.x*r.w, c.y*r.w
        )
    }
    def outerProduct(c: AnyVec3, r: AnyVec2) :Mat3x2 = {
        Mat3x2(
            c.x*r.x, c.y*r.x, c.z*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y
        )
    }
    def outerProduct(c: AnyVec3, r: AnyVec3) :Mat3 = {
        Mat3(
            c.x*r.x, c.y*r.x, c.z*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y,
            c.x*r.z, c.y*r.z, c.z*r.z
        )
    }
    def outerProduct(c: AnyVec3, r: AnyVec4) :Mat3x4 = {
        Mat3x4(
            c.x*r.x, c.y*r.x, c.z*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y,
            c.x*r.z, c.y*r.z, c.z*r.z,
            c.x*r.w, c.y*r.w, c.z*r.w
        )
    }
    def outerProduct(c: AnyVec4, r: AnyVec2) :Mat4x2 = {
        Mat4x2(
            c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y
        )
    }
    def outerProduct(c: AnyVec4, r: AnyVec3) :Mat4x3 = {
        Mat4x3(
            c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y,
            c.x*r.z, c.y*r.z, c.z*r.z, c.w*r.z
        )
    }
    def outerProduct(c: AnyVec4, r: AnyVec4) :Mat4 = {
        Mat4(
            c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
            c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y,
            c.x*r.z, c.y*r.z, c.z*r.z, c.w*r.z,
            c.x*r.w, c.y*r.w, c.z*r.w, c.w*r.w
        )
    }

    def transpose(a: AnyMat2) :Mat2 = {
        Mat2(
            a.m00, a.m01,
            a.m10, a.m11
        )
    }
    def transpose(a: AnyMat3x2) :Mat2x3 = {
        Mat2x3(
            a.m00, a.m01,
            a.m10, a.m11,
            a.m20, a.m21
        )
    }
    def transpose(a: AnyMat4x2) :Mat2x4 = {
        Mat2x4(
            a.m00, a.m01,
            a.m10, a.m11,
            a.m20, a.m21,
            a.m30, a.m31
        )
    }
    def transpose(a: AnyMat2x3) :Mat3x2 = {
        Mat3x2(
            a.m00, a.m01, a.m02,
            a.m10, a.m11, a.m12
        )
    }
    def transpose(a: AnyMat3) :Mat3 = {
        Mat3(
            a.m00, a.m01, a.m02,
            a.m10, a.m11, a.m12,
            a.m20, a.m21, a.m22
        )
    }
    def transpose(a: AnyMat4x3) :Mat3x4 = {
        Mat3x4(
            a.m00, a.m01, a.m02,
            a.m10, a.m11, a.m12,
            a.m20, a.m21, a.m22,
            a.m30, a.m31, a.m32
        )
    }
    def transpose(a: AnyMat2x4) :Mat4x2 = {
        Mat4x2(
            a.m00, a.m01, a.m02, a.m03,
            a.m10, a.m11, a.m12, a.m13
        )
    }
    def transpose(a: AnyMat3x4) :Mat4x3 = {
        Mat4x3(
            a.m00, a.m01, a.m02, a.m03,
            a.m10, a.m11, a.m12, a.m13,
            a.m20, a.m21, a.m22, a.m23
        )
    }
    def transpose(a: AnyMat4) :Mat4 = {
        Mat4(
            a.m00, a.m01, a.m02, a.m03,
            a.m10, a.m11, a.m12, a.m13,
            a.m20, a.m21, a.m22, a.m23,
            a.m30, a.m31, a.m32, a.m33
        )
    }

    // Ideally this method should not be here, but compiler complains when using
    // static imports for overloaded functions from different objects.
    // Since normalize(u: AnyVec) are here, this methods has to be here as well.
    def normalize(q: AnyQuat4) :Quat4 = {
        q*inversesqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
    }


    // *** Extra Math functions ************************************************

    // Lerp
    def lerp(x: Float, y: Float, a: Float) = mix(x, y, a)
    def lerp(u: AnyVec2, v: AnyVec2, a: Float) = mix(u, v, a)
    def lerp(u: AnyVec3, v: AnyVec3, a: Float) = mix(u, v, a)
    def lerp(u: AnyVec4, v: AnyVec4, a: Float) = mix(u, v, a)

    def lerp(m: Mat2, n: Mat2, a: Float) :Mat2 = {
        val b = 1 - a

        Mat2(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11
        )
    }
    def lerp(m: Mat2x3, n: Mat2x3, a: Float) :Mat2x3 = {
        val b = 1 - a

        Mat2x3(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12
        )
    }
    def lerp(m: Mat2x4, n: Mat2x4, a: Float) :Mat2x4 = {
        val b = 1 - a

        Mat2x4(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12,
            b*m.m03 + a*n.m03, b*m.m13 + a*n.m13
        )
    }
    def lerp(m: Mat3x2, n: Mat3x2, a: Float) :Mat3x2 = {
        val b = 1 - a

        Mat3x2(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21
        )
    }
    def lerp(m: Mat3, n: Mat3, a: Float) :Mat3 = {
        val b = 1 - a

        Mat3(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22
        )
    }
    def lerp(m: Mat3x4, n: Mat3x4, a: Float) :Mat3x4 = {
        val b = 1 - a

        Mat3x4(
            b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
            b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
            b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22,
            b*m.m03 + a*n.m03, b*m.m13 + a*n.m13, b*m.m23 + a*n.m23
        )
    }
    def lerp(m: Mat4x2, n: Mat4x2, a: Float) :Mat4x2 = {
      val b = 1 - a

      Mat4x2(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31
      )
    }
    def lerp(m: Mat4x3, n: Mat4x3, a: Float) :Mat4x3 = {
      val b = 1 - a

      Mat4x3(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
        b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32
      )
    }
    def lerp(m: Mat4, n: Mat4, a: Float) :Mat4 = {
      val b = 1 - a

      Mat4(
        b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
        b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
        b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32,
        b*m.m03 +a*n.m03, b*m.m13 +a*n.m13, b*m.m23 +a*n.m23, b*m.m33 +a*n.m33
      )
    }

    def lengthSquare(x: Float) :Float = x*x
    def lengthSquare(u: AnyVec2) :Float = u.x*u.x + u.y*u.y
    def lengthSquare(u: AnyVec3) :Float = u.x*u.x + u.y*u.y + u.z*u.z
    def lengthSquare(u: AnyVec4) :Float = u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w

    def hasErrors(x: Float) :Boolean = isinf(x) || isnan(x)
    def hasErrors(u: AnyVec2) :Boolean = u.hasErrors
    def hasErrors(u: AnyVec3) :Boolean = u.hasErrors
    def hasErrors(u: AnyVec4) :Boolean = u.hasErrors
    def hasErrors(q: AnyQuat4) :Boolean = q.hasErrors
    def hasErrors(m: AnyMat2) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat2x3) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat2x4) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3x2) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat3x4) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4x2) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4x3) :Boolean = m.hasErrors
    def hasErrors(m: AnyMat4) :Boolean = m.hasErrors

    def approxEqual(x: Float, y: Float, absDelta: Float) :Boolean = {
        abs(x - y) < absDelta
    }
    def approxEqual(u: AnyVec2, v: AnyVec2, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta
    }
    def approxEqual(u: AnyVec3, v: AnyVec3, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta &&
        abs(v.z - u.z) < absDelta
    }
    def approxEqual(u: AnyVec4, v: AnyVec4, absDelta: Float) :Boolean = {
        abs(v.x - u.x) < absDelta &&
        abs(v.y - u.y) < absDelta &&
        abs(v.z - u.z) < absDelta &&
        abs(v.w - u.w) < absDelta
    }
    def approxEqual(p: AnyQuat4, q: AnyQuat4, absDelta: Float) :Boolean = {
        abs(p.a - q.a) < absDelta &&
        abs(p.b - q.b) < absDelta &&
        abs(p.c - q.c) < absDelta &&
        abs(p.d - q.d) < absDelta
    }
    def approxEqual(m: AnyMat2, n: AnyMat2, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta
        )
    }
    def approxEqual(m: AnyMat2x3, n: AnyMat2x3, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&

            abs(n.m02 - m.m02) < absDelta &&
            abs(n.m12 - m.m12) < absDelta
        )
    }
    def approxEqual(m: AnyMat2x4, n: AnyMat2x4, absDelta: Float) :Boolean = {
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

    def approxEqual(m: AnyMat3x2, n: AnyMat3x2, absDelta: Float) :Boolean = {
        (
            abs(n.m00 - m.m00) < absDelta &&
            abs(n.m10 - m.m10) < absDelta &&
            abs(n.m20 - m.m20) < absDelta &&

            abs(n.m01 - m.m01) < absDelta &&
            abs(n.m11 - m.m11) < absDelta &&
            abs(n.m21 - m.m21) < absDelta
        )
    }
    def approxEqual(m: AnyMat3, n: AnyMat3, absDelta: Float) :Boolean = {
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
    def approxEqual(m: AnyMat3x4, n: AnyMat3x4, absDelta: Float) :Boolean = {
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
    def approxEqual(m: AnyMat4x2, n: AnyMat4x2, absDelta: Float) :Boolean = {
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
    def approxEqual(m: AnyMat4x3, n: AnyMat4x3, absDelta: Float) :Boolean = {
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
    def approxEqual(m: AnyMat4, n: AnyMat4, absDelta: Float) :Boolean = {
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

    // Determinant and inverse
    def det(m: AnyMat2) :Float = m.m00*m.m11 - m.m01*m.m10

    def det(m: AnyMat3) :Float = {
        import m._

        val c0 = m11*m22 - m12*m21
        val c1 = m12*m20 - m10*m22
        val c2 = m10*m21 - m11*m20

        m00*c0 + m01*c1 + m02*c2
    }

    def det(m: AnyMat4) :Float = {
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
    def inverse(m: AnyMat2) :Mat2 = {
        val detInv = 1/det(m)
        Mat2(
            m.m11*detInv, -m.m10*detInv,
            -m.m01*detInv, m.m00*detInv
          )
    }

    /**
     * This method is equivalent to casting the matrix as 3x3 then inverting it
     * and the casting the result back to 2x3.<br/>
     *
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using InverseTransform(translation, rotation, scale).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat2x3) :Mat2x3 = {
        import m._

        val det = m00*m11 - m01*m10

        val mat = Mat2x3(
            m11, -m10,
            -m01, m00,
            m01*m12 - m02*m11, m02*m10 - m00*m12
        )

        mat /= det
        mat
    }

    /**
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using InverseTransform(translation, rotation, scale).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat3) :Mat3 = {
        import m._

        val c0 = m11*m22 - m12*m21
        val c1 = m12*m20 - m10*m22
        val c2 = m10*m21 - m11*m20

        val det = m00*c0 + m01*c1 + m02*c2

        val mat = Mat3(
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
     * This method is equivalent to casting the matrix as 4x4 then inverting it
     * and the casting the result back to 3x4.<br/>
     *
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using InverseTransform(translation, rotation, scale).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat3x4) :Mat3x4 = {
        import m._

        val fA0 = m00*m11 - m01*m10
        val fA1 = m00*m12 - m02*m10
        val fA2 = m00*m13 - m03*m10
        val fA3 = m01*m12 - m02*m11
        val fA4 = m01*m13 - m03*m11
        val fA5 = m02*m13 - m03*m12

        val det = fA0*m22 - fA1*m21 + fA3*m20

        val mat = Mat3x4(
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

    /**
     * This is a general matrix inverse. You can invert transofrmations
     * quicker by using InverseTransform(translation, rotation, scale).
     * A rotation matrix that does not scale can be inverted even faster by
     * using transpose. In the latter case you can avoid inverse alltogether
     * by using transpose multiplication:
     * instead of multiplying a matrix by a vectors (M*v),
     * you can multiply the vector by the matrix (v*M).
     *
     * <br/>If matrix determinant is zero the result is undefined.
     */
    def inverse(m: AnyMat4) :Mat4 = {
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

        val mat = Mat4(
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

    def transposeSubMat2(m: RotationSubMat2) {
        import m._

        val t10 = m10
        m10 = m01
        m01 = t10
    }

    def transposeSubMat3(m: RotationSubMat3) {
        import m._

        val t10 = m10
        val t20 = m20
        val t21 = m21
        m10 = m01
        m20 = m02
        m01 = t10
        m21 = m12
        m02 = t20
        m12 = t21
    }

    // Quaternion
    def normSquare(q: AnyQuat4) :Float = q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d
    def norm(q: AnyQuat4) :Float = {
        sqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
    }
    def conjugate(q: AnyQuat4) :Quat4 = Quat4(q.a, -q.b, -q.c, -q.d)

    /**
     * This method is here for completness. Normally you should work with
     * unit quaternions (<code>norm(q) == 1</code>), and in this case
     * <code>inverse(q) == conjugate(q)</code>.
     */
    def inverse(q: AnyQuat4) :Quat4 = conjugate(q)/normSquare(q)

    def slerp(p: AnyQuat4, q: AnyQuat4, a: Float) :Quat4 = {
        if (approxEqual(p, q, 1e-5f)) return Quat4(q)

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

        Quat4(
            s*p.a + t*q.a,
            s*p.b + t*q.b,
            s*p.c + t*q.c,
            s*p.d + t*q.d
        )
    }

    // Rotation
    /**
     * This method creates a 2d transformation matrix that rotates a vector
     * counterclockwise by the specified angle.
     */
    def rotationMatFrom(angle: Float) :Mat2 = {
        val m = Mat2(1)
        rotationMatFrom(angle, m)
        m
    }
    /**
     * This method creates a 2d transformation matrix that rotates a vector
     * counterclockwise by the specified angle.
     */
    def rotationMatFrom(angle: Float, result: RotationSubMat2) {
        val cosA = cos(angle)
        val sinA = sin(angle)

        result.set(
             cosA, sinA,
            -sinA, cosA
        )
    }

    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation.
     */
    def rotationAngleFrom(m: ConstRotationSubMat2) :Float = {
        acos((m.m00 + m.m11)*0.5f)
    }

    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation.
     */
    def quatFrom(m: ConstRotationSubMat3) :Quat4 = {
        val q = Quat4()
        quatFrom(m, q)
        q
    }
    /**
     * The result is undefined if the matrix does not represent
     * non-scaling rotation.
     */
    def quatFrom(m: ConstRotationSubMat3, result: Quat4) {
        import m._

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
    }

    /**
     * The result is undefined for axis with non-unit length.
     */
    def quatFrom(angle: Float, axis: AnyVec3) :Quat4 = {
        val s = sin(angle/2)
        Quat4(cos(angle/2), s*axis.x, s*axis.y, s*axis.z)
    }
    /**
     * The result is undefined for axis with non-unit length.
     */
    def quatFrom(angle: Float, axis: AnyVec3, result: Quat4) {
        val s = sin(angle/2)
        result.a = cos(angle/2)
        result.b = s*axis.x
        result.c = s*axis.y
        result.d = s*axis.z
    }

    /**
     * The result is undefined for quaternions with non-unit norm.
     */
    def rotationMatFrom(q: AnyQuat4) :Mat3 = {
        val m = Mat3(1)
        rotationMatFrom(q, m)
        m
    }
    /**
     * The result is undefined for quaternions with non-unit norm.
     */
    def rotationMatFrom(q: AnyQuat4, result: RotationSubMat3) {
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

        result.set(
            tc - td, bc + da, bd - ca,
            bc - da, 1 - tb - td, cd + ba,
            bd + ca, cd - ba, tc - tb
        )
    }
    /**
     * The result is undefined for axis with non-unit length.
     */
    def rotationMatFrom(angle: Float, axis: AnyVec3) :Mat3 = {
        val m = Mat3(1)
        rotationMatFrom(angle, axis, m)
        m
    }
    /**
     * The result is undefined for axis with non-unit length.
     */
    def rotationMatFrom(angle: Float, axis: AnyVec3, result: RotationSubMat3)
    {
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

        result.set(
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
    def angleAxisFrom(q: AnyQuat4, axisResult: Vec3) :Float = {
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
    def angleAxisFrom(m: ConstRotationSubMat3, axisResult: Vec3) :Float = {
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

    def lookAt(direction: AnyVec3, up: AnyVec3) :Mat3 = {
        val m = Mat3(1)
        lookAt(direction, up, m)
        m
    }
    def lookAt(direction: AnyVec3, up: AnyVec3, m: RotationSubMat3) {
        val zaxis = normalize(direction)
        val xaxis = normalize(cross(up, zaxis))
        val yaxis = cross(zaxis, xaxis)
        m.set(
            xaxis.x, xaxis.y, xaxis.z,
            yaxis.x, yaxis.y, yaxis.z,
            zaxis.x, zaxis.y, zaxis.z
        )
    }

    // Projection
    def perspective(fieldOfView: Float, aspectRatio: Float,
                    near: Float, far: Float)
    :Mat4 =
    {
        val focus = 1/tan(fieldOfView * 0.5f)
        val n_f = 1/(near - far)

        Mat4(
            focus/aspectRatio, 0, 0, 0,
            0, focus, 0, 0,
            0, 0, (near + far)*n_f, -1,
            0, 0, 2*near*far*n_f, 0
        )
    }

    def ortho(left: Float, right: Float,
              bottom: Float, top: Float,
              near: Float, far: Float)
    :Mat4 =
    {
        val r_l = 1/(right - left);
        val t_b = 1/(top - bottom);
        val f_n = 1/(far - near);

        Mat4(
            2*r_l, 0, 0, 0,
            0, 2*t_b, 0, 0,
            0, 0, -2*f_n, 0,
            -(right + left)*r_l, -(top + bottom)*t_b, -(far + near)*f_n, 1
        )
    }
}
