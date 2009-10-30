/*
 * Simplex3D, Math package
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * CLASSPATH EXCEPTION FOR UNMODIFIED WORK:
 * Linking this library statically or dynamically with other modules is making
 * a combined work based on this library. Thus, the terms and conditions of
 * the GNU General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce
 * an executable, regardless of the license terms of these independent modules,
 * and to copy and distribute the resulting executable under terms of your
 * choice, provided that you also meet, for each linked independent module,
 * the terms and conditions of the license of that module. An independent module
 * is a module which is not derived from or based on this library. If you modify
 * this library in any way, then this exception is null and void and no longer
 * applies, in this case delete this exception statement from your version.
 */

package simplex3d.math

import java.lang.{Math => JMath, StrictMath => SMath}
import toxi.math.noise.SimplexNoise


/**
 * @author Aleksey Nikiforov (lex)
 */
object VecMath {
    // Implicits
    implicit def floatToExtendedFloat(s: Float) = new ExtendedFloat(s)
    implicit def intToExtendedInt(s: Int) = new ExtendedInt(s)
    implicit def vec2iToVec2(u: AnyVec2i) :Vec2 = Vec2(u)
    implicit def vec3iToVec3(u: AnyVec3i) :Vec3 = Vec3(u)
    implicit def vec4iToVec4(u: AnyVec4i) :Vec4 = Vec4(u)

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

    // Cast
    def int(x: Long) :Int = x.asInstanceOf[Int]
    def int(x: Float) :Int = x.asInstanceOf[Int]
    def int(x: Double) :Int = x.asInstanceOf[Int]

    def float(x: Double) :Float = x.asInstanceOf[Float]

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
    def round(x: Float) :Float = SMath.round(x)
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

    // Int functions
    def abs(x: Int) :Int = if (x < 0) -x else x
    def sign(x: Int) :Int = if (x > 0) 1 else if (x < 0) -1 else 0
    def min(x: Int, y: Int) :Int = if (x < y) x else y
    def max(x: Int, y: Int) :Int = if (x > y) x else y
    def clamp(x: Int, minVal: Int, maxVal: Int) :Int = {
        if (x < minVal) minVal
        else if (x > maxVal) maxVal
        else x
    }

    // Vec2i functions
    def abs(u: AnyVec2i) :Vec2i = Vec2i(abs(u.x), abs(u.y))
    def sign(u: AnyVec2i) :Vec2i = Vec2i(sign(u.x), sign(u.y))
    def min(u: AnyVec2i, s: Int) :Vec2i = Vec2i(min(u.x, s), min(u.y, s))
    def min(u: AnyVec2i, v: AnyVec2i) :Vec2i = {
        Vec2i(min(u.x, v.x), min(u.y, v.y))
    }
    def max(u: AnyVec2i, s: Int) :Vec2i = Vec2i(max(u.x, s), max(u.y, s))
    def max(u: AnyVec2i, v: AnyVec2i) :Vec2i = {
        Vec2i(max(u.x, v.x), max(u.y, v.y))
    }
    def clamp(u: AnyVec2i, minVal: Int, maxVal: Int) :Vec2i = {
        Vec2i(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
    }
    def clamp(u: AnyVec2i, minVal: AnyVec2i, maxVal: AnyVec2i) :Vec2i = {
        Vec2i(clamp(u.x, minVal.x, maxVal.x), clamp(u.y, minVal.y, maxVal.y))
    }
    
    // Vec3i functions
    def abs(u: AnyVec3i) :Vec3i = Vec3i(abs(u.x), abs(u.y), abs(u.z))
    def sign(u: AnyVec3i) :Vec3i = Vec3i(sign(u.x), sign(u.y), sign(u.z))
    def min(u: AnyVec3i, s: Int) :Vec3i = {
        Vec3i(min(u.x, s), min(u.y, s), min(u.z, s))
    }
    def min(u: AnyVec3i, v: AnyVec3i) :Vec3i = {
        Vec3i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
    }
    def max(u: AnyVec3i, s: Int) :Vec3i = {
        Vec3i(max(u.x, s), max(u.y, s), max(u.z, s))
    }
    def max(u: AnyVec3i, v: AnyVec3i) :Vec3i = {
        Vec3i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
    }
    def clamp(u: AnyVec3i, minVal: Int, maxVal: Int) :Vec3i = {
        Vec3i(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec3i, minVal: AnyVec3i, maxVal: AnyVec3i) :Vec3i = {
        Vec3i(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z)
        )
    }

    // Vec4i functions
    def abs(u: AnyVec4i) :Vec4i = Vec4i(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
    def sign(u: AnyVec4i) :Vec4i = {
        Vec4i(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
    }
    def min(u: AnyVec4i, s: Int) :Vec4i = {
        Vec4i(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
    }
    def min(u: AnyVec4i, v: AnyVec4i) :Vec4i = {
        Vec4i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
    }
    def max(u: AnyVec4i, s: Int) :Vec4i = {
        Vec4i(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
    }
    def max(u: AnyVec4i, v: AnyVec4i) :Vec4i = {
        Vec4i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
    }
    def clamp(u: AnyVec4i, minVal: Int, maxVal: Int) :Vec4i = {
        Vec4i(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal),
            clamp(u.w, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec4i, minVal: AnyVec4i, maxVal: AnyVec4i) :Vec4i = {
        Vec4i(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z),
            clamp(u.w, minVal.w, maxVal.w)
        )
    }

    // Vec2b functions
    def any(u: AnyVec2b) :Boolean = {
        u.x || u.y
    }
    def all(u: AnyVec2b) :Boolean = {
        u.x && u.y
    }
    def not(u: AnyVec2b) :Vec2b = Vec2b(!u.x, !u.y)

    // Vec3b functions
    def any(u: AnyVec3b) :Boolean = {
        u.x || u.y || u.z
    }
    def all(u: AnyVec3b) :Boolean = {
        u.x && u.y && u.z
    }
    def not(u: AnyVec3b) :Vec3b = Vec3b(!u.x, !u.y, !u.z)
    
    // Vec4b functions
    def any(u: AnyVec4b) :Boolean = {
        u.x || u.y || u.z || u.w
    }
    def all(u: AnyVec4b) :Boolean = {
        u.x && u.y && u.z && u.w
    }
    def not(u: AnyVec4b) :Vec4b = Vec4b(!u.x, !u.y, !u.z, !u.w)

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

    // Ideally this method should not be here, but compiler complains when doing
    // static imports of overloaded functions from different objects.
    // Since normalize(u: AnyVecN) are here, this methods has to be here too.
    def normalize(q: AnyQuat4) :Quat4 = {
        q*inversesqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
    }
}
