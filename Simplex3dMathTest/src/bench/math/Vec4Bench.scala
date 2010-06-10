/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010, Simplex3d Team
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

package bench.math

import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Vec4Bench {
  def main(args: Array[String]) {
    new AccessorBench().run()
  }
}

class AccessorBench {
  def run() {
    val length = 20000
    val loops = 20000

    var start = 0L

    start = System.currentTimeMillis
    testModified(length, loops)
    val modifiedTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testOriginal(length, loops)
    val originalTime = System.currentTimeMillis - start

    println("Original time: " + originalTime +
            ", modified time: " + modifiedTime + ".")
  }

  def testOriginal(length: Int, loops: Int) {
    val answer = Vec4(0)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {
        
        // Bench code
        answer += Vec4(i, i + 1, i + 2, i + 3)
        answer -= Vec4(l)
        answer *= 1 + 1f/(i + 1)
        val prod = answer * answer
        answer /= sqrt(prod.x + prod.y + prod.z + prod.w)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testModified(length: Int, loops: Int) {
    val answer = ModifiedVec4(0)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {
        
        // Bench code
        answer += ModifiedVec4(i, i + 1, i + 2, i + 3)
        answer -= ModifiedVec4(l)
        answer *= 1 + 1f/(i + 1)
        val prod = answer * answer
        answer /= sqrt(prod.x + prod.y + prod.z + prod.w)

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}

// Modified Vec4
import simplex3d.math._

sealed abstract class AnyModifiedVec4 {

  protected var vx: Float = _
  protected var vy: Float = _
  protected var vz: Float = _
  protected var vw: Float = _

  def x: Float = vx
  def y: Float = vy
  def z: Float = vz
  def w: Float = vw

  def r = vx
  def g = vy
  def b = vz
  def a = vw

  def s = vx
  def t = vy
  def p = vz
  def q = vw


  def apply(i: Int) :Float = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j)
    }
  }

  def unary_-() = ModifiedVec4(-x, -y, -z, -w)
  def *(s: Float) = ModifiedVec4(x*s, y*s, z*s, w*s)
  def /(s: Float) = { val inv = 1/s; ModifiedVec4(x*inv, y*inv, z*inv, w*inv) }

  def +(u: AnyModifiedVec4) = ModifiedVec4(x + u.x, y + u.y, z + u.z, w + u.w)
  def -(u: AnyModifiedVec4) = ModifiedVec4(x - u.x, y - u.y, z - u.z, w - u.w)
  def *(u: AnyModifiedVec4) = ModifiedVec4(x * u.x, y * u.y, z * u.z, w * u.w)
  def /(u: AnyModifiedVec4) = ModifiedVec4(x / u.x, y / u.y, z / u.z, w / u.w)

  def ==(u: AnyModifiedVec4) :Boolean = {
    if (u eq null) false
    else x == u.x && y == u.y && z == u.z && w == u.w
  }

  def !=(u: AnyModifiedVec4) :Boolean = !(this == u)

  private[math] def hasErrors: Boolean = {
    import java.lang.Float._
    (
      isNaN(x) || isInfinite(x) ||
      isNaN(y) || isInfinite(y) ||
      isNaN(z) || isInfinite(z) ||
      isNaN(w) || isInfinite(w)
    )
  }

  override def toString = {
    this.getClass.getSimpleName +
    "(" + x + ", " + y + ", " + z + ", " + w + ")"
  }
}

final class ConstModifiedVec4 private (cx: Float, cy: Float,
                 cz: Float, cw: Float)
extends AnyModifiedVec4
{
  vx = cx
  vy = cy
  vz = cz
  vw = cw
}

final class ModifiedVec4 private (cx: Float, cy: Float,
              cz: Float, cw: Float)
extends AnyModifiedVec4
{
  vx = cx
  vy = cy
  vz = cz
  vw = cw

  override def x: Float = vx
  override def y: Float = vy
  override def z: Float = vz
  override def w: Float = vw

  override def r = x
  override def g = y
  override def b = z
  override def a = w

  override def s = x
  override def t = y
  override def p = z
  override def q = w

  def x_=(x: Float) { vx = x }
  def y_=(y: Float) { vy = y }
  def z_=(z: Float) { vz = z }
  def w_=(w: Float) { vw = w }

  def r_=(r: Float) { vx = r }
  def g_=(g: Float) { vy = g }
  def b_=(b: Float) { vz = b }
  def a_=(a: Float) { vw = a }

  def s_=(s: Float) { vx = s }
  def t_=(t: Float) { vy = t }
  def p_=(p: Float) { vz = p }
  def q_=(q: Float) { vw = q }


  def *=(s: Float) { x *= s; y *= s; z *= s; w *= s }
  def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }

  def +=(u: AnyModifiedVec4) { x += u.x; y += u.y; z += u.z; w += u.w }
  def -=(u: AnyModifiedVec4) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
  def *=(u: AnyModifiedVec4) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
  def /=(u: AnyModifiedVec4) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }

  def :=(u: AnyModifiedVec4) { x = u.x; y = u.y; z = u.z; w = u.w }
  def set(x: Float, y: Float, z: Float, w: Float) {
    this.x = x; this.y = y; this.z = z; this.w = w
  }

  def update(i: Int, s: Float) {
    i match {
      case 0 => x = s
      case 1 => y = s
      case 2 => z = s
      case 3 => w = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j)
    }
  }
}

object ModifiedVec4 {

  def apply(s: Float) =
    new ModifiedVec4(s, s, s, s)

  def apply(x: Float, y: Float, z: Float, w: Float) =
    new ModifiedVec4(x, y, z, w)

  def apply(u: AnyModifiedVec4) =
    new ModifiedVec4(u.x, u.y, u.z, u.w)
}
