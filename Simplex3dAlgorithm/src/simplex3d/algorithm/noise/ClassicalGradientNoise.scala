/*
 * Simplex3dAlgorithm - Noise Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dAlgorithm.
 *
 * Simplex3dAlgorithm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dAlgorithm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.algorithm.noise


/** This is an implementation of Classical Gradient Noise.
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
class ClassicalGradientNoise(seed: Int)
extends TiledNoiseSource(seed) with Serializable
{

  
  // *** Hash **********************************************************************************************************
  
  private val a = (seed ^ 0x97CD8F8C) | ((1 << 16) + 1)
  private val b = seed ^ 0xA4BAA665
  
  private val c = (seed ^ 0xB5C18E6A) | ((1 << 16) + 1)
  private val d = seed ^ 0xF292D0B2
  
  // 16 bits of hash
  private final def perm(x: Int) :Int = {
    (a*(x ^ c)) >>> 16
  }
  
  // More expencive hash for 1d and 2d noise.
  private final def perm2(x: Int) :Int = {
    val t = (a*(x ^ b)) >>> 16
    (c*(t ^ d)) >>> 16
  }
  
  
  // *** Gradient ******************************************************************************************************
  
  private final def grad2dot(i: Int)(x: Double, y: Double) :Double = {
    import scala.annotation._
    
    ((i & 0x07): @switch) match {
      case 0 => x
      case 1 => y
      case 2 => -x
      case 3 => -y
      case 4 => x + y
      case 5 => x - y
      case 6 => -x + y
      case 7 => -x - y
    }
  }
  
  private final def grad3dot(i: Int)(x: Double, y: Double, z: Double) :Double = {
    import scala.annotation._
    
    val n = i % 12
    var a = 0.0; var b = 0.0
    
    if (n < 8) { a = y; if (n < 4) b = x else b = z }
    else { a = x; b = z }
    
    ((n & 0x03): @switch) match {
      case 0 => a + b
      case 1 => a - b
      case 2 => -a + b
      case 3 => -a - b
    }
  }
  
  private final def grad4dot(i: Int)(x: Double, y: Double, z: Double, w: Double) :Double = {
    import scala.annotation._
    
    val n = i & 0x1F
    var a = 0.0; var b = 0.0; var c = 0.0
    
    if (n < 16) { if (n < 8) { a = y; b = z; c = w } else { a = x; b = z; c = w } }
    else { if (n < 24) { a = x; b = y; c = w } else { a = x; b = y; c = z } }
    
    ((n & 0x07): @switch) match {
      case 0 => a + b + c
      case 1 => a + b - c
      case 2 => a - b + c
      case 3 => a - b - c
      case 4 => -a + b + c
      case 5 => -a + b - c
      case 6 => -a - b + c
      case 7 => -a - b - c
    }
  }

  // *** Util **********************************************************************************************************
  
  private final def ifloor(x: Double) :Long = {
    val i = x.toLong
    if (x > 0 || x == i) i else i - 1
  }

  private final def fade(t: Double) :Double = {
    t*t*t*(t*(t*6 - 15) + 10)
  }


  // *** Noise *********************************************************************************************************
  
  final def apply(x: Double) :Double = {
    val lx = ifloor(x)
    val fx = x - lx
    val ix = lx.toInt

    val n0 = {
      val px = perm2(ix)
      // Gradient function, produces ints in [-8, 8] excluding 0 from perm.
      val grad = if ((px & 0x8) == 0) ((px & 0x7) + 1) else (px | 0xFFFFFFF8)
      grad*fx
    }
    
    val n1 = {
      val px = perm2(ix + 1)
      // Gradient function, produces ints in [-8, 8] excluding 0 from perm.
      val grad = if ((px & 0x8) == 0) ((px & 0x7) + 1) else (px | 0xFFFFFFF8)
      grad*(fx - 1)
    }

    val xfade = fade(fx)
    (n0*(1 - xfade) + n1*xfade)*0.25
  }

  final def apply(x: Double, y: Double) :Double = {
    val lx = ifloor(x)
    val ly = ifloor(y)

    val fx = x - lx
    val fy = y - ly

    val ix = lx.toInt
    val iy = ly.toInt

    val px0 = perm2(ix)
    val px1 = perm2(ix + 1)

    val n00 = grad2dot(perm2(px0 + iy))(fx, fy)
    val n10 = grad2dot(perm2(px1 + iy))(fx - 1, fy)
    val n01 = grad2dot(perm2(px0 + iy + 1))(fx, fy - 1)
    val n11 = grad2dot(perm2(px1 + iy + 1))(fx - 1, fy - 1)

    val xfade = fade(fx)
    val mx0 = n00*(1 - xfade) + n10*xfade
    val mx1 = n01*(1 - xfade) + n11*xfade

    val yfade = fade(fy)
    (mx0*(1 - yfade) + mx1*yfade)*1.5// 1.5 is a guess
  }

  final def apply(x: Double, y: Double, z:Double) :Double = {
    val lx = ifloor(x)
    val ly = ifloor(y)
    val lz = ifloor(z)

    val fx = x - lx
    val fy = y - ly
    val fz = z - lz

    val ix = lx.toInt
    val iy = ly.toInt
    val iz = lz.toInt

    val px0 = perm(ix)
    val px1 = perm(ix + 1)
    val py00 = perm(px0 + iy)
    val py10 = perm(px1 + iy)
    val py01 = perm(px0 + iy + 1)
    val py11 = perm(px1 + iy + 1)

    val n000 = grad3dot(perm(py00 + iz))(fx, fy, fz)
    val n100 = grad3dot(perm(py10 + iz))(fx - 1, fy, fz)
    val n010 = grad3dot(perm(py01 + iz))(fx, fy - 1, fz)
    val n110 = grad3dot(perm(py11 + iz))(fx - 1, fy - 1, fz)
    val n001 = grad3dot(perm(py00 + iz + 1))(fx, fy, fz - 1)
    val n101 = grad3dot(perm(py10 + iz + 1))(fx - 1, fy, fz - 1)
    val n011 = grad3dot(perm(py01 + iz + 1))(fx, fy - 1, fz - 1)
    val n111 = grad3dot(perm(py11 + iz + 1))(fx - 1, fy - 1, fz - 1)

    val xfade = fade(fx)
    val mx00 = n000*(1 - xfade) + n100*xfade
    val mx10 = n010*(1 - xfade) + n110*xfade
    val mx01 = n001*(1 - xfade) + n101*xfade
    val mx11 = n011*(1 - xfade) + n111*xfade

    val yfade = fade(fy)
    val my0 = mx00*(1 - yfade) + mx10*yfade
    val my1 = mx01*(1 - yfade) + mx11*yfade

    val zfade = fade(fz)
    (my0*(1 - zfade) + my1*zfade)*1.3// 1.3 is a guess
  }

  final def apply(x: Double, y: Double, z:Double, w:Double) :Double = {
    val lx = ifloor(x)
    val ly = ifloor(y)
    val lz = ifloor(z)
    val lw = ifloor(w)

    val fx = x - lx
    val fy = y - ly
    val fz = z - lz
    val fw = w - lw

    val ix = lx.toInt
    val iy = ly.toInt
    val iz = lz.toInt
    val iw = lw.toInt

    val px0 = perm(ix)
    val px1 = perm(ix + 1)
    val py00 = perm(px0 + iy)
    val py10 = perm(px1 + iy)
    val py01 = perm(px0 + iy + 1)
    val py11 = perm(px1 + iy + 1)
    val pz000 = perm(py00 + iz)
    val pz100 = perm(py10 + iz)
    val pz010 = perm(py01 + iz)
    val pz110 = perm(py11 + iz)
    val pz001 = perm(py00 + iz + 1)
    val pz101 = perm(py10 + iz + 1)
    val pz011 = perm(py01 + iz + 1)
    val pz111 = perm(py11 + iz + 1)

    val n0000 = grad4dot(perm(pz000 + iw))(fx, fy, fz, fw)
    val n1000 = grad4dot(perm(pz100 + iw))(fx - 1, fy, fz, fw)
    val n0100 = grad4dot(perm(pz010 + iw))(fx, fy - 1, fz, fw)
    val n1100 = grad4dot(perm(pz110 + iw))(fx - 1, fy - 1, fz, fw)
    val n0010 = grad4dot(perm(pz001 + iw))(fx, fy, fz - 1, fw)
    val n1010 = grad4dot(perm(pz101 + iw))(fx - 1, fy, fz - 1, fw)
    val n0110 = grad4dot(perm(pz011 + iw))(fx, fy - 1, fz - 1, fw)
    val n1110 = grad4dot(perm(pz111 + iw))(fx - 1, fy - 1, fz - 1, fw)
    val n0001 = grad4dot(perm(pz000 + iw + 1))(fx, fy, fz, fw - 1)
    val n1001 = grad4dot(perm(pz100 + iw + 1))(fx - 1, fy, fz, fw - 1)
    val n0101 = grad4dot(perm(pz010 + iw + 1))(fx, fy - 1, fz, fw - 1)
    val n1101 = grad4dot(perm(pz110 + iw + 1))(fx - 1, fy - 1, fz, fw - 1)
    val n0011 = grad4dot(perm(pz001 + iw + 1))(fx, fy, fz - 1, fw - 1)
    val n1011 = grad4dot(perm(pz101 + iw + 1))(fx - 1, fy, fz - 1, fw - 1)
    val n0111 = grad4dot(perm(pz011 + iw + 1))(fx, fy - 1, fz - 1, fw - 1)
    val n1111 = grad4dot(perm(pz111 + iw + 1))(fx - 1, fy - 1, fz - 1, fw - 1)

    val xfade = fade(fx)
    val mx000 = n0000*(1 - xfade) + n1000*xfade
    val mx100 = n0100*(1 - xfade) + n1100*xfade
    val mx010 = n0010*(1 - xfade) + n1010*xfade
    val mx110 = n0110*(1 - xfade) + n1110*xfade
    val mx001 = n0001*(1 - xfade) + n1001*xfade
    val mx101 = n0101*(1 - xfade) + n1101*xfade
    val mx011 = n0011*(1 - xfade) + n1011*xfade
    val mx111 = n0111*(1 - xfade) + n1111*xfade

    val yfade = fade(fy)
    val my00 = mx000*(1 - yfade) + mx100*yfade
    val my10 = mx010*(1 - yfade) + mx110*yfade
    val my01 = mx001*(1 - yfade) + mx101*yfade
    val my11 = mx011*(1 - yfade) + mx111*yfade

    val zfade = fade(fz)
    val mz0 = my00*(1 - zfade) + my10*zfade
    val mz1 = my01*(1 - zfade) + my11*zfade

    val wfade = fade(fw)
    (mz0*(1 - wfade) + mz1*wfade)*1.2// 1.2 is a guess
  }

  // Tiled noise
  final val tileSizeX :Double = 1.0
  final val tileSizeY :Double = 1.0
  final val tileSizeZ :Double = 1.0
  final val tileSizeW :Double = 1.0

  final def apply(
    tile: Int,
    x: Double
  ) :Double = {
    val lx = ifloor(x)
    val fx = x - lx
    val ix = lx.toInt & 0x7FFFFFFF

    val n0 = {
      val px = perm2(ix % tile)
      // Gradient function, produces ints in [-8, 8] excluding 0 from perm.
      val grad = if ((px & 0x8) == 0) ((px & 0x7) + 1) else (px | 0xFFFFFFF8)
      grad*fx
    }

    val n1 = {
      val px = perm2((ix + 1) % tile)
      // Gradient function, produces ints in [-8, 8] excluding 0 from perm.
      val grad = if ((px & 0x8) == 0) ((px & 0x7) + 1) else (px | 0xFFFFFFF8)
      grad*(fx - 1)
    }

    val xfade = fade(fx)
    (n0*(1 - xfade) + n1*xfade)*0.25
  }

  final def apply(
    tilex: Int, tiley: Int,
    x: Double, y: Double
  ) :Double = {
    val lx = ifloor(x)
    val ly = ifloor(y)

    val fx = x - lx
    val fy = y - ly

    val ix = lx.toInt & 0x7FFFFFFF
    val iy = ly.toInt & 0x7FFFFFFF

    val px0 = perm2(ix % tilex)
    val px1 = perm2((ix + 1) % tilex)
    val ty = iy % tiley
    val ty1 = (iy + 1) % tiley

    val n00 = grad2dot(perm2(px0 + ty))(fx, fy)
    val n10 = grad2dot(perm2(px1 + ty))(fx - 1, fy)
    val n01 = grad2dot(perm2(px0 + ty1))(fx, fy - 1)
    val n11 = grad2dot(perm2(px1 + ty1))(fx - 1, fy - 1)

    val xfade = fade(fx)
    val mx0 = n00*(1 - xfade) + n10*xfade
    val mx1 = n01*(1 - xfade) + n11*xfade

    val yfade = fade(fy)
    (mx0*(1 - yfade) + mx1*yfade)*1.5// 1.5 is a guess
  }

  final def apply(
    tilex: Int, tiley: Int, tilez: Int,
    x: Double, y: Double, z:Double
  ) :Double = {
    val lx = ifloor(x)
    val ly = ifloor(y)
    val lz = ifloor(z)

    val fx = x - lx
    val fy = y - ly
    val fz = z - lz

    val ix = lx.toInt & 0x7FFFFFFF
    val iy = ly.toInt & 0x7FFFFFFF
    val iz = lz.toInt & 0x7FFFFFFF

    val px0 = perm(ix % tilex)
    val px1 = perm((ix + 1) % tilex)
    val ty = iy % tiley
    val ty1 = (iy + 1) % tiley
    val py00 = perm(px0 + ty)
    val py10 = perm(px1 + ty)
    val py01 = perm(px0 + ty1)
    val py11 = perm(px1 + ty1)
    val tz = iz % tilez
    val tz1 = (iz + 1) % tilez

    val n000 = grad3dot(perm(py00 + tz))(fx, fy, fz)
    val n100 = grad3dot(perm(py10 + tz))(fx - 1, fy, fz)
    val n010 = grad3dot(perm(py01 + tz))(fx, fy - 1, fz)
    val n110 = grad3dot(perm(py11 + tz))(fx - 1, fy - 1, fz)
    val n001 = grad3dot(perm(py00 + tz1))(fx, fy, fz - 1)
    val n101 = grad3dot(perm(py10 + tz1))(fx - 1, fy, fz - 1)
    val n011 = grad3dot(perm(py01 + tz1))(fx, fy - 1, fz - 1)
    val n111 = grad3dot(perm(py11 + tz1))(fx - 1, fy - 1, fz - 1)

    val xfade = fade(fx)
    val mx00 = n000*(1 - xfade) + n100*xfade
    val mx10 = n010*(1 - xfade) + n110*xfade
    val mx01 = n001*(1 - xfade) + n101*xfade
    val mx11 = n011*(1 - xfade) + n111*xfade

    val yfade = fade(fy)
    val my0 = mx00*(1 - yfade) + mx10*yfade
    val my1 = mx01*(1 - yfade) + mx11*yfade

    val zfade = fade(fz)
    (my0*(1 - zfade) + my1*zfade)*1.3// 1.3 is a guess
  }

  final def apply(
    tilex: Int, tiley: Int, tilez: Int, tilew: Int,
    x: Double, y: Double, z:Double, w:Double
  ) :Double = {
    val lx = ifloor(x)
    val ly = ifloor(y)
    val lz = ifloor(z)
    val lw = ifloor(w)

    val fx = x - lx
    val fy = y - ly
    val fz = z - lz
    val fw = w - lw

    val ix = lx.toInt & 0x7FFFFFFF
    val iy = ly.toInt & 0x7FFFFFFF
    val iz = lz.toInt & 0x7FFFFFFF
    val iw = lw.toInt & 0x7FFFFFFF

    val px0 = perm(ix % tilex)
    val px1 = perm((ix + 1) % tilex)
    val ty = iy % tiley
    val ty1 = (iy + 1) % tiley
    val py00 = perm(px0 + ty)
    val py10 = perm(px1 + ty)
    val py01 = perm(px0 + ty1)
    val py11 = perm(px1 + ty1)
    val tz = iz % tilez
    val tz1 = (iz + 1) % tilez
    val pz000 = perm(py00 + tz)
    val pz100 = perm(py10 + tz)
    val pz010 = perm(py01 + tz)
    val pz110 = perm(py11 + tz)
    val pz001 = perm(py00 + tz1)
    val pz101 = perm(py10 + tz1)
    val pz011 = perm(py01 + tz1)
    val pz111 = perm(py11 + tz1)
    val tw = iw % tilew
    val tw1 = (iw + 1) % tilew

    val n0000 = grad4dot(perm(pz000 + tw))(fx, fy, fz, fw)
    val n1000 = grad4dot(perm(pz100 + tw))(fx - 1, fy, fz, fw)
    val n0100 = grad4dot(perm(pz010 + tw))(fx, fy - 1, fz, fw)
    val n1100 = grad4dot(perm(pz110 + tw))(fx - 1, fy - 1, fz, fw)
    val n0010 = grad4dot(perm(pz001 + tw))(fx, fy, fz - 1, fw)
    val n1010 = grad4dot(perm(pz101 + tw))(fx - 1, fy, fz - 1, fw)
    val n0110 = grad4dot(perm(pz011 + tw))(fx, fy - 1, fz - 1, fw)
    val n1110 = grad4dot(perm(pz111 + tw))(fx - 1, fy - 1, fz - 1, fw)
    val n0001 = grad4dot(perm(pz000 + tw1))(fx, fy, fz, fw - 1)
    val n1001 = grad4dot(perm(pz100 + tw1))(fx - 1, fy, fz, fw - 1)
    val n0101 = grad4dot(perm(pz010 + tw1))(fx, fy - 1, fz, fw - 1)
    val n1101 = grad4dot(perm(pz110 + tw1))(fx - 1, fy - 1, fz, fw - 1)
    val n0011 = grad4dot(perm(pz001 + tw1))(fx, fy, fz - 1, fw - 1)
    val n1011 = grad4dot(perm(pz101 + tw1))(fx - 1, fy, fz - 1, fw - 1)
    val n0111 = grad4dot(perm(pz011 + tw1))(fx, fy - 1, fz - 1, fw - 1)
    val n1111 = grad4dot(perm(pz111 + tw1))(fx - 1, fy - 1, fz - 1, fw - 1)

    val xfade = fade(fx)
    val mx000 = n0000*(1 - xfade) + n1000*xfade
    val mx100 = n0100*(1 - xfade) + n1100*xfade
    val mx010 = n0010*(1 - xfade) + n1010*xfade
    val mx110 = n0110*(1 - xfade) + n1110*xfade
    val mx001 = n0001*(1 - xfade) + n1001*xfade
    val mx101 = n0101*(1 - xfade) + n1101*xfade
    val mx011 = n0011*(1 - xfade) + n1011*xfade
    val mx111 = n0111*(1 - xfade) + n1111*xfade

    val yfade = fade(fy)
    val my00 = mx000*(1 - yfade) + mx100*yfade
    val my10 = mx010*(1 - yfade) + mx110*yfade
    val my01 = mx001*(1 - yfade) + mx101*yfade
    val my11 = mx011*(1 - yfade) + mx111*yfade

    val zfade = fade(fz)
    val mz0 = my00*(1 - zfade) + my10*zfade
    val mz1 = my01*(1 - zfade) + my11*zfade

    val wfade = fade(fw)
    (mz0*(1 - wfade) + mz1*wfade)*1.2// 1.2 is a guess
  }
}


object ClassicalGradientNoise extends ClassicalGradientNoise(0)
