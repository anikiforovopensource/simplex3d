/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package graphics

import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data.extension._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.util._


abstract class Texture[A <: Accessor with AnyVec[Double]] private[engine] (
  @transient protected val accessible: ReadData[A] with DirectSrc with ContiguousSrc,
  protected val linked: DirectSrc with ContiguousSrc
)(
  private var _magFilter: ImageFilter.Value, private var _minFilter: ImageFilter.Value,
  private var _mipMapFilter: MipMapFilter.Value, initAnisotropyLevel: Double
) extends EngineInfo {
  
  final class Subtext private[engine] () {
    private[engine] var dataChanges = true
    private[engine] var filterChanges = true
    
    def hasFilterChanges = filterChanges
    
    def hasDataChanges = subtext.dataChanges
    def clearDataChanges() { dataChanges = false }
    def clearFilterChanges() { filterChanges = false }
  }
  private[engine] val subtext = new Subtext
  
  
  {
    var count = 0
    if (accessible != null) count += 1
    if (linked != null) count += 1
    
    require(count == 1, "Data source must not be null.")
  }

  
  def isAccessible = (accessible != null)
  def isWritable = (isAccessible && !accessible.isReadOnly)
  
  def read: ReadData[A] with DirectSrc with ContiguousSrc = {
    if (isAccessible) accessible.asReadOnly().asInstanceOf[ReadData[A] with DirectSrc with ContiguousSrc]
    else null
  }
  
  def write: Data[A] with DirectSrc with ContiguousSrc = {
    if (isWritable) {
      subtext.dataChanges = true
      accessible.asInstanceOf[Data[A] with DirectSrc with ContiguousSrc]
    }
    else null
  }
  
  def src: DirectSrc with ContiguousSrc = if (isAccessible) accessible else linked
  
  
  def magFilter = _magFilter
  def magFilter_=(filter: ImageFilter.Value) { subtext.filterChanges = true; _magFilter = filter }
  
  def minFilter = _minFilter
  def minFilter_=(filter: ImageFilter.Value) { subtext.filterChanges = true; _minFilter = filter }
  
  def mipMapFilter = _mipMapFilter
  def mipMapFilter_=(filter: MipMapFilter.Value) { subtext.filterChanges = true; _mipMapFilter = filter }
  
  private var _anisotropyLevel: Double = 0
  def anisotropyLevel = _anisotropyLevel
  def anisotropyLevel_=(level: Double) { subtext.filterChanges = true; _anisotropyLevel = max(1, level) }
  anisotropyLevel = initAnisotropyLevel
}


object MipMapFilter extends Enumeration {
  val Disabled, Nearest, Linear = Value
}
object ImageFilter extends Enumeration {
  val Nearest, Linear = Value
}


class Texture2d[A <: Accessor with AnyVec[Double]] private (
  final val dimensions: ConstVec2i,
  accessible: ReadData[A] with DirectSrc with ContiguousSrc,
  linked: DirectSrc with ContiguousSrc
)(
  magFilter: ImageFilter.Value, minFilter: ImageFilter.Value,
  mipMapFilter: MipMapFilter.Value, anisotropyLevel: Double
) extends Texture[A](
  accessible, linked
)(magFilter, minFilter, mipMapFilter, anisotropyLevel) {
  
  if (accessible != null) {
    if (dimensions.x < 0 || dimensions.x < 0) throw new IllegalArgumentException(
      "Dimensions = " + dimensions + " contain negative components."
    )
    if (accessible.size != dimensions.x*dimensions.y) throw new IllegalArgumentException(
      "Texture dimensions do not match data size."
    )
  }
  
  /** Fill the texture with pixels obtained from the function.
   * 
   * @param function (dimensions, pixelCoordinates) => pixelValue
   */
  def fillWith(function: inVec2 => A#Read) :this.type = {
    val data = this.write
    
    var y = 0; while (y < dimensions.y) {
      renderLine(data, function, y)
      
      y += 1
    }
    
    this
  }
  private[this] val renderLine = (data: Data[A], function: inVec2 => A#Read, y: Int) => {
    val pixel = Vec2(0, y)

    var x = 0; while (x < dimensions.x) { val i = x + y*dimensions.x
      
      pixel.x = x
      data(i) = function(pixel)
      
      x += 1
    }
  }
}


object Texture2d {
  
  def apply[F <: Format { type Component = RDouble; type Accessor <: simplex3d.data.Accessor with AnyVec[Double] }](
    dimensions: ConstVec2i,
    magFilter: ImageFilter.Value = ImageFilter.Linear, minFilter: ImageFilter.Value = ImageFilter.Linear,
    mipMapFilter: MipMapFilter.Value = MipMapFilter.Linear, anisotropyLevel: Double = 4
  )(implicit composition: CompositionFactory[F, _ >: UByte])
  :Texture2d[F#Accessor] = {
    val primitive = implicitly[PrimitiveFactory[RDouble, UByte]]
    val data = composition.mkDataBuffer(primitive.mkDataBuffer(dimensions.x*dimensions.y*composition.components))
    new Texture2d[F#Accessor](dimensions, data, null)(
      magFilter, minFilter, mipMapFilter, anisotropyLevel
    )
  }
  
  def fromData[A <: Accessor with AnyVec[Double]](
    dimensions: ConstVec2i, data: ReadData[A] with DirectSrc with ContiguousSrc,
    magFilter: ImageFilter.Value = ImageFilter.Linear, minFilter: ImageFilter.Value = ImageFilter.Linear,
    mipMapFilter: MipMapFilter.Value = MipMapFilter.Linear, anisotropyLevel: Double = 4
  )
  :Texture2d[A] = {
    new Texture2d(dimensions, data, null)(
      magFilter, minFilter, mipMapFilter, anisotropyLevel
    )
  }

  def fromUncheckedSrc[A <: Accessor with AnyVec[Double]](
    dimensions: ConstVec2i, src: DirectSrc with ContiguousSrc,
    magFilter: ImageFilter.Value = ImageFilter.Linear, minFilter: ImageFilter.Value = ImageFilter.Linear,
    mipMapFilter: MipMapFilter.Value = MipMapFilter.Linear, anisotropyLevel: Double = 4
  )(implicit accessorManifest: ClassManifest[A]) :Texture2d[A] = {
    
    require(src.accessorManifest == accessorManifest, "Data accessor type doest not match manifest.")
    
    if (src.isInstanceOf[Data[_]]) {
      fromData(
        dimensions, src.asInstanceOf[Data[A] with DirectSrc with ContiguousSrc],
        magFilter, minFilter, mipMapFilter, anisotropyLevel
      )
    }
    else {
      new Texture2d(dimensions, null, src)(
        magFilter, minFilter, mipMapFilter, anisotropyLevel
      )
    }
  }
}
