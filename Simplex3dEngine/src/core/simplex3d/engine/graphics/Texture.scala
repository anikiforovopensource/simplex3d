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

import scala.reflect._
import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data.extension._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.util._


trait Concrete

abstract class Texture[A <: Accessor] private[engine] (
  @transient protected val accessible: ReadData[A] with DirectSrc with ContiguousSrc,
  protected val linked: DirectSrc with ContiguousSrc
)
extends EngineInfoRef {
  
  type Accessor = A with simplex3d.data.Accessor
  
  def bindingDimensions: AnyVec[Int]//XXX possibly simplify to dimensions?
  
  protected var dataChanges = true
    
  private[engine] def hasDataChanges = dataChanges
  private[engine] def clearDataChanges() { dataChanges = false }
  
  
  {
    var count = 0
    if (accessible != null) count += 1
    if (linked != null) count += 1
    
    if (count != 1) throw new IllegalArgumentException("Data source must not be null.")
  }

  
  def isAccessible = (accessible != null)
  def isWritable = (isAccessible && !accessible.isReadOnly)
  
  def read: ReadData[A] with DirectSrc with ContiguousSrc = {
    if (isAccessible) accessible.asReadOnly().asInstanceOf[ReadData[A] with DirectSrc with ContiguousSrc]
    else throw new IllegalAccessException("Texture data is not accessible.")
  }
  
  def write: Data[A] with DirectSrc with ContiguousSrc = {
    if (isWritable) {
      dataChanges = true
      accessible.asInstanceOf[Data[A] with DirectSrc with ContiguousSrc]
    }
    else throw new IllegalAccessException("Texture data is not writable.")
  }
  
  def src: DirectSrc with ContiguousSrc = if (isAccessible) accessible else linked
  
  
  // *** Parameters ***************************************************************************************************
  protected var parameterChanges = true
  private[engine] def hasParameterChanges = parameterChanges//XXX bridge via AnyVal proxy
  private[engine] def clearParameterChanges() { parameterChanges = false }//XXX bridge via AnyVal proxy
  
  private var _magFilter: ImageFilter.Value = ImageFilter.Linear
  def magFilter = _magFilter
  def magFilter_=(filter: ImageFilter.Value) { parameterChanges = true; _magFilter = filter }
  
  private var _minFilter: ImageFilter.Value = ImageFilter.Linear
  def minFilter = _minFilter
  def minFilter_=(filter: ImageFilter.Value) { parameterChanges = true; _minFilter = filter }
  
  private var _mipMapFilter: MipMapFilter.Value = MipMapFilter.Linear
  def mipMapFilter = _mipMapFilter
  def mipMapFilter_=(filter: MipMapFilter.Value) { parameterChanges = true; _mipMapFilter = filter }
  
  private var _anisotropyLevel: Double = 4
  def anisotropyLevel = _anisotropyLevel
  def anisotropyLevel_=(level: Double) { parameterChanges = true; _anisotropyLevel = max(1, level) }
  
  private val _borderColor = Vec4(0)
  def borderColor: ReadVec4 = _borderColor
  def borderColor_=(color: inVec4) { parameterChanges = true; _borderColor := color }
}


object MipMapFilter extends Enumeration {
  val Disabled, Nearest, Linear = Value
}
object ImageFilter extends Enumeration {
  val Nearest, Linear = Value
}

object TextureWrap extends Enumeration {
  val ClampToEdge, ClampToBorder, MirrorRepeat, Repeat = Value
}


class Texture2d[A <: Accessor] private (
  final val dimensions: ConstVec2i,
  accessible: ReadData[A] with DirectSrc with ContiguousSrc,
  linked: DirectSrc with ContiguousSrc
)
extends Texture[A](accessible, linked) with Concrete
{
  
  if (accessible != null) {
    if (dimensions.x < 0 || dimensions.x < 0) throw new IllegalArgumentException(
      "Dimensions = " + dimensions + " contain negative components."
    )
    if (accessible.size != dimensions.x*dimensions.y) throw new IllegalArgumentException(
      "Texture dimensions do not match data size."
    )
  }
  
  
  final def bindingDimensions = dimensions
  
  
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
  private[this] final def renderLine = (data: Data[A], function: inVec2 => A#Read, y: Int) => {
    val pixel = Vec2(0, y)

    var x = 0; while (x < dimensions.x) { val i = x + y*dimensions.x
      
      pixel.x = x
      data(i) = function(pixel)
      
      x += 1
    }
  }
  
  
  private var _wrapS: TextureWrap.Value = TextureWrap.Repeat
  def wrapS = _wrapS
  def wrapS_=(wrapValue: TextureWrap.Value) { parameterChanges = true; _wrapS = wrapValue }
  
  private var _wrapT: TextureWrap.Value = TextureWrap.Repeat
  def wrapT = _wrapT
  def wrapT_=(wrapValue: TextureWrap.Value) { parameterChanges = true; _wrapT = wrapValue }
}


object Texture2d {
  val Tag = classTag[Texture2d[_]]
  
  def apply[F <: Format { type Component = RDouble; type Accessor <: simplex3d.data.Accessor }](
    dimensions: ConstVec2i
  )(implicit
    composition: CompositionFactory[F, _ >: UByte]
  )
  :Texture2d[F#Accessor] =
  {
    val primitive = implicitly[PrimitiveFactory[RDouble, UByte]]
    val data = composition.mkDataBuffer(primitive.mkDataBuffer(dimensions.x*dimensions.y*composition.components))
    new Texture2d[F#Accessor](dimensions, data, null)
  }
  
  def fromData[A <: Accessor](
    dimensions: ConstVec2i, data: ReadData[A] with DirectSrc with ContiguousSrc
  )
  :Texture2d[A] = {
    new Texture2d(dimensions, data, null)
  }

  def fromUncheckedSrc[A <: Accessor](
    dimensions: ConstVec2i, src: DirectSrc with ContiguousSrc
  )(implicit accessorTag: ClassTag[A]) :Texture2d[A] = {
    
    if (src.accessorTag != accessorTag) throw new IllegalArgumentException(
      "Data accessor type doest not match the tag.")
    
    if (src.isInstanceOf[Data[_]]) {
      fromData(dimensions, src.asInstanceOf[Data[A] with DirectSrc with ContiguousSrc])
    }
    else {
      new Texture2d(dimensions, null, src)
    }
  }
}
