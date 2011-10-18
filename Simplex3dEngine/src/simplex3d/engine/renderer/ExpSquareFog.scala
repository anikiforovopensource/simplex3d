/*
 * Simplex3dEngine - Renderer Module
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
package renderer

import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine.graphics._


sealed abstract class ReadExpSquareFog extends EnvironmentalEffect[ReadExpSquareFog]
with ReflectCompoundType[ReadExpSquareFog] {
  type Mutable = ExpSquareFog
  
  def color: ReadVec3
  def density: ReadDoubleRef
  
  def mutableCopy() = {
    val fog = new ExpSquareFog
    
    fog.color := color
    fog.density := density
    
    fog
  }
  
  def propagate(parentVal: ReadExpSquareFog, result: ExpSquareFog) :Boolean = {
    val parent = parentVal.asInstanceOf[ReadExpSquareFog]
    val res = result.asInstanceOf[ExpSquareFog]
    
    val densitySum = parent.density + this.density
    res.color := mix(parent.color, this.color, parent.density/densitySum)
    res.density := densitySum
    
    false
  }
  
  override def equals(other: Any) :Boolean = {
    other match {
      case f: ReadExpSquareFog =>
        color == f.color &&
        density == f.density
      case _ => false
    }
  }
  
  def resolveBinding() = this
  def updateBinding(predefinedUniforms: ReadPredefinedUniforms) {}
}


final class ExpSquareFog extends ReadExpSquareFog with Writable[ReadExpSquareFog] {
  val color = Vec3(1)
  val density = new DoubleRef(0)
  
  reflect(classOf[ExpSquareFog])
  
  
  def :=(r: ReadExpSquareFog) {
    color := r.color
    density := r.density
  }
}


object ExpSquareFog {
  val FieldNames = new ReadArray(Array[String]("color", "density"))
  val Default: ReadExpSquareFog = new ExpSquareFog
}
