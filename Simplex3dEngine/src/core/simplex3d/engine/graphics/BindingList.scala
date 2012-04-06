/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011-2012, Aleksey Nikiforov
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

import scala.collection.mutable.ArrayBuffer
import simplex3d.math.types._
import simplex3d.math.double.functions._
import simplex3d.engine.util._


sealed abstract class ReadBindingList[W <: Writable[W] with Binding](
  implicit
  val elementManifest: ClassManifest[W],
  private[this] val structuralChangeListener: StructuralChangeListener
)
extends Readable[BindingList[W]] with Binding
{
  def size: Int
  final def length: Int = size
  
  def apply(i: Int) :W#Read
}


final class BindingList[W <: Writable[W] with Binding](
  implicit
  elementManifest: ClassManifest[W],
  structuralChangeListener: StructuralChangeListener
)
extends ReadBindingList[W] with Writable[BindingList[W]]
{
  type Read = ReadBindingList[W]
  
  private val buff = new ArrayBuffer[W]
  def size = buff.size
  
  final def mutableCopy(): BindingList[W] = {
    val copy = new BindingList[W]()(elementManifest, null)
    copy := this
    copy
  }
  
  def :=(r: Readable[BindingList[W]]) {
    this := r.asInstanceOf[BindingList[W]].buff
  }
  def :=(seq: Seq[W#Read]) {
    val s = size; var i = 0; for (e <- seq) {
      if (i < s) buff(i) := e
      else buff += e.mutableCopy()
      i += 1
    }
    if (i < s) buff.remove(i, s - i)
    
    if (structuralChangeListener != null && i != s) structuralChangeListener.signalStructuralChanges()
  }
  
  def apply(i: Int) :W = buff(i)
  
  def +=(elem: W#Read) {
    buff += elem.mutableCopy()
    if (structuralChangeListener != null) structuralChangeListener.signalStructuralChanges()
  }
  
  def ++=(r: Readable[BindingList[W]]) {
    this ++= r.asInstanceOf[BindingList[W]].buff
  }
  def ++=(seq: Seq[W#Read]) {
    buff ++= seq.map(_.mutableCopy())
    if (structuralChangeListener != null) structuralChangeListener.signalStructuralChanges()
  }
  
  def insert(index: Int, elems: W#Read*) {
    buff.insert(index, elems.map(_.mutableCopy()): _*)
    if (structuralChangeListener != null) structuralChangeListener.signalStructuralChanges()
  }
  
  def insertAll(index: Int, r: Readable[BindingList[W]]) {
    this.insertAll(index, r.asInstanceOf[BindingList[W]].buff)
  }
  def insertAll(index: Int, seq: Seq[W#Read]) {
    buff.insertAll(index, seq.map(_.mutableCopy()))
    if (structuralChangeListener != null) structuralChangeListener.signalStructuralChanges()
  }
  
  def remove(index: Int) {
    buff.remove(index)
    if (structuralChangeListener != null) structuralChangeListener.signalStructuralChanges()
  }
  def remove(index: Int, count: Int) {
    buff.remove(index, count)
    if (structuralChangeListener != null) structuralChangeListener.signalStructuralChanges()
  }
  
  def clear() {
    buff.clear()
    if (structuralChangeListener != null) structuralChangeListener.signalStructuralChanges()
  }
  
  override def toString() :String = {
    "BindingList(" + buff.mkString(", ") + ")"
  }
}


object BindingList {
  def apply[W <: Writable[W] with Binding]
    (elems: W#Read*)
    (implicit elementManifest: ClassManifest[W], structuralChangeListener: StructuralChangeListener)
  :BindingList[W] =
  {
    val list = new BindingList[W]
    list ++= elems
    list
  }
}
