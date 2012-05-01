/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2012, Aleksey Nikiforov
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


final class RemappedList[W <: Writable[W] with Binding] private(
  val suppliedManifest: ClassManifest[W],
  val rootList: BindingList[UncheckedBinding],
  val path: String
) {
  
  //XXX remove when 2.10 comes out.
  private def avoidCompilerCrash = rootList.asInstanceOf[BindingList[_]].elementManifest.erasure

  val elementManifest: ClassManifest[W] =
    if (path.isEmpty) {
      if (suppliedManifest != null) {
        require(
          suppliedManifest == rootList.elementManifest,
          "RemappedList manifest must match the wrapped list when remapping path is empty.")
          
        suppliedManifest
      }
      else {
        rootList.elementManifest.asInstanceOf[ClassManifest[W]]
      }
    }
    else if (!classOf[Struct[_]].isAssignableFrom(avoidCompilerCrash)) {
      throw new IllegalArgumentException("RemappedList must wrap a list of structs when remapping path is nonempty.")
    }
    else {
      val s = avoidCompilerCrash.newInstance().asInstanceOf[Struct[_]]//XXX use rootList(0) if available, construct proper
      val resolved = s.resolvePath(path)
      require(resolved != null, "RemappingList path cannot be resolved.")
      
      if (suppliedManifest != null) {
        require(
          suppliedManifest.erasure.isAssignableFrom(resolved.getClass),
          "RemappingList path must resolve to a value that matches the elementManifest.")
          
        suppliedManifest
      }
      else {
        ClassManifest.fromClass(resolved.getClass).asInstanceOf[ClassManifest[W]]
      }
    }
  
  
  private[this] var size0 = 0 
  def size: Int = size0
  def length: Int = size0
  
  
  private[this] def get(i: Int) :W = {
    val root = rootList(i)
    if (path.isEmpty) root.asInstanceOf[W]
    else root.asInstanceOf[Struct[_]].resolvePath(path).asInstanceOf[W]
  }
  
  def apply(i: Int) :W = {
    if (i < 0 || i >= size) throw new IndexOutOfBoundsException()
    get(i)
  }
  
  def +=(elem: W#Read) {
    val next = size0 + 1
    
    if (next >= rootList.size) {
      if (path.isEmpty) {
        rootList += elem.asInstanceOf[UncheckedBinding#Read]
      }
      else {
        rootList += avoidCompilerCrash.newInstance().asInstanceOf[UncheckedBinding#Read]
        get(next) := elem
      }
    }
    else {
      get(next) := elem
    }
    
    size0 = next
  }
  
  def ++=(r: RemappedList[W]) {
    var i = 0; while (i < r.size) {
      this += r(i)
      
      i += 1
    }
  }
  def ++=(seq: Seq[W#Read]) {
    for (elem <- seq) {
      this += elem
    }
  }
  
  private def toArray() :Array[W] = {
    val array = elementManifest.newArray(size)
    
    var i = 0; while (i < size) {
      array(i) = this(i)
      
      i += 1
    }
    
    array
  }
  
  override def toString() :String = {
    "RemappedList(" + toArray.mkString(", ") + ")"
  }
}


object RemappedList {
  def apply[W <: Writable[W] with Binding](rootList: BindingList[UncheckedBinding], path: String)
    (implicit elementManifest: ClassManifest[W])
  :RemappedList[W] = {
    new RemappedList(elementManifest, rootList, path)
  }
  
  def unchecked(rootList: BindingList[UncheckedBinding], path: String)
  :RemappedList[UncheckedBinding] = {
    new RemappedList(null, rootList, path)
  }
}
