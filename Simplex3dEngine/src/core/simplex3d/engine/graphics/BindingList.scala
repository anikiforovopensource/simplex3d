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

import java.lang.Integer
import java.util.HashMap
import scala.reflect._
import scala.collection.mutable.ArrayBuffer
import simplex3d.math.types._
import simplex3d.math.double.functions._
import simplex3d.engine.util._


sealed abstract class BindingSeq[T <: Accessible with Binding { type Read >: T <: Protected } ](
  implicit val elementTag: ClassTag[T]
)
extends Protected with Binding with PropertyContextDependent
{
  if(elementTag.runtimeClass == classOf[BindingSeq[_]]) throw new IllegalArgumentException(
    "Nested sequences are not supported."
  )
  
  
  // *** Prop *********************************************************************************************************
  
  protected var context: PropertyContext = _
  
  private[engine] override def register(context: PropertyContext) {
    this.context = context
    if (manageElems) registerElems(0, buff.size)
  }
  
  private[engine] override def unregister() {
    if (manageElems) unregisterElems(0, buff.size)
    context = null
  }
  
  protected def registerPropertyContext(context: PropertyContext) {}
  protected def unregisterPropertyContext() {}
  
  
  protected val managable = classOf[PropertyContextDependent].isAssignableFrom(elementTag.runtimeClass)
  protected def manageElems = (context != null && managable)
  
  protected def registerElems(offset: Int, count: Int) {
    var i = offset; while (i < offset + count) {
      buff(i).asInstanceOf[PropertyContextDependent].register(context)
      i += 1
    }
  }
  protected def unregisterElems(offset: Int, count: Int) {
    var i = offset; while (i < offset + count) {
      buff(i).asInstanceOf[PropertyContextDependent].unregister()
      i += 1
    }
  }
  
  
  // *** Seq **********************************************************************************************************
  
  type Read = BindingSeq[T]
  
  private[graphics] val buff = new ArrayBuffer[T]
  def size = buff.size
  final def length: Int = size
  
  type Elem <: T#Read
  final def apply(i: Int) :Elem = buff(i).asInstanceOf[Elem]
  
  
  // TODO Multidimensional arrays can be supported by having multidimensional size key.
  // Then declare eg [BindingList[BindingList[X]]]("name").size(expr1, expr2). And def size(expr: String*).
  final def collectKeys(path: String, nameKey: ListNameKey, lists: HashMap[ListNameKey, Integer], enums: HashMap[String, Object]) {
    val size = this.size
    val existing = lists.get(nameKey)
    
    if (existing == null || size < existing) lists.put(nameKey, size) //XXX warn on existing and non-equal size
    
    if (size > 0) {
      if (apply(0).isInstanceOf[Struct]) {
        var j = 0; while (j < size) {
          apply(j).asInstanceOf[Struct].collectKeys(path + "[" + j + "]", lists, enums)
          j += 1
        }
      }
      else if (apply(0).isInstanceOf[EnumRef[_]]) {
        var j = 0; while (j < size) {
          apply(j).asInstanceOf[EnumRef[_]].collectKeys(path + "[" + j + "]", enums)
          j += 1
        }
      }
    }
  }
  
  final def samplerRemapping(path: String, remapping: HashMap[String, String]) {
    if (path.contains("[*]")) return // nested arrays cannot be re-mapped.
    
    if (size > 0) {
      apply(0).asInstanceOf[AnyRef] match {
        case s: Struct => s.samplerRemapping(path + "[*]", remapping)
        case t: TextureBinding[_] => t.samplerRemapping(path + "[*]", remapping)
        case _ => // do nothing
      }
    }
  }
}


final class BindingList[T <: Accessible with Binding { type Read >: T <: Protected } ](
  implicit elementTag: ClassTag[T]
)
extends BindingSeq[T] with Accessible
{
  type Mutable = BindingList[T]
  type Elem = T
  
  final def mutableCopy(): BindingList[T] = {
    val copy = new BindingList[T]()(elementTag)
    copy := this
    copy
  }
  
  def :=(r: BindingSeq[T]) {
    this := r.buff.asInstanceOf[ArrayBuffer[T#Read]]
  }
  def :=(seq: Seq[T#Read]) {
    val s = size; var i = 0; for (e <- seq) {
      if (i < s) {
        val stable = buff(i)
        stable := e.asInstanceOf[stable.Read]
      }
      else {
        val mc = e.mutableCopy().asInstanceOf[T]
        if (manageElems) mc.asInstanceOf[PropertyContextDependent].register(context)
        buff += mc
      }
      i += 1
    }
    
    if (i < s) {
      if (manageElems) unregisterElems(i, s - i)
      buff.remove(i, s - i)
    }
    
    if (context != null && i != s) context.signalStructuralChanges()
  }
  
  
  def +=(elem: T#Read) {
    val mc = elem.mutableCopy().asInstanceOf[T]
    if (manageElems) mc.asInstanceOf[PropertyContextDependent].register(context)
    buff += mc
    if (context != null) context.signalStructuralChanges()
  }
  
  def ++=(r: BindingSeq[T]) {
    this ++= r.buff.asInstanceOf[ArrayBuffer[T#Read]]
  }
  def ++=(seq: Seq[T#Read]) {
    val size0 = buff.size
    buff ++= seq.map(_.mutableCopy().asInstanceOf[T])
    if (manageElems) registerElems(size0, buff.size - size0)
    if (context != null) context.signalStructuralChanges()
  }
  
  def insert(index: Int, elems: T#Read*) {
    this.insertAll(index, elems)
  }
  
  def insertAll(index: Int, r: BindingSeq[T]) {
    this.insertAll(index, r.buff.asInstanceOf[ArrayBuffer[T#Read]])
  }
  def insertAll(index: Int, seq: Seq[T#Read]) {
    val size0 = buff.size
    buff.insertAll(index, seq.map(_.mutableCopy().asInstanceOf[T]))
    if (manageElems) registerElems(index, buff.size - size0)
    if (context != null) context.signalStructuralChanges()
  }
  
  def remove(index: Int) {
    remove(index, 1)
  }
  def remove(index: Int, count: Int) {
    if (index < 0 || index > buff.size) throw new IndexOutOfBoundsException()
    if (index + count > buff.size) throw new IndexOutOfBoundsException()
    
    if (manageElems) unregisterElems(index, count)
    buff.remove(index, count)
    if (context != null) context.signalStructuralChanges()
  }
  
  def take(count: Int) { remove(count, size - count) }
  def takeRigth(count: Int) { remove(0, size - count) }
  def drop(count: Int) { remove(0, count) }
  def dropRight(count: Int) { remove(size - count, count) }
  
  
  def clear() {
    remove(0, buff.size)
  }
  
  override def toString() :String = {
    "BindingList(" + buff.mkString(", ") + ")"
  }
}


object BindingList {
  def apply[T <: Accessible with Binding { type Read >: T <: Protected } ]
    (elems: T#Read*)
    (implicit elementTag: ClassTag[T])
  :BindingList[T] =
  {
    val list = new BindingList[T]
    list ++= elems
    list
  }
}


final class BindingArray[T <: Accessible with Binding { type Read >: T <: Protected } ] private()(
  implicit elementTag: ClassTag[T]
)
extends BindingSeq[T] with Accessible
{
  def this(size: Int)(implicit elementTag: ClassTag[T]) {
    this()
    
    var i = 0; while (i < size) {
      buff += elementTag.runtimeClass.newInstance().asInstanceOf[T]
      
      i += 1
    }
    
    if (manageElems) registerElems(0, size)
  }
  
  type Mutable = BindingArray[T]
  type Elem = T
  
  final def mutableCopy(): BindingArray[T] = {
    val copy = new BindingArray[T]()(elementTag)
    copy := this
    copy
  }
  
  def :=(r: BindingSeq[T]) {
    set(r.buff.asInstanceOf[ArrayBuffer[T#Read]], r.size)
  }
  def :=(seq: Seq[T#Read]) {
    set(seq, seq.size)
  }
  private def set(src: Seq[T#Read], srcSize: Int) {
    if (srcSize != size) throw new IllegalArgumentException("Source size must match the destination size.")
    
    val s = size; var i = 0; for (e <- src) {
      val stable = buff(i)
      stable := e.asInstanceOf[stable.Read]
      
      i += 1
    }
  }
  
  override def toString() :String = {
    "BindingArray(" + buff.mkString(", ") + ")"
  }
}


object BindingArray {
  def apply[T <: Accessible with Binding { type Read >: T <: Protected } ]
    (elems: T#Read*)
    (implicit elementTag: ClassTag[T])
  :BindingArray[T] =
  {
    val size = elems.size
    val array = new BindingArray[T](size)
    array.set(elems, size)
    array
  }
}
