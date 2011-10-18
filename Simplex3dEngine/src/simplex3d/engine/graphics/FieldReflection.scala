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

import java.lang.Class
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.util.HashMap
import simplex3d.math.types._


object FieldReflection {
  private[this] val cache = new HashMap[(Class[_], Class[_]), (ReadArray[String], ReadArray[Method])]
  
  def isFinal(instance: AnyRef) = ((instance.getClass.getModifiers & Modifier.FINAL) != 0)
  

  def getAccessorMap(instance: AnyRef, target: Class[_], blacklist: Seq[String] = Nil)
  :(ReadArray[String], ReadArray[Method]) = {
    val clazz = instance.getClass
    
    val cached = cache.get((clazz, target))
    if (cached != null) {
      cached
    }
    else {
      val methods = clazz.getMethods.filter { method =>
        (method.getModifiers & Modifier.STATIC) == 0 &&
        method.getParameterTypes.length == 0 &&
        target.isAssignableFrom(method.getReturnType) &&
        !clazz.isAssignableFrom(method.getReturnType) &&
        !blacklist.contains(method.getName())
      }

      val accessors = (new ReadArray(methods.map(_.getName).toArray), new ReadArray(methods))
      cache.put((clazz, target), accessors)
      accessors
    }
  }
  
  def getValueMap[T <: AnyRef](instance: AnyRef, target: Class[T], blacklist: Seq[String] = Nil)
  :(ReadArray[String], ReadArray[T]) = {
    val (names, accessors) = getAccessorMap(instance, target, blacklist)
    
    val values = new Array[AnyRef](accessors.length).asInstanceOf[Array[T]]
    var i = 0; while (i < accessors.length) {
      values(i) = accessors(i).invoke(instance).asInstanceOf[T]
      i += 1
    }
    
    (names, new ReadArray(values))
  }
}
