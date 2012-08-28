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
package prototype

import java.lang.Class
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.lang.reflect.ParameterizedType
import java.util.HashMap
import simplex3d.math.types._
import simplex3d.engine.util._


private[engine] object FieldReflection {
  
  private[engine] val BindingFilter = List(classOf[Protected], classOf[Binding])
  private[engine] val EnvironmentalEffectFilter = List(classOf[EnvironmentalEffect])
  
  
  private[this] val cache = new HashMap[Class[_], (ReadArray[String], ReadArray[Method])]
  
  def isFinal(instance: AnyRef) = ((instance.getClass.getModifiers & Modifier.FINAL) != 0)
  

  def getAccessorMap(
    instance: AnyRef,
    targetType: Class[_], upperBoundsForTargetTypeArgs: List[Class[_]],
    blacklist: Seq[String]
  )
  :(ReadArray[String], ReadArray[Method]) = {
    val clazz = instance.getClass
    
    val cached = cache.get(clazz)
    if (cached != null) {
      cached
    }
    else {
      val dupMethods = clazz.getMethods.filter { method =>
        (method.getModifiers & Modifier.STATIC) == 0 &&
        method.getParameterTypes.length == 0 &&
        targetType.isAssignableFrom(method.getReturnType) &&
        !clazz.isAssignableFrom(method.getReturnType) &&
        !blacklist.contains(method.getName) &&
        !method.getName.contains('$') && {
          if (upperBoundsForTargetTypeArgs.isEmpty) true
          else {
            method.getGenericReturnType match {
              case p: ParameterizedType =>
                val typeArgument = p.getActualTypeArguments()(0) match {
                  case c: Class[_] => c
                  case p: ParameterizedType => p.getRawType match {
                    case c: Class[_] => c
                    case _ => null
                  }
                }
                if (typeArgument == null) false
                else upperBoundsForTargetTypeArgs.forall(_.isAssignableFrom(typeArgument))
              case _ =>
                false
            }
          }
        }
      }
      
      val methodMap = new HashMap[String, Method]
      for (method <- dupMethods) {
        val existing = methodMap.get(method.getName)
        if (existing == null || method.getDeclaringClass == clazz) {
          methodMap.put(method.getName, method)
        }
      }
      val methods = methodMap.values().toArray(new Array[Method](0))

      val accessors = (new ReadArray(methods.map(_.getName).toArray), new ReadArray(methods))
      cache.put(clazz, accessors)
      accessors
    }
  }
  
  def getValueMap[T <: AnyRef](
    instance: AnyRef,
    targetType: Class[T], upperBoundsForTargetTypeArgs: List[Class[_]],
    blacklist: Seq[String]
  )
  :(ReadArray[String], ReadArray[T]) = {
    val (names, accessors) = getAccessorMap(instance, targetType, upperBoundsForTargetTypeArgs, blacklist)
    
    val values = new Array[AnyRef](accessors.length).asInstanceOf[Array[T]]
    var i = 0; while (i < accessors.length) {
      values(i) = accessors(i).invoke(instance).asInstanceOf[T]
      i += 1
    }
    
    (names, new ReadArray(values))
  }
}
