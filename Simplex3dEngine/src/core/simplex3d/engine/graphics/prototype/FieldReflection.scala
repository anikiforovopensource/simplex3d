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


private[engine] object JavaReflection {
  
  private[engine] val BindingFilter = List(classOf[Protected], classOf[Binding])
  private[engine] val EnvironmentalEffectFilter = List(classOf[EnvironmentalEffect])
  
  
  private[this] val cache = new HashMap[Class[_], (ReadArray[String], ReadArray[Method])]
  
  def isFinal(instance: AnyRef) = ((instance.getClass.getModifiers & Modifier.FINAL) != 0)
  

  private def accessorMap(
    instance: AnyRef,
    targetFieldType: Class[_], fieldTypeArgumentBounds: List[Class[_]],
    blacklist: Seq[String]
  )
  :(ReadArray[String], ReadArray[Method]) = {
    val clazz = instance.getClass
    
    val cached = cache.get(clazz)
    if (cached != null) cached
    else {
      cache.synchronized {
        val cached = cache.get(clazz)
        if (cached != null) return cached
        
        val dupMethods = clazz.getMethods.filter { method =>
          /* not static                  */ (method.getModifiers & Modifier.STATIC) == 0 &&
          /* no parameters               */ method.getParameterTypes.length == 0 &&
          /* target return type          */ targetFieldType.isAssignableFrom(method.getReturnType) &&
          /* return type is distinct     */ !clazz.isAssignableFrom(method.getReturnType) &&
          /* not blacklisted             */ !blacklist.contains(method.getName) &&
          /* not synthetic               */ !method.getName.contains('$') &&
          /* return type argument bounds */ {
            if (fieldTypeArgumentBounds.isEmpty) true
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
                  else fieldTypeArgumentBounds.forall(_.isAssignableFrom(typeArgument))
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
  }
  
  def valueMap[T <: AnyRef](
    instance: AnyRef,
    targetType: Class[T], upperBoundsForTargetTypeArgs: List[Class[_]],
    blacklist: Seq[String]
  )
  :(ReadArray[String], ReadArray[T]) = {
    val (names, accessors) = accessorMap(instance, targetType, upperBoundsForTargetTypeArgs, blacklist)
    
    val values = new Array[AnyRef](accessors.length).asInstanceOf[Array[T]]
    var i = 0; while (i < accessors.length) {
      values(i) = accessors(i).invoke(instance).asInstanceOf[T]
      i += 1
    }
    
    (names, new ReadArray(values))
  }
}


import scala.reflect._
import scala.reflect.runtime.universe._

private[engine] object ScalaReflection {
  
  private[engine] val GeometryFilter = typeTag[Attributes[_, _]]
  private[engine] val MaterialFilter = typeTag[Property[_]]
  private[engine] val EnvironmentlFilter = typeTag[Property[_ <: EnvironmentalEffect]]
  private[engine] val StructFilter = typeTag[Accessible]
  private[engine] val ShaderValFilter = typeTag[Value[_ <: Binding]]
  
  
  private[this] val mirror = runtimeMirror(this.getClass.getClassLoader)
  private[this] val cache = new HashMap[Type, (ReadArray[String], ReadArray[MethodSymbol])]
  
  
  private[this] def symbolMap[V: TypeTag](tpe: Type, filter: Set[String] = Set.empty[String])
  :(ReadArray[String], ReadArray[MethodSymbol]) =
  {
    def getSymbols() = {
      val vals = tpe.members.filter { a => a.isMethod && {
        val m = a.asMethod
        m.isGetter && !m.isSynthetic && !m.isImplementationArtifact &&
        (m.isPublic || m.privateWithin != NoSymbol) &&
        m.returnType <:< typeOf[V] &&
        !filter.contains(m.name.decoded)
      }}
      
      val (names, methods) = vals.map(v => (v.name.decoded, v.asMethod)).unzip
      (new ReadArray(names.toArray), new ReadArray(methods.toArray))
    }
    
    
    val cached = cache.get(tpe)
    if (cached != null) cached
    else {
      cache.synchronized {
        val cached = cache.get(tpe)
        if (cached != null) return cached
        
        getSymbols()
      }
    }
  }
  
  def valueMap[T, V](instance: T, valType: TypeTag[V], filter: Set[String] = Set.empty[String])
  :(ReadArray[String], ReadArray[V]) =
  {
    val instanceMirror = mirror.reflect(instance)(ClassTag(instance.getClass))
    val tpe = instanceMirror.symbol.toType
    val (names, methods) = symbolMap[V](tpe, filter)(valType)
    val values = new Array[Any](methods.length)
    
    var i = 0; while (i < values.length) {
      values(i) = instanceMirror.reflectField(methods(i)).get
      
      i += 1
    }
    
    (names, new ReadArray(values).asInstanceOf[ReadArray[V]])
  }
}

package foo.bar {
  class Test {
    val v: String = ""
    val a = Property(() => simplex3d.math.Vec2i(0)); a.update
    def d()  = 1
    def n: String = ""
    
    private[engine] val vp: Int = 1
    protected val vh: String = ""
  }
}

object Test {
  def main(args: Array[String]) {
    val (names, props) = ScalaReflection.valueMap(new foo.bar.Test, ScalaReflection.MaterialFilter)
    println(names(0), props(0).asInstanceOf[AnyRef])
  }
}
