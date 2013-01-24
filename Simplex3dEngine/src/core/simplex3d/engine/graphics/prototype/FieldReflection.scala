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
  private[this] val cache = new HashMap[ClassSymbol, (ReadArray[String], ReadArray[MethodSymbol])]
  
  
  private[this] def symbolMap[V: TypeTag](instanceMirror: InstanceMirror, filter: Set[String] = Set.empty[String])
  :(ReadArray[String], ReadArray[MethodSymbol]) =
  {
    def getSymbols() = {
      val tpe = instanceMirror.symbol.toType
      
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
    
    
    val cached = cache.get(instanceMirror.symbol)
    if (cached != null) cached
    else {
      cache.synchronized {
        val cached = cache.get(instanceMirror.symbol)
        if (cached != null) return cached
        
        getSymbols()
      }
    }
  }
  
  def valueMap[T, V](instance: T, valType: TypeTag[V], filter: Set[String] = Set.empty[String])
  :(ReadArray[String], ReadArray[V]) =
  {
    val instanceMirror = mirror.reflect(instance)(ClassTag(instance.getClass))
    
    val (names, methods) = symbolMap[V](instanceMirror, filter)(valType)
    val values = new Array[Any](methods.length)
    
    var i = 0; while (i < values.length) {
      values(i) = instanceMirror.reflectField(methods(i)).get
      
      i += 1
    }
    
    (names, new ReadArray(values).asInstanceOf[ReadArray[V]])
  }
}

object Test {
  import simplex3d.math._
  
  def main(args: Array[String]) {
    test()
  }
  
  def bench(name: String)(block: => String){
    System.gc()
    val start = System.currentTimeMillis
    
    println(block)
    
    System.gc()
    val total = System.currentTimeMillis - start
    
    println(name + ": " + total/1000.0)
  }
  
  class TestClass {
    val p01 = Property(() => new BooleanRef())
    val p02 = Property(() => new IntRef())
    val p03 = Property(() => Vec2i(0))
    val p04 = Property(() => Vec3i(0))
    val p05 = Property(() => Vec4i(0))
  }
  
  def test() {
    val ierations = 1000
    
    bench("Java Reflection") {
      var a = 0
      
      var i = 0; while (i < ierations) {
        val (names, props) = JavaReflection.valueMap(
            new TestClass, classOf[Property[_]], JavaReflection.BindingFilter, Nil)
            
        a += names.length
        
        i += 1
      }
      
      a.toString
    }
    
    
    System.exit(1)
    
    bench("Scala Reflection") {
      val valueTag = typeTag[Property[_ <: Binding]]
      
      var a = 0
      
      var i = 0; while (i < ierations) {
        val (names, props) = ScalaReflection.valueMap(new TestClass, valueTag)
        
        a += names.length
        
        i += 1
      }
      
      a.toString
    }
  }
}
