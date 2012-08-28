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

package simplex3d.engine.util

import simplex3d.math.types._
import simplex3d.engine.graphics._


private[engine] object PathUtil {
  /* case NameIndex(name, index, rest) */
  private val NameIndexRest = """(\w+)\[(\d+)\]\.?(.*)""".r
  
  /* case NameRest(name, rest) */
  private val NameRest = """(\w+)\.?(.*)""".r
  
  /* case RestIndex(rest, index) */
  private val PrefixIndex = """(.+)\[(\d+)\]""".r
  
  /* case ListPathRest(listPath, rest) */
  private val ListPathRest = """(\w+)\[\*\]\.?(.*)""".r
  
  /* case RestName(rest, name) */
  private val RestName = """(.*)\.(\w+)""".r
  
  /* case RestNameIndex(rest, name, index) */
  private val RestNameIndex = """(.*)\.(\w+)\[(\d+)\]""".r
  
  
  def find(names: ReadArray[String], name: String) :Int = {
    var i = 0; while (i < names.length) {
      if (names(i) == name) return i
      
      i += 1
    }
    
    -1
  }
  
  def rootName(path: String) :String = {
    path match {
      case NameRest(name, _) => name
      case _ => path
    }
  }
  
  def leafName(path: String) :String = {
    path match {
      case RestName(_, name) => name
      case RestNameIndex(_, name, _) => name
      case _ => path
    }
  }
  
  def resolve(path: String, bindingFromName: String => Binding) :Binding = {
    path match {
      
      case NameIndexRest(name, index, rest) =>
        val res = bindingFromName(name)
        if (res == null) null else res match {
          
          case list: BindingList[_] =>
            val id = index.toInt
            if (id >= list.size) null else {
              
              val indexed = list(id)
              if (rest.isEmpty) indexed
              else {
                indexed match {
                  
                  case s: Struct =>
                    s.resolvePath(rest)

                  /* Replace when 2.10 is out.
                  case t: ReadTextureBinding[_] if t.isBound && rest == "dimensions" =>
                    t.bound.dimensions
                    
                  case _ =>
                    null*/
                    
                  case _ =>
                    if (TextureBinding.avoidCompilerCrashB(indexed)) {
                      val t = TextureBinding.avoidCompilerCrash(indexed)
                      if (t.isBound && rest == "dimensions") t.bound.asInstanceOf[Texture[_]].bindingDimensions
                      else null
                    }
                    else null
                }
              }
            }
            
          case _ =>
            null
        }
        
      case NameRest(name, rest) =>
        val res = bindingFromName(name)
        if (res == null) null else if (rest.isEmpty) res else res match {
          
          case s: Struct =>
            s.resolvePath(rest)
            
          /* Replace when 2.10 is out.
          case t: ReadTextureBinding[_] if t.isBound && rest == "dimensions" =>
            t.bound.dimensions
            
          case _ =>
            null*/
            
          case _ =>
            if (TextureBinding.avoidCompilerCrashB(res)) {
              val t = TextureBinding.avoidCompilerCrash(res)
              if (t.isBound && rest == "dimensions") t.bound.asInstanceOf[Texture[_]].bindingDimensions
              else null
            }
            else null
        }
        
      case _ =>
        null
    }
  }
  
  
  def remap(path: String, pathRemapping: Map[String, String]) :String = {
    if (pathRemapping.isEmpty) return path
    
    var prefix: String = null
    var index: String = null
    if (path.charAt(path.length - 1) == ']') {
      path match {
        case PrefixIndex(r, i) => prefix = r; index = i
        case _ => // do nothing
      }
    }
    
    if (prefix != null) {
      val remapping = pathRemapping.get(prefix)
      if (remapping.isDefined) {
        val arrayRemapping = remapping.get
        arrayRemapping.replaceAll("""\*""", index)
      }
      else path
    }
    else {
      val remapping = pathRemapping.get(path)
      if (remapping.isDefined) remapping.get else path
    }
  }
  
  def remappedList(path: String) :(String, String) = {
    path match {
      case ListPathRest(listPath, rest) => (listPath, rest)
      case _ => throw new IllegalArgumentException("Must be a list path with a wildcard [*].")
    }
  }
  
  
  // XXX remove this
  def parentPathListNameXXX(path: String, pathRemapping: Map[String, String]) :(String, String) = {
    def extractParent(rest: String, name: String) =  rest match {
      case RestName(_, parentName) => (parentName, name)
      case RestNameIndex(_, parentName, _) => (parentName, name)
      case _ => (rest, name)
    }
    def extract(path: String) = path match {
      case RestName(rest, name) => extractParent(rest, name)
      case RestNameIndex(rest, name, _) => extractParent(rest, name)
      case _ => ("", path)
    }
    
    if (pathRemapping.isEmpty) return extract(path)
    
    val remapping = pathRemapping.get(path)
    if (!remapping.isDefined) extract(path) else {
      remapping.get match {
        case ListPathRest(listPath, rest) => extract(listPath)
        case _ => extract(path)
      }
    }
  }
}
