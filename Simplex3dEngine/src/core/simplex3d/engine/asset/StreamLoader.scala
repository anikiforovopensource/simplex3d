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

package simplex3d.engine.asset

import java.lang.Integer
import javax.imageio.ImageIO
import java.io._
import scala.collection.mutable.ArrayBuilder
import java.util.HashMap
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._


trait StreamLoader extends AssetLoader {
  
  protected def openStream(path: String) :InputStream
  
  protected def load[T](path: String)(loadStream: InputStream => T) :Option[T] = {
    var stream: InputStream = null
    try {
      stream = openStream(path)
      if (stream == null) {
        println(path + " not found")//XXX log
        None 
      }
      else  Some(loadStream(stream))
    }
    catch {
      case ioe: IOException =>
        ioe.printStackTrace()//XXX log
        None
      case e: Exception =>
        e.printStackTrace()//XXX log
        None
    }
    finally {
      if (stream != null) stream.close()
    }
  }
  
  def loadRgbImg(path: String) :Option[(ConstVec2i, RgbTextureData)] = {
    load(path) { stream =>
      val img = ImageIO.read(stream)
      
      val width = img.getWidth
      val height = img.getHeight
      
      val data = DataBuffer[Vec3, UByte](width*height)
      val buffer = data.buffer()
      
      var y = 0; while (y < height) {
        var x = 0; while (x < width) {
          
          val pixel = img.getRGB(x, y)
          val index = (x + (height - 1 - y)*width)*3
          buffer.put(index, (pixel >> 16).toByte)
          buffer.put(index + 1, (pixel >> 8).toByte)
          buffer.put(index + 2, pixel.toByte)
          
          x += 1
        }
        y += 1
      }
      
      (ConstVec2i(width, height), data)
    }
  }
  
  def loadRgbaImg(path: String) :Option[(ConstVec2i, RgbaTextureData)] = {
    load(path) { stream =>
      val img = ImageIO.read(stream)
      
      val width = img.getWidth
      val height = img.getHeight
      
      val data = DataBuffer[Vec4, UByte](width*height)
      val buffer = data.buffer()
      
      var y = 0; while (y < height) {
        var x = 0; while (x < width) {
          
          val i = img.getRGB(x, y)
          val pixel = img.getRGB(x, y)
          val index = (x + (height - 1 - y)*width)*4
          buffer.put(index, (pixel >> 16).toByte)
          buffer.put(index + 1, (pixel >> 8).toByte)
          buffer.put(index + 2, pixel.toByte)
          buffer.put(index + 3, (pixel >> 24).toByte)
          
          x += 1
        }
        y += 1
      }
      
      (ConstVec2i(width, height), data)
    }
  }
  
  def loadObj(path: String)
  :Option[(
    DataBuffer[SInt, Unsigned],// indices
    DataBuffer[Vec3, RFloat], // vertices
    Option[DataBuffer[Vec3, RFloat]], // normals
    Option[DataBuffer[Vec2, RFloat]] // texCoords
  )] = {
    load(path) { stream =>
      val reader = new BufferedReader(new InputStreamReader(stream))
      
      val verticesBuilder = ArrayBuilder.make[Vec3]()
      val normalsBuilder = ArrayBuilder.make[Vec3]()
      val texCoordsBuilder = ArrayBuilder.make[Vec2]()
      
      val indexBuilders = Array(
        ArrayBuilder.make[Array[Int]](),
        ArrayBuilder.make[Array[Int]](),
        ArrayBuilder.make[Array[Int]]()
      )
      
      def decodeIndex(group: String) :Array[Int] = group.split("/").map(_.toInt - 1)
      
      var lineNumber = 1
      var line = reader.readLine(); while (line != null) {
        val tokens: Seq[String] = line.split("\\s")
        tokens match {
          case Seq("f", s0, s1, s2) => {
            indexBuilders(0) += decodeIndex(s0)
            indexBuilders(1) += decodeIndex(s1)
            indexBuilders(2) += decodeIndex(s2)
          }
          case Seq("v", x, y, z) => verticesBuilder += Vec3(x.toDouble, y.toDouble, z.toDouble)
          case Seq("vn", x, y, z) => normalsBuilder += Vec3(x.toDouble, y.toDouble, z.toDouble)
          case Seq("vt", x, y) => texCoordsBuilder += Vec2(x.toDouble, y.toDouble)
          //case Seq("f", _*) => { ... }// TODO: this is bugged in 2.9.2, works in 2.10, change after updating.
          //case _ => // ignore
          case _ => {
            if (tokens.size > 0 && tokens(0) == "f") throw new RuntimeException(
                "Unsupported polygon format in obj model '" + path +
                "' on line " + lineNumber + ", only triangles are supported.")
          }
        }

        lineNumber += 1
        line = reader.readLine()
      }
      
      val vertices = verticesBuilder.result()
      val normals = normalsBuilder.result()
      val texCoords = texCoordsBuilder.result()
      
      val data = {
        val builder = ArrayBuilder.make[IndexedSeq[Object]]()
        builder += vertices
        if (!texCoords.isEmpty) builder += texCoords
        if (!normals.isEmpty) builder += normals
        builder.result()
      }
      
      val index = indexBuilders.map(_.result())
      val vertexMap = new HashMap[IndexedSeq[Object], Integer]
      val resultIndexBuilder = ArrayBuilder.make[Int]()
      
      var count = 0
      var n = 0; while (n < index(0).length) {
        var k = 0; while (k < 3) {
          val group = index(k)(n)
          
          val vertexDataArray = new Array[Object](3)
          var i = 0; while (i < data.length) {
            
            val idx = if (i < group.length) group(i) else group(0)
            vertexDataArray(i) = data(i)(idx)
            
            i += 1
          }
          
          val vertexData: IndexedSeq[Object] = vertexDataArray
          val existing = vertexMap.get(vertexData)
          val vertexId = if (existing != null) existing.toInt else {
            val id = count
            count += 1
            vertexMap.put(vertexData, id)
            id
          }
          
          resultIndexBuilder += vertexId
          k += 1
        }
        
        n += 1
      }
      
      
      def reverseMap[K, V](map: HashMap[K, V]) :HashMap[V, K] = {
        val result = new HashMap[V, K]
        val iter = map.entrySet.iterator
        while (iter.hasNext) {
          val entry = iter.next()
          result.put(entry.getValue, entry.getKey)
        }
        result
      }
      val lookup = reverseMap(vertexMap)
      
      
      val resultIndexArray = resultIndexBuilder.result();
      val resultIndices = IndexBuffer(count, resultIndexArray.length)
      resultIndices.put(resultIndexArray)

      val resultVertices = DataBuffer[Vec3, RFloat](count)
      val resultNormals = if (normals.isEmpty) None else Some(DataBuffer[Vec3, RFloat](count))
      val resultTexCoords = if (normals.isEmpty) None else Some(DataBuffer[Vec2, RFloat](count))
      
      var i = 0; while (i < count) {
        val data = lookup.get(i)
        
        resultVertices(i) = data(0).asInstanceOf[Vec3]
        
        var next = 1
        
        if (resultTexCoords.isDefined) {
          resultTexCoords.get(i) = data(next).asInstanceOf[Vec2]
          next += 1
        }
        
        if (resultNormals.isDefined) {
          resultNormals.get(i) = data(next).asInstanceOf[Vec3]
        }
        
        i += 1
      }
      
      (resultIndices, resultVertices, resultNormals, resultTexCoords)
    }
  }
}
