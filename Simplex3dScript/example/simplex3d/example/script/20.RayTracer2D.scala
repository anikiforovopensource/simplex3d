package simplex3d.example.script

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.script.ImageUtils._


/**
 * @author Krux
 */
object RayTracer2D extends App {

// by Krux.

{
  def raytrace(start:Vec2,dir:Vec2) = {
    val pos = Vec2i(floor(start))
    val step = Vec2i(0)
    val tMax = Vec2(0)
    for(i <- 0 to 1){
      step(i) = if(dir(i) > 0) 1 else -1
    
      if(step(0) == 1)
        tMax(i) = (ceil(start(i))-start(i))/abs(dir(i));
      else
        tMax(i) = (start(i)-floor(start(i)))/abs(dir(i));
    }
    val tDelta = abs(1/dir)
    pos.clone +: {
    for(i <- 0 to 20) yield {
      if(tMax.x < tMax.y){
        pos.x += step.x
        tMax.x += tDelta.x
      }
      else {
        pos.y += step.y
        tMax.y += tDelta.y
      }
      pos.clone
    }
    }
  }
  def makequads(s:Seq[Vec2i]) = {
    val vertices:Seq[Vec2] = Seq(0,1,2,3,0,2,1,3) map Seq(Vec2(2,2),Vec2(30,2),Vec2(2,30),Vec2(30,30))
    s.flatMap(offset => vertices.map(v => v + 32f * offset))
  }
  {
    val start = Vec2(1.5,1.6)
    val direction = normalize(Vec2(2,3))
    val ray =  Seq(start,direction*20) map (_*32)
    val intersections = raytrace(start,direction)
    println(intersections)
    val gridlines = ((0 to 16) flatMap ( i =>  List(Vec2(0,i),Vec2(16,i),Vec2(i,0),Vec2(i,16)) )) map (_*32)
    val otherlines = makequads(intersections)
    val lines = DataArray[Vec2, RFloat]( (gridlines ++ otherlines ++ ray) : _*)
    val colors = DataArray[Vec3, UByte](lines.size)
    for (i <- 0 until colors.size) { colors(i) = if(i%2 == 1){if(i<gridlines.size) Vec3(1,0,0) else Vec3(0, 1, 0) }else Vec3(0, 0, 1) }
  
  
    drawLines("Ray", Vec3(0)) { (dims) => {println(dims);
      (lines, colors, lines.size)}
    }
  }
}

}
