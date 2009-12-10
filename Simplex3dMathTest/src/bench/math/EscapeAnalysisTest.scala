/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package bench.math


object EscapeBench {
    def main(args: Array[String]) {
        new EscapeBenchTest().run()
    }
}

class EscapeBenchTest {
    val length = 10000
    val loops = 100000
    
    val random = new scala.util.Random(1)
    def rd = random.nextDouble()
    val data = new Array[Vec4m](length)
    for (i <- 0 until length) {
        data(i) = new Vec4m(rd, rd, rd, rd)
    }

    def run() {
        var start = 0L

        start = System.currentTimeMillis
        testNew(length, loops)
        val makeNewTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testTemp(length, loops)
        val tempTime = System.currentTimeMillis - start

        println("new time: " + makeNewTime +
                ", temp time: " + tempTime + ".")
    }

    def testNew(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length - 2) {

                val t = data(i) + data(i + 1) + data(i + 2)
                answer += (t.x + t.y + t.z + t.w).asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testTemp(length: Int, loops: Int) {
        var answer = 0
        val tmp = new Vec4m(0, 0, 0, 0)

        var l = 0; while (l < loops) {
            var i = 0; while (i < length - 2) {

                val t = data(i).add(data(i + 1), tmp).add(data(i + 2), tmp)
                answer += (t.x + t.y + t.z + t.w).asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }

}

class Vec4m(var x: Double, var y: Double, var z: Double, var w: Double) {
    def +(u: Vec4m) = new Vec4m(x + u.x, y + u.y, z + u.z, w + u.w)
    def add(u: Vec4m, r: Vec4m) = {
        r.x = x + u.x
        r.y = y + u.y
        r.z = z + u.z
        r.w = w + u.w
        r
    }
}
