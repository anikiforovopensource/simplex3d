/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package simplex3d.math

abstract class Quat[T] extends Iterable[T] {
    def apply(i: Int) :T
    def components :Int = 4
    def iterator :Iterator[T] = new QuatIterator

    private final class QuatIterator extends Iterator[T] {
        private var c = 0
        def hasNext: Boolean = (c < components)
        def next() :T = {
            if (c < components) {
                val n = apply(c)
                c += 1
                n
            } else Iterator.empty.next
        }
    }
}
