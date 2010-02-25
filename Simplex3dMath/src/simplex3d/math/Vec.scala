/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package simplex3d.math

abstract class Vec[T] extends Iterable[T] {
    def apply(i: Int) :T
    def components :Int
    def iterator :Iterator[T] = new VecIterator

    private final class VecIterator extends Iterator[T] {
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
