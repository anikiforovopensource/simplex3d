/*
 * Simplex3d, BaseMath module
 * Copyright (C) 2010 Simplex3d Team
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math

import scala.collection._
import scala.annotation.unchecked._


/** <code>MathObject</code> is a common collection supertype for
 * all the vectors, quaternions, and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class MathObject[+T] extends Iterable[T] {
    def apply(i: Int) :T
    override def size :Int
    def iterator :Iterator[T] = new MathIterator

    private final class MathIterator extends Iterator[T] {
        private var i = 0
        def hasNext: Boolean = (i < size)
        def next() :T = {
            if (i < size) {
                val n = apply(i)
                i += 1
                n
            } else Iterator.empty.next
        }
    }

    override def head = apply(0)
    override def last = apply(size - 1)

    override def foreach[U](f: T => U): Unit = {
        var i = 0; while (i < size) {

            f(apply(i))

            i += 1
        }
    }
}

/** <code>AnyVec[_]</code> is a common collection supertype for all vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class AnyVec[T] extends MathObject[T]

/** <code>ConstVec[_]</code> is a common collection supertype
 * for all constant vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait ConstVec[T] extends AnyVec[T] with Immutable

/** <code>Vec[_]</code> is a common collection supertype
 * for all mutable vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait Vec[T] extends AnyVec[T] with Mutable with Implicits[On]

/** <code>AnyQuat[_]</code> is a common collection supertype
 * for all quaternions.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class AnyQuat[T] extends MathObject[T]

/** <code>ConstQuat[_]</code> is a common collection supertype
 * for all constant quaternions.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait ConstQuat[T] extends AnyQuat[T] with Immutable

/** <code>Quat[_]</code> is a common collection supertype
 * for all mutable quaternions.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait Quat[T] extends AnyQuat[T] with Mutable with Implicits[On]

/** <code>AnyMat[_]</code> is a common collection supertype
 * for all matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class AnyMat[+V <: ConstVec[_]] extends MathObject[V]

/** <code>ConstMat[_]</code> is a common collection supertype
 * for all constant matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait ConstMat[+V <: ConstVec[_]] extends AnyMat[V] with Immutable

/** <code>Mat[_]</code> is a common collection supertype
 * for all mutable matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait Mat[+V <: ConstVec[_]] extends AnyMat[V] with Mutable with Implicits[On]
