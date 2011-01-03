/*
 * Simplex3d, CoreMath module
 * Copyright (C) 2010-2011, Simplex3d Team
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

/** <code>StateSelector</code> is an indicator to enable or disable implicits.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] sealed trait StateSelector

/** <code>On</code> is a subtrait of <code>StateSelector</code>,
 * it is used to enable implicits.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] sealed trait On extends StateSelector

/** <code>Off</code> is a subtrait of <code>StateSelector</code>,
 * it is used to disable implicits.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] sealed trait Off extends On

/** <code>Implicits</code> is a mix-in trait to enable or disable implicits
 * by setting the type parameter to On or Off.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] trait Implicits[+Enabled <: StateSelector]
