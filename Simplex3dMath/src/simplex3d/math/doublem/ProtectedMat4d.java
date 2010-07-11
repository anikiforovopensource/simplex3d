/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2010, Simplex3d Team
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

package simplex3d.math.doublem;

import java.io.Serializable;
import simplex3d.math.*;


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class ProtectedMat4d<P, T> extends Read4x4<P, T> implements Serializable {
    double p00; double p10; double p20; double p30;
    double p01; double p11; double p21; double p31;
    double p02; double p12; double p22; double p32;
    double p03; double p13; double p23; double p33;
}
