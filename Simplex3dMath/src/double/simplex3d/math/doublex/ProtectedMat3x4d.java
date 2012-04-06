/*
 * Simplex3dMath - Double Module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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

package simplex3d.math.doublex;

import java.io.Serializable;
import simplex3d.math.types.*;


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class ProtectedMat3x4d<P> extends AnyMat3x4<P> implements Serializable {
    public static final long serialVersionUID = 8104346712419693669L;
    double p00; double p01; double p02; double p03;
    double p10; double p11; double p12; double p13;
    double p20; double p21; double p22; double p23;
}
