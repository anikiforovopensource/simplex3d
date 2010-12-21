/*
 * Simplex3d, CoreBuffer module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer;

import java.io.*;
import java.nio.*;


/** Prevents gaining access to read-only content.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class Protected<A> {
    final Object sharedStore;

    Protected(Object shared) {
        if (shared instanceof Protected) {
            this.sharedStore = ((Protected) shared).sharedStore;
        }
        else {
            this.sharedStore = shared;
        }
    }

    @SuppressWarnings("unchecked")
    final A sharedArray() {
        return (A) sharedStore;
    }

    final ByteBuffer sharedBuffer() {
        return (ByteBuffer) sharedStore;
    }

    protected final Object writeReplace() throws ObjectStreamException {
        if (this instanceof ReadDataArray) {
            ReadBaseSeq src = (ReadBaseSeq) this;
            SerializableData data = mkSerializableInstance();
            data.content_$eq(sharedStore);
            data.readOnly_$eq(src.readOnly());
            return data;
        }
        throw new NotSerializableException();
    }

    protected SerializableData mkSerializableInstance()
    throws NotSerializableException
    {
        throw new NotSerializableException();
    }
}
