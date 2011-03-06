/*
 * Simplex3dConsole
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dConsole.
 *
 * Simplex3dConsole is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dConsole is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.console

import java.security.AllPermission
import java.security.CodeSource
import java.security.Permissions
import java.security.Policy
import java.security.ProtectionDomain


/**
 * @author Aleksey Nikiforov (lex)
 */
class InterpretedPolicy extends Policy {
  private val allPermissions = new Permissions
  allPermissions.add(new AllPermission)
  allPermissions.setReadOnly

  private val noPermissions = new Permissions
  noPermissions.setReadOnly

  override def getPermissions(codesource: CodeSource) = {
    if (codesource.getLocation == null) noPermissions
    else allPermissions
  }

  override def getPermissions(domain: ProtectionDomain) = {
    if (domain.getCodeSource.getLocation == null) noPermissions
    else allPermissions
  }
}
