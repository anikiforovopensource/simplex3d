/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2012, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine


case class Config(//XXX no default values, add BackendProvider, rename this to BackendConfig
  val timer: String = "simplex3d.engine.backend.lwjgl.Timer",
  val launcher: String = "simplex3d.engine.backend.lwjgl.FixedResolutionLauncher",
  val mainLoop: String = "simplex3d.engine.backend.lwjgl.SimpleLoop",
  val renderManager: String = "simplex3d.engine.backend.lwjgl.RenderManager"
)
