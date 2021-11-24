/*
 * Copyright 2020 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.tessla_compiler

import java.security.Permission

import de.uni_luebeck.isp.tessla.tessla_compiler.NoExitSecurityManager.SystemExitException

object NoExitSecurityManager {
  case class SystemExitException(status: Int) extends SecurityException
}

final class NoExitSecurityManager extends SecurityManager {
  // Clear out permission checks
  override def checkPermission(perm: Permission): Unit = {}

  override def checkPermission(perm: Permission, context: Object): Unit = {}

  // Throw an exception when checking for exit symbolising insufficient permissions for the exit call
  override def checkExit(status: Int): Unit = throw SystemExitException(status)
}
