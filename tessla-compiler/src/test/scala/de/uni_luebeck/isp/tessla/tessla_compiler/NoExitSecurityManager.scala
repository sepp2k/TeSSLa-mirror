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
