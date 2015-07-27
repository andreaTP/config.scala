package com.typesafe.config.impl

import java.util.Collection
//remove if not needed
import scala.collection.JavaConversions._

object ResolveStatus extends Enumeration {

  val UNRESOLVED = new ResolveStatus()

  val RESOLVED = new ResolveStatus()

  class ResolveStatus extends Val

  def fromValues(values: Collection[_ <: AbstractConfigValue]): ResolveStatus = {
    values.find(_.resolveStatus() == ResolveStatus.UNRESOLVED)
      .map(ResolveStatus.UNRESOLVED)
      .getOrElse(ResolveStatus.RESOLVED)
  }

  def fromBoolean(resolved: Boolean): ResolveStatus = {
    if (resolved) ResolveStatus.RESOLVED else ResolveStatus.UNRESOLVED
  }

  implicit def convertValue(v: Value): ResolveStatus = v.asInstanceOf[ResolveStatus]
}
