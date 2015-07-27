package com.typesafe.config.impl

import com.typesafe.config.ConfigException
import ResolveResult._
//remove if not needed
import scala.collection.JavaConversions._

object ResolveResult {

  def make[V <: AbstractConfigValue](context: ResolveContext, value: V): ResolveResult[V] = new ResolveResult[V](context, value)
}

class ResolveResult[V <: AbstractConfigValue] private (val context: ResolveContext, val value: V)
    {

  def asObjectResult(): ResolveResult[AbstractConfigObject] = {
    if (!(value.isInstanceOf[AbstractConfigObject])) throw new ConfigException.BugOrBroken("Expecting a resolve result to be an object, but it was " + 
      value)
    val o = this
    o.asInstanceOf[ResolveResult[AbstractConfigObject]]
  }

  def asValueResult(): ResolveResult[AbstractConfigValue] = {
    val o = this
    o.asInstanceOf[ResolveResult[AbstractConfigValue]]
  }

  def popTrace(): ResolveResult[V] = make(context.popTrace(), value)

  override def toString(): String = "ResolveResult(" + value + ")"
}
