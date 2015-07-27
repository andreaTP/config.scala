package com.typesafe.config.impl

//remove if not needed
import scala.collection.JavaConversions._

object FromMapMode extends Enumeration {

  val KEYS_ARE_PATHS = new FromMapMode()

  val KEYS_ARE_KEYS = new FromMapMode()

  class FromMapMode extends Val

  implicit def convertValue(v: Value): FromMapMode = v.asInstanceOf[FromMapMode]
}
