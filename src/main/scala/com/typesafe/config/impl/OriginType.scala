package com.typesafe.config.impl

//remove if not needed
import scala.collection.JavaConversions._

object OriginType extends Enumeration {

  val GENERIC = new OriginType()

  val FILE = new OriginType()

  val URL = new OriginType()

  val RESOURCE = new OriginType()

  class OriginType extends Val

  implicit def convertValue(v: Value): OriginType = v.asInstanceOf[OriginType]
}
