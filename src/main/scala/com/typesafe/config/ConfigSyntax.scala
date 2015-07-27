package com.typesafe.config

//remove if not needed
import scala.collection.JavaConversions._

object ConfigSyntax extends Enumeration {

  val JSON = new ConfigSyntax()

  val CONF = new ConfigSyntax()

  val PROPERTIES = new ConfigSyntax()

  class ConfigSyntax extends Val

  implicit def convertValue(v: Value): ConfigSyntax = v.asInstanceOf[ConfigSyntax]
}
