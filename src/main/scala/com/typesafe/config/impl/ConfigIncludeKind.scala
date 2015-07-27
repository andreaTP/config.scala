package com.typesafe.config.impl

//remove if not needed
import scala.collection.JavaConversions._

object ConfigIncludeKind extends Enumeration {

  val URL = new ConfigIncludeKind()

  val FILE = new ConfigIncludeKind()

  val CLASSPATH = new ConfigIncludeKind()

  val HEURISTIC = new ConfigIncludeKind()

  class ConfigIncludeKind extends Val

  implicit def convertValue(v: Value): ConfigIncludeKind = v.asInstanceOf[ConfigIncludeKind]
}
