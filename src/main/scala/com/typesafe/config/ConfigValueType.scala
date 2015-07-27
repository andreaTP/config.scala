package com.typesafe.config

//remove if not needed
import scala.collection.JavaConversions._

object ConfigValueType extends Enumeration {

  val OBJECT = new ConfigValueType()

  val LIST = new ConfigValueType()

  val NUMBER = new ConfigValueType()

  val BOOLEAN = new ConfigValueType()

  val NULL = new ConfigValueType()

  val STRING = new ConfigValueType()

  class ConfigValueType extends Val

  implicit def convertValue(v: Value): ConfigValueType = v.asInstanceOf[ConfigValueType]
}
