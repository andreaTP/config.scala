package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigValueType
import com.typesafe.config.ConfigValueType._
//remove if not needed
import scala.collection.JavaConversions._

@SerialVersionUID(2L)
class ConfigDouble(origin: ConfigOrigin, val value: Double, originalText: String)
    extends ConfigNumber(origin, originalText) with Serializable {

  override def valueType(): ConfigValueType = ConfigValueType.NUMBER

  override def unwrapped(): java.lang.Double = value

  override def transformToString(): String = {
    val s = super.transformToString()
    if (s == null) value.toString else s
  }

  protected override def longValue(): Long = value.toLong

  protected override def doubleValue(): Double = value

  protected override def newCopy(origin: ConfigOrigin): ConfigDouble = {
    new ConfigDouble(origin, value, originalText)
  }

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)
}
