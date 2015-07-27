package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigValueType
//remove if not needed
import scala.collection.JavaConversions._

@SerialVersionUID(2L)
class ConfigLong(origin: ConfigOrigin, val value: Long, originalText: String)
    extends ConfigNumber(origin, originalText) with Serializable {

  override def valueType(): ConfigValueType = ConfigValueType.NUMBER

  override def unwrapped(): java.lang.Long = value

  override def transformToString(): String = {
    val s = super.transformToString()
    if (s == null) Long toString value else s
  }

  protected override def longValue(): Long = value

  protected override def doubleValue(): Double = value

  protected override def newCopy(origin: ConfigOrigin): ConfigLong = {
    new ConfigLong(origin, value, originalText)
  }

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)
}
