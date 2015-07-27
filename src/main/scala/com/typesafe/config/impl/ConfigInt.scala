package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigValueType
//remove if not needed
import scala.collection.JavaConversions._

@SerialVersionUID(2L)
class ConfigInt(origin: ConfigOrigin, val value: Int, originalText: String) extends ConfigNumber(origin, 
  originalText) with Serializable {

  override def valueType(): ConfigValueType = ConfigValueType.NUMBER

  override def unwrapped(): java.lang.Integer = value

  override def transformToString(): String = {
    val s = super.transformToString()
    if (s == null) Integer toString value else s
  }

  protected override def longValue(): Long = value

  protected override def doubleValue(): Double = value

  protected override def newCopy(origin: ConfigOrigin): ConfigInt = {
    new ConfigInt(origin, value, originalText)
  }

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)
}
