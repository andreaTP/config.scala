package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigValueType
//remove if not needed
import scala.collection.JavaConversions._

@SerialVersionUID(2L)
class ConfigBoolean(origin: ConfigOrigin, val value: Boolean) extends AbstractConfigValue(origin) with Serializable {

  override def valueType(): ConfigValueType = ConfigValueType.BOOLEAN

  override def unwrapped(): java.lang.Boolean = value

  override def transformToString(): String = if (value) "true" else "false"

  protected override def newCopy(origin: ConfigOrigin): ConfigBoolean = new ConfigBoolean(origin, value)

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)
}
