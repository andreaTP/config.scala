package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValueType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This exists because sometimes null is not the same as missing. Specifically,
 * if a value is set to null we can give a better error message (indicating
 * where it was set to null) in case someone asks for the value. Also, null
 * overrides values set "earlier" in the search path, while missing values do
 * not.
 *
 */
@SerialVersionUID(2L)
class ConfigNull(origin: ConfigOrigin) extends AbstractConfigValue(origin) with Serializable {

  override def valueType(): ConfigValueType = ConfigValueType.NULL

  override def unwrapped(): AnyRef = null

  override def transformToString(): String = "null"

  protected override def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      options: ConfigRenderOptions) {
    sb.append("null")
  }

  protected override def newCopy(origin: ConfigOrigin): ConfigNull = new ConfigNull(origin)

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)
}
