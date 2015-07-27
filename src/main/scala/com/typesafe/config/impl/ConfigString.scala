package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValueType
import ConfigString._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigString {

  class Quoted(origin: ConfigOrigin, value: String) extends ConfigString(origin, value) {

    protected override def newCopy(origin: ConfigOrigin): Quoted = new Quoted(origin, value)

    private def writeReplace(): AnyRef = new SerializedConfigValue(this)
  }

  class Unquoted(origin: ConfigOrigin, value: String) extends ConfigString(origin, value) {

    protected override def newCopy(origin: ConfigOrigin): Unquoted = new Unquoted(origin, value)

    private def writeReplace(): AnyRef = new SerializedConfigValue(this)
  }
}

@SerialVersionUID(2L)
abstract class ConfigString protected (origin: ConfigOrigin, protected val value: String)
    extends AbstractConfigValue(origin) with Serializable {

  def wasQuoted(): Boolean = (this.isInstanceOf[Quoted])

  override def valueType(): ConfigValueType = ConfigValueType.STRING

  override def unwrapped(): String = value

  override def transformToString(): String = value

  protected override def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      options: ConfigRenderOptions) {
    var rendered: String = null
    rendered = if (options.getJson) ConfigImplUtil.renderJsonString(value) else ConfigImplUtil.renderStringUnquotedIfPossible(value)
    sb.append(rendered)
  }
}
