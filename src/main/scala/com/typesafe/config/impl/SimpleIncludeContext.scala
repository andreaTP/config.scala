package com.typesafe.config.impl

import com.typesafe.config.ConfigIncludeContext
import com.typesafe.config.ConfigParseOptions
import com.typesafe.config.ConfigParseable
//remove if not needed
import scala.collection.JavaConversions._

class SimpleIncludeContext(val parseable: Parseable) extends ConfigIncludeContext {

  def withParseable(parseable: Parseable): SimpleIncludeContext = {
    if (parseable == this.parseable) this else new SimpleIncludeContext(parseable)
  }

  override def relativeTo(filename: String): ConfigParseable = {
    if (ConfigImpl.traceLoadsEnabled()) ConfigImpl.trace("Looking for '" + filename + "' relative to " + parseable)
    if (parseable != null) parseable.relativeTo(filename) else null
  }

  override def parseOptions(): ConfigParseOptions = {
    SimpleIncluder.clearForInclude(parseable.options())
  }
}
