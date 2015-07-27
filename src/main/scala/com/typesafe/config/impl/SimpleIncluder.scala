package com.typesafe.config.impl

import java.io.File
import java.net.MalformedURLException
import java.net.URL
import java.util.ArrayList
import java.util.List
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigIncludeContext
import com.typesafe.config.ConfigIncluder
import com.typesafe.config.ConfigIncluderClasspath
import com.typesafe.config.ConfigIncluderFile
import com.typesafe.config.ConfigIncluderURL
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigParseOptions
import com.typesafe.config.ConfigParseable
import com.typesafe.config.ConfigSyntax
import SimpleIncluder._
//remove if not needed
import scala.collection.JavaConversions._

object SimpleIncluder {

  def clearForInclude(options: ConfigParseOptions): ConfigParseOptions = {
    options.setSyntax(null).setOriginDescription(null).setAllowMissing(true)
  }

  def includeWithoutFallback(context: ConfigIncludeContext, name: String): ConfigObject = {
    var url: URL = null
    try {
      url = new URL(name)
    } catch {
      case e: MalformedURLException => url = null
    }
    if (url != null) {
      includeURLWithoutFallback(context, url)
    } else {
      val source = new RelativeNameSource(context)
      fromBasename(source, name, context.parseOptions())
    }
  }

  def includeURLWithoutFallback(context: ConfigIncludeContext, url: URL): ConfigObject = {
    ConfigFactory.parseURL(url, context.parseOptions())
      .root()
  }

  def includeFileWithoutFallback(context: ConfigIncludeContext, file: File): ConfigObject = {
    ConfigFactory.parseFileAnySyntax(file, context.parseOptions())
      .root()
  }

  def includeResourceWithoutFallback(context: ConfigIncludeContext, resource: String): ConfigObject = {
    ConfigFactory.parseResourcesAnySyntax(resource, context.parseOptions())
      .root()
  }

  trait NameSource {

    def nameToParseable(name: String, parseOptions: ConfigParseOptions): ConfigParseable
  }

  private class RelativeNameSource(val context: ConfigIncludeContext) extends NameSource {

    override def nameToParseable(name: String, options: ConfigParseOptions): ConfigParseable = {
      val p = context.relativeTo(name)
      if (p == null) {
        Parseable.newNotFound(name, "include was not found: '" + name + "'", options)
      } else {
        p
      }
    }
  }

  def fromBasename(source: NameSource, name: String, options: ConfigParseOptions): ConfigObject = {
    var obj: ConfigObject = null
    if (name.endsWith(".conf") || name.endsWith(".json") || name.endsWith(".properties")) {
      val p = source.nameToParseable(name, options)
      obj = p.parse(p.options().setAllowMissing(options.getAllowMissing))
    } else {
      val confHandle = source.nameToParseable(name + ".conf", options)
      val jsonHandle = source.nameToParseable(name + ".json", options)
      val propsHandle = source.nameToParseable(name + ".properties", options)
      var gotSomething = false
      val fails = new ArrayList[ConfigException.IO]()
      val syntax = options.getSyntax
      obj = SimpleConfigObject.empty(SimpleConfigOrigin.newSimple(name))
      if (syntax == null || syntax == ConfigSyntax.CONF) {
        try {
          obj = confHandle.parse(confHandle.options().setAllowMissing(false).setSyntax(ConfigSyntax.CONF))
          gotSomething = true
        } catch {
          case e: ConfigException.IO => fails.add(e)
        }
      }
      if (syntax == null || syntax == ConfigSyntax.JSON) {
        try {
          val parsed = jsonHandle.parse(jsonHandle.options().setAllowMissing(false).setSyntax(ConfigSyntax.JSON))
          obj = obj.withFallback(parsed)
          gotSomething = true
        } catch {
          case e: ConfigException.IO => fails.add(e)
        }
      }
      if (syntax == null || syntax == ConfigSyntax.PROPERTIES) {
        try {
          val parsed = propsHandle.parse(propsHandle.options().setAllowMissing(false).setSyntax(ConfigSyntax.PROPERTIES))
          obj = obj.withFallback(parsed)
          gotSomething = true
        } catch {
          case e: ConfigException.IO => fails.add(e)
        }
      }
      if (!options.getAllowMissing && !gotSomething) {
        if (ConfigImpl.traceLoadsEnabled()) {
          ConfigImpl.trace("Did not find '" + name + 
            "' with any extension (.conf, .json, .properties); " + 
            "exceptions should have been logged above.")
        }
        if (fails.isEmpty) {
          throw new ConfigException.BugOrBroken("should not be reached: nothing found but no exceptions thrown")
        } else {
          val sb = new StringBuilder()
          for (t <- fails) {
            sb.append(t.getMessage)
            sb.append(", ")
          }
          sb.setLength(sb.length - 2)
          throw new ConfigException.IO(SimpleConfigOrigin.newSimple(name), sb.toString, fails.get(0))
        }
      } else if (!gotSomething) {
        if (ConfigImpl.traceLoadsEnabled()) {
          ConfigImpl.trace("Did not find '" + name + 
            "' with any extension (.conf, .json, .properties); but '" + 
            name + 
            "' is allowed to be missing. Exceptions from load attempts should have been logged above.")
        }
      }
    }
    obj
  }

  private class Proxy(val delegate: ConfigIncluder) extends FullIncluder {

    override def withFallback(fallback: ConfigIncluder): ConfigIncluder = this

    override def include(context: ConfigIncludeContext, what: String): ConfigObject = delegate.include(context, what)

    override def includeResources(context: ConfigIncludeContext, what: String): ConfigObject = {
      if (delegate.isInstanceOf[ConfigIncluderClasspath]) delegate.asInstanceOf[ConfigIncluderClasspath].includeResources(context, 
        what) else includeResourceWithoutFallback(context, what)
    }

    override def includeURL(context: ConfigIncludeContext, what: URL): ConfigObject = {
      if (delegate.isInstanceOf[ConfigIncluderURL]) delegate.asInstanceOf[ConfigIncluderURL].includeURL(context, 
        what) else includeURLWithoutFallback(context, what)
    }

    override def includeFile(context: ConfigIncludeContext, what: File): ConfigObject = {
      if (delegate.isInstanceOf[ConfigIncluderFile]) delegate.asInstanceOf[ConfigIncluderFile].includeFile(context, 
        what) else includeFileWithoutFallback(context, what)
    }
  }

  def makeFull(includer: ConfigIncluder): FullIncluder = {
    if (includer.isInstanceOf[FullIncluder]) includer.asInstanceOf[FullIncluder] else new Proxy(includer)
  }
}

class SimpleIncluder(var fallback: ConfigIncluder) extends FullIncluder {

  override def include(context: ConfigIncludeContext, name: String): ConfigObject = {
    val obj = includeWithoutFallback(context, name)
    if (fallback != null) {
      obj.withFallback(fallback.include(context, name))
    } else {
      obj
    }
  }

  override def includeURL(context: ConfigIncludeContext, url: URL): ConfigObject = {
    val obj = includeURLWithoutFallback(context, url)
    if (fallback != null && fallback.isInstanceOf[ConfigIncluderURL]) {
      obj.withFallback(fallback.asInstanceOf[ConfigIncluderURL].includeURL(context, url))
    } else {
      obj
    }
  }

  override def includeFile(context: ConfigIncludeContext, file: File): ConfigObject = {
    val obj = includeFileWithoutFallback(context, file)
    if (fallback != null && fallback.isInstanceOf[ConfigIncluderFile]) {
      obj.withFallback(fallback.asInstanceOf[ConfigIncluderFile].includeFile(context, file))
    } else {
      obj
    }
  }

  override def includeResources(context: ConfigIncludeContext, resource: String): ConfigObject = {
    val obj = includeResourceWithoutFallback(context, resource)
    if (fallback != null && fallback.isInstanceOf[ConfigIncluderClasspath]) {
      obj.withFallback(fallback.asInstanceOf[ConfigIncluderClasspath].includeResources(context, resource))
    } else {
      obj
    }
  }

  override def withFallback(fallback: ConfigIncluder): ConfigIncluder = {
    if (this == fallback) {
      throw new ConfigException.BugOrBroken("trying to create includer cycle")
    } else if (this.fallback == fallback) {
      this
    } else if (this.fallback != null) {
      new SimpleIncluder(this.fallback.withFallback(fallback))
    } else {
      new SimpleIncluder(fallback)
    }
  }


}
