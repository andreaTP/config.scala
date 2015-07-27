package com.typesafe.config.impl

import java.io.File
import java.lang.ref.WeakReference
import java.net.URL
import java.time.Duration
import java.util.ArrayList
import java.util.Collections
import java.util.HashMap
import java.util.Iterator
import java.util.List
import java.util.Map
import java.util.Properties
import java.util.concurrent.Callable
import com.typesafe.config.Config
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigIncluder
import com.typesafe.config.ConfigMemorySize
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigParseOptions
import com.typesafe.config.ConfigParseable
import com.typesafe.config.ConfigValue
import com.typesafe.config.impl.SimpleIncluder.NameSource
import ConfigImpl._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigImpl {

  private class LoaderCache() {

    private var currentSystemProperties: Config = null

    private var currentLoader: WeakReference[ClassLoader] = new WeakReference[ClassLoader](null)

    private var cache: Map[String, Config] = new HashMap[String, Config]()

    def getOrElseUpdate(loader: ClassLoader, key: String, updater: Callable[Config]): Config = {
      synchronized {
        if (loader != currentLoader.get) {
          cache.clear()
          currentLoader = new WeakReference[ClassLoader](loader)
        }
        val systemProperties = systemPropertiesAsConfig()
        if (systemProperties != currentSystemProperties) {
          cache.clear()
          currentSystemProperties = systemProperties
        }
        var config = cache.get(key)
        if (config == null) {
          config = updater.call()
          if (config == null) throw new ConfigException.BugOrBroken("null config from cache updater")
          cache.put(key, config)
        }
        config
      }
    }
  }

  object LoaderCacheHolder {

    val cache = new LoaderCache()
  }

  def computeCachedConfig(loader: ClassLoader, key: String, updater: Callable[Config]): Config = {
    var cache: LoaderCache = null
    cache = LoaderCacheHolder.cache
    cache.getOrElseUpdate(loader, key, updater)
  }

  class FileNameSource extends SimpleIncluder.NameSource {

    override def nameToParseable(name: String, parseOptions: ConfigParseOptions): ConfigParseable = {
      Parseable.newFile(new File(name), parseOptions)
    }
  }

  class ClasspathNameSource extends SimpleIncluder.NameSource {

    override def nameToParseable(name: String, parseOptions: ConfigParseOptions): ConfigParseable = {
      Parseable.newResources(name, parseOptions)
    }
  }

  class ClasspathNameSourceWithClass(val klass: Class[_]) extends SimpleIncluder.NameSource {

    override def nameToParseable(name: String, parseOptions: ConfigParseOptions): ConfigParseable = {
      Parseable.newResources(klass, name, parseOptions)
    }
  }

  def parseResourcesAnySyntax(klass: Class[_], resourceBasename: String, baseOptions: ConfigParseOptions): ConfigObject = {
    val source = new ClasspathNameSourceWithClass(klass)
    SimpleIncluder.fromBasename(source, resourceBasename, baseOptions)
  }

  def parseResourcesAnySyntax(resourceBasename: String, baseOptions: ConfigParseOptions): ConfigObject = {
    val source = new ClasspathNameSource()
    SimpleIncluder.fromBasename(source, resourceBasename, baseOptions)
  }

  def parseFileAnySyntax(basename: File, baseOptions: ConfigParseOptions): ConfigObject = {
    val source = new FileNameSource()
    SimpleIncluder.fromBasename(source, basename.getPath, baseOptions)
  }

  def emptyObject(originDescription: String): AbstractConfigObject = {
    val origin = if (originDescription != null) SimpleConfigOrigin.newSimple(originDescription) else null
    emptyObject(origin)
  }

  def emptyConfig(originDescription: String): Config = {
    emptyObject(originDescription).toConfig()
  }

  def empty(origin: ConfigOrigin): AbstractConfigObject = emptyObject(origin)

  private val defaultValueOrigin = SimpleConfigOrigin.newSimple("hardcoded value")

  private val defaultTrueValue = new ConfigBoolean(defaultValueOrigin, true)

  private val defaultFalseValue = new ConfigBoolean(defaultValueOrigin, false)

  private val defaultNullValue = new ConfigNull(defaultValueOrigin)

  private val defaultEmptyList = new SimpleConfigList(defaultValueOrigin, Collections.emptyList[AbstractConfigValue]())

  private val defaultEmptyObject = SimpleConfigObject.empty(defaultValueOrigin)

  private def emptyList(origin: ConfigOrigin): SimpleConfigList = {
    if (origin == null || origin == defaultValueOrigin) defaultEmptyList else new SimpleConfigList(origin, 
      Collections.emptyList[AbstractConfigValue]())
  }

  private def emptyObject(origin: ConfigOrigin): AbstractConfigObject = {
    if (origin == defaultValueOrigin) defaultEmptyObject else SimpleConfigObject.empty(origin)
  }

  private def valueOrigin(originDescription: String): ConfigOrigin = {
    if (originDescription == null) defaultValueOrigin else SimpleConfigOrigin.newSimple(originDescription)
  }

  def fromAnyRef(`object`: AnyRef, originDescription: String): ConfigValue = {
    val origin = valueOrigin(originDescription)
    fromAnyRef(`object`, origin, FromMapMode.KEYS_ARE_KEYS)
  }

  def fromPathMap(pathMap: Map[String, _ <: Any], originDescription: String): ConfigObject = {
    val origin = valueOrigin(originDescription)
    fromAnyRef(pathMap, origin, FromMapMode.KEYS_ARE_PATHS).asInstanceOf[ConfigObject]
  }

  def fromAnyRef(`object`: AnyRef, origin: ConfigOrigin, mapMode: FromMapMode): AbstractConfigValue = {
    if (origin == null) throw new ConfigException.BugOrBroken("origin not supposed to be null")
    if (`object` == null) {
      if (origin != defaultValueOrigin) new ConfigNull(origin) else defaultNullValue
    } else if (`object`.isInstanceOf[AbstractConfigValue]) {
      `object`.asInstanceOf[AbstractConfigValue]
    } else if (`object`.isInstanceOf[java.lang.Boolean]) {
      if (origin != defaultValueOrigin) {
        new ConfigBoolean(origin, `object`.asInstanceOf[java.lang.Boolean])
      } else if (`object`.asInstanceOf[java.lang.Boolean]) {
        defaultTrueValue
      } else {
        defaultFalseValue
      }
    } else if (`object`.isInstanceOf[String]) {
      new ConfigString.Quoted(origin, `object`.asInstanceOf[String])
    } else if (`object`.isInstanceOf[Number]) {
      if (`object`.isInstanceOf[java.lang.Double]) {
        new ConfigDouble(origin, `object`.asInstanceOf[java.lang.Double], null)
      } else if (`object`.isInstanceOf[java.lang.Integer]) {
        new ConfigInt(origin, `object`.asInstanceOf[java.lang.Integer], null)
      } else if (`object`.isInstanceOf[java.lang.Long]) {
        new ConfigLong(origin, `object`.asInstanceOf[java.lang.Long], null)
      } else {
        ConfigNumber.newNumber(origin, `object`.asInstanceOf[Number].doubleValue(), null)
      }
    } else if (`object`.isInstanceOf[Duration]) {
      new ConfigLong(origin, `object`.asInstanceOf[Duration].toMillis(), null)
    } else if (`object`.isInstanceOf[Map[_,_]]) {
      if (`object`.asInstanceOf[Map[_, _]].isEmpty) return emptyObject(origin)
      if (mapMode == FromMapMode.KEYS_ARE_KEYS) {
        val values = new HashMap[String, AbstractConfigValue]()
        for ((key, value) <- `object`.asInstanceOf[Map[_, _]]) {
          if (!(key.isInstanceOf[String])) throw new ConfigException.BugOrBroken("bug in method caller: not valid to create ConfigObject from map with non-String key: " + 
            key)
          val _value: AbstractConfigValue = fromAnyRef(value.asInstanceOf[AnyRef], origin, mapMode)
          values.put(key.asInstanceOf[String], _value)
        }
        new SimpleConfigObject(origin, values)
      } else {
        PropertiesParser.fromPathMap(origin, `object`.asInstanceOf[Map[_, _]])
      }
    } else if (`object`.isInstanceOf[java.lang.Iterable[_]]) {
      val i = `object`.asInstanceOf[java.lang.Iterable[_]].iterator()
      if (!i.hasNext) return emptyList(origin)
      val values = new ArrayList[AbstractConfigValue]()
      while (i.hasNext) {
        val v = fromAnyRef(i.next().asInstanceOf[AnyRef], origin, mapMode)
        values.add(v)
      }
      new SimpleConfigList(origin, values)
    } else if (`object`.isInstanceOf[ConfigMemorySize]) {
      new ConfigLong(origin, `object`.asInstanceOf[ConfigMemorySize].toBytes(), null)
    } else {
      throw new ConfigException.BugOrBroken("bug in method caller: not valid to create ConfigValue from: " + 
        `object`)
    }
  }

  object DefaultIncluderHolder {

    val defaultIncluder = new SimpleIncluder(null)
  }

  def defaultIncluder(): ConfigIncluder = DefaultIncluderHolder.defaultIncluder

  private def getSystemProperties(): Properties = {
    //DOESN?T MAKE ANY SENSE IN JS
    //val systemProperties = System.getProperties
    //val systemPropertiesCopy = new Properties()
    //synchronized (systemProperties) {
    //  systemPropertiesCopy.putAll(systemProperties)
    //}
    //systemPropertiesCopy
    new Properties()
  }

  private def loadSystemProperties(): AbstractConfigObject = {
    Parseable.newProperties(getSystemProperties, ConfigParseOptions.defaults().setOriginDescription("system properties"))
      .parse().asInstanceOf[AbstractConfigObject]
  }

  object SystemPropertiesHolder {

    @volatile var systemProperties: AbstractConfigObject = loadSystemProperties()
  }

  def systemPropertiesAsConfigObject(): AbstractConfigObject = SystemPropertiesHolder.systemProperties

  def systemPropertiesAsConfig(): Config = {
    systemPropertiesAsConfigObject().toConfig()
  }

  def reloadSystemPropertiesConfig() {
    SystemPropertiesHolder.systemProperties = loadSystemProperties()
  }

  private def loadEnvVariables(): AbstractConfigObject = {
    val env = System.getenv
    val m = new HashMap[String, AbstractConfigValue]()
    for ((key, value) <- env) {
      m.put(key, new ConfigString.Quoted(SimpleConfigOrigin.newSimple("env var " + key), value))
    }
    new SimpleConfigObject(SimpleConfigOrigin.newSimple("env variables"), m, ResolveStatus.RESOLVED, 
      false)
  }

  object EnvVariablesHolder {

    val envVariables = loadEnvVariables()
  }

  def envVariablesAsConfigObject(): AbstractConfigObject = EnvVariablesHolder.envVariables

  def envVariablesAsConfig(): Config = envVariablesAsConfigObject().toConfig()

  def defaultReference(loader: ClassLoader): Config = {
    computeCachedConfig(loader, "defaultReference", new Callable[Config]() {

      override def call(): Config = {
        val unresolvedResources = Parseable.newResources("reference.conf", ConfigParseOptions.defaults().setClassLoader(loader))
          .parse()
          .toConfig()
        systemPropertiesAsConfig().withFallback(unresolvedResources)
          .resolve()
      }
    })
  }

  object DebugHolder {

    private var LOADS: String = "loads"

    private var SUBSTITUTIONS: String = "substitutions"

    private def loadDiagnostics(): Map[String, Boolean] = {
      val result = new HashMap[String, Boolean]()
      result.put(LOADS, false)
      result.put(SUBSTITUTIONS, false)
      val s = System.getProperty("config.trace")
      if (s == null) {
        result
      } else {
        val keys = s.split(",")
        for (k <- keys) {
          if (k == LOADS) {
            result.put(LOADS, true)
          } else if (k == SUBSTITUTIONS) {
            result.put(SUBSTITUTIONS, true)
          } else {
            System.err.println("config.trace property contains unknown trace topic '" + 
              k + 
              "'")
          }
        }
        result
      }
    }

    private val diagnostics = loadDiagnostics()

    var traceLoadsEnabled: Boolean = diagnostics.get(LOADS)

    var traceSubstitutionsEnabled: Boolean = diagnostics.get(SUBSTITUTIONS)
  }

  def traceLoadsEnabled(): Boolean = 
    DebugHolder.traceLoadsEnabled

  def traceSubstitutionsEnabled(): Boolean = 
    DebugHolder.traceSubstitutionsEnabled

  def trace(message: String) {
    System.err.println(message)
  }

  def trace(indentLevel: Int, message: String) {
    var _indentLevel = indentLevel
    while (_indentLevel > 0) {
      System.err.print("  ")
      _indentLevel -= 1
    }
    System.err.println(message)
  }

  def improveNotResolved(what: Path, original: ConfigException.NotResolved): ConfigException.NotResolved = {
    val newMessage = what.render() + 
      " has not been resolved, you need to call Config#resolve()," + 
      " see API docs for Config#resolve()"
    if (newMessage == original.getMessage) original else new ConfigException.NotResolved(newMessage, 
      original)
  }

  def newSimpleOrigin(description: String): ConfigOrigin = {
    if (description == null) {
      defaultValueOrigin
    } else {
      SimpleConfigOrigin.newSimple(description)
    }
  }

  def newFileOrigin(filename: String): ConfigOrigin = SimpleConfigOrigin.newFile(filename)

  def newURLOrigin(url: URL): ConfigOrigin = SimpleConfigOrigin.newURL(url)
}

/**
 * Internal implementation detail, not ABI stable, do not touch.
 * For use only by the {@link com.typesafe.config} package.
 */
class ConfigImpl {






}
