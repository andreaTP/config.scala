package com.typesafe.config.impl

import java.io.BufferedReader
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.FilterReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.Reader
import java.io.StringReader
import java.io.UnsupportedEncodingException
import java.net.MalformedURLException
import java.net.URI
import java.net.URISyntaxException
import java.net.URL
import java.net.URLConnection
import java.util._
import com.typesafe.config._
import com.typesafe.config.parser._
import Parseable._
import ParseableURL._
import ParseableResources._

//remove if not needed
import scala.collection.JavaConversions._

object Parseable {

  /**
   * Internal implementation detail, not ABI stable, do not touch.
   */
  protected trait Relativizer {

    def relativeTo(filename: String): ConfigParseable
  }

  private val parseStack = new ThreadLocal[LinkedList[Parseable]]() {

    protected override def initialValue(): LinkedList[Parseable] = new LinkedList[Parseable]()
  }

  private val MAX_INCLUDE_DEPTH = 50

  protected def trace(message: String) {
    if (ConfigImpl.traceLoadsEnabled()) {
      ConfigImpl.trace(message)
    }
  }

  def forceParsedToObject(value: ConfigValue): AbstractConfigObject = {
    if (value.isInstanceOf[AbstractConfigObject]) {
      value.asInstanceOf[AbstractConfigObject]
    } else {
      throw new ConfigException.WrongType(value.origin(), "", "object at file root", value.valueType().name())
    }
  }

  private def syntaxFromExtension(name: String): ConfigSyntax = {
    if (name.endsWith(".json")) ConfigSyntax.JSON else if (name.endsWith(".conf")) ConfigSyntax.CONF else if (name.endsWith(".properties")) ConfigSyntax.PROPERTIES else null
  }

  private def readerFromStream(input: InputStream): Reader = readerFromStream(input, "UTF-8")

  private def readerFromStream(input: InputStream, encoding: String): Reader = {
    val reader = new InputStreamReader(input, encoding)
    new BufferedReader(reader)
  }

  private def doNotClose(input: Reader): Reader = {
    new FilterReader(input) {

      override def close() {
      }
    }
  }

  def relativeTo(url: URL, filename: String): URL = {
    if (new File(filename).isAbsolute) return null
    try {
      val siblingURI = url.toURI()
      val relative = new URI(filename)
      val resolved = siblingURI.resolve(relative).toURL()
      resolved
    } catch {
      case e: MalformedURLException => null
      case e: URISyntaxException => null
      case e: IllegalArgumentException => null
    }
  }

  def relativeTo(file: File, filename: String): File = {
    val child = new File(filename)
    if (child.isAbsolute) return null
    val parent = file.getParentFile
    if (parent == null) null else new File(parent, filename)
  }

  private class ParseableNotFound(val what: String, val message: String, options: ConfigParseOptions)
      extends Parseable {

    postConstruct(options)

    protected override def reader(): Reader = {
      throw new FileNotFoundException(message)
    }

    protected override def createOrigin(): ConfigOrigin = SimpleConfigOrigin.newSimple(what)
  }

  def newNotFound(whatNotFound: String, message: String, options: ConfigParseOptions): Parseable = {
    new ParseableNotFound(whatNotFound, message, options)
  }

  private class ParseableReader(val reader: Reader, options: ConfigParseOptions)
      extends Parseable {

    postConstruct(options)

    protected override def reader(): Reader = {
      if (ConfigImpl.traceLoadsEnabled()) trace("Loading config from reader " + reader)
      reader
    }

    protected override def createOrigin(): ConfigOrigin = SimpleConfigOrigin.newSimple("Reader")
  }

  def newReader(reader: Reader, options: ConfigParseOptions): Parseable = {
    new ParseableReader(doNotClose(reader), options)
  }

  private class ParseableString(val input: String, options: ConfigParseOptions)
      extends Parseable {

    postConstruct(options)

    protected override def reader(): Reader = {
      if (ConfigImpl.traceLoadsEnabled()) trace("Loading config from a String " + input)
      new StringReader(input)
    }

    protected override def createOrigin(): ConfigOrigin = SimpleConfigOrigin.newSimple("String")

    override def toString(): String = {
      getClass.getSimpleName + "(" + input + ")"
    }
  }

  def newString(input: String, options: ConfigParseOptions): Parseable = new ParseableString(input, options)

  private val jsonContentType = "application/json"

  private val propertiesContentType = "text/x-java-properties"

  private val hoconContentType = "application/hocon"

  object ParseableURL {

    private def acceptContentType(options: ConfigParseOptions): String = {
      if (options.getSyntax == null) return null
      options.getSyntax match {
        case JSON => return jsonContentType
        case CONF => return hoconContentType
        case PROPERTIES => return propertiesContentType
      }
      null
    }
  }

  private class ParseableURL protected (protected val input: URL) extends Parseable {

    private var contentType: String = null

    def this(input: URL, options: ConfigParseOptions) {
      this(input)
      postConstruct(options)
    }

    protected override def reader(): Reader = {
      throw new ConfigException.BugOrBroken("reader() without options should not be called on ParseableURL")
    }

    protected override def reader(options: ConfigParseOptions): Reader = {
      if (ConfigImpl.traceLoadsEnabled()) trace("Loading config from a URL: " + input.toExternalForm())
      val connection = input.openConnection()
      val acceptContent = acceptContentType(options)
      if (acceptContent != null) {
        connection.setRequestProperty("Accept", acceptContent)
      }
      connection.connect()
      contentType = connection.getContentType
      if (contentType != null) {
        if (ConfigImpl.traceLoadsEnabled()) trace("URL sets Content-Type: '" + contentType + "'")
        contentType = contentType.trim()
        val semi = contentType.indexOf(';')
        if (semi >= 0) contentType = contentType.substring(0, semi)
      }
      val stream = connection.getInputStream
      readerFromStream(stream)
    }

    override def guessSyntax(): ConfigSyntax = syntaxFromExtension(input.getPath)

    override def contentType(): ConfigSyntax = {
      if (contentType != null) {
        if (contentType == jsonContentType) ConfigSyntax.JSON else if (contentType == propertiesContentType) ConfigSyntax.PROPERTIES else if (contentType == hoconContentType) ConfigSyntax.CONF else {
          if (ConfigImpl.traceLoadsEnabled()) trace("'" + contentType + "' isn't a known content type")
          null
        }
      } else {
        null
      }
    }

    override def relativeTo(filename: String): ConfigParseable = {
      val url = relativeTo(input, filename)
      if (url == null) return null
      newURL(url, options().setOriginDescription(null))
    }

    protected override def createOrigin(): ConfigOrigin = SimpleConfigOrigin.newURL(input)

    override def toString(): String = {
      getClass.getSimpleName + "(" + input.toExternalForm() + 
        ")"
    }
  }

  def newURL(input: URL, options: ConfigParseOptions): Parseable = {
    if (input.getProtocol == "file") {
      newFile(ConfigImplUtil.urlToFile(input), options)
    } else {
      new ParseableURL(input, options)
    }
  }

  private class ParseableFile(val input: File, options: ConfigParseOptions) extends Parseable {

    postConstruct(options)

    protected override def reader(): Reader = {
      if (ConfigImpl.traceLoadsEnabled()) trace("Loading config from a file: " + input)
      val stream = new FileInputStream(input)
      readerFromStream(stream)
    }

    override def guessSyntax(): ConfigSyntax = syntaxFromExtension(input.getName)

    override def relativeTo(filename: String): ConfigParseable = {
      var sibling: File = null
      sibling = if ((new File(filename)).isAbsolute) new File(filename) else relativeTo(input, filename)
      if (sibling == null) return null
      if (sibling.exists()) {
        trace(sibling + " exists, so loading it as a file")
        newFile(sibling, options().setOriginDescription(null))
      } else {
        trace(sibling + 
          " does not exist, so trying it as a classpath resource")
        super.relativeTo(filename)
      }
    }

    protected override def createOrigin(): ConfigOrigin = {
      SimpleConfigOrigin.newFile(input.getPath)
    }

    override def toString(): String = {
      getClass.getSimpleName + "(" + input.getPath + ")"
    }
  }

  def newFile(input: File, options: ConfigParseOptions): Parseable = new ParseableFile(input, options)

  private class ParseableResourceURL(input: URL, 
      options: ConfigParseOptions, 
      val resource: String, 
      val relativizer: Relativizer) extends ParseableURL(input) {

    postConstruct(options)

    protected override def createOrigin(): ConfigOrigin = {
      SimpleConfigOrigin.newResource(resource, input)
    }

    override def relativeTo(filename: String): ConfigParseable = relativizer.relativeTo(filename)
  }

  private def newResourceURL(input: URL, 
      options: ConfigParseOptions, 
      resource: String, 
      relativizer: Relativizer): Parseable = {
    new ParseableResourceURL(input, options, resource, relativizer)
  }

  object ParseableResources {

    def parent(resource: String): String = {
      val i = resource.lastIndexOf('/')
      if (i < 0) {
        null
      } else {
        resource.substring(0, i)
      }
    }
  }

  private class ParseableResources(val resource: String, options: ConfigParseOptions)
      extends Parseable with Relativizer {

    postConstruct(options)

    protected override def reader(): Reader = {
      throw new ConfigException.BugOrBroken("reader() should not be called on resources")
    }

    protected override def rawParseValue(origin: ConfigOrigin, finalOptions: ConfigParseOptions): AbstractConfigObject = {
      val loader = finalOptions.getClassLoader
      if (loader == null) throw new ConfigException.BugOrBroken("null class loader; pass in a class loader or use Thread.currentThread().setContextClassLoader()")
      val e = loader.getResources(resource)
      if (!e.hasMoreElements()) {
        if (ConfigImpl.traceLoadsEnabled()) trace("Loading config from class loader " + loader + " but there were no resources called " + 
          resource)
        throw new IOException("resource not found on classpath: " + resource)
      }
      var merged = SimpleConfigObject.empty(origin)
      while (e.hasMoreElements()) {
        val url = e.nextElement()
        if (ConfigImpl.traceLoadsEnabled()) trace("Loading config from resource '" + resource + "' URL " + 
          url.toExternalForm() + 
          " from class loader " + 
          loader)
        val element = newResourceURL(url, finalOptions, resource, this)
        val v = element.parseValue()
        merged = merged.withFallback(v)
      }
      merged
    }

    override def guessSyntax(): ConfigSyntax = syntaxFromExtension(resource)

    override def relativeTo(sibling: String): ConfigParseable = {
      if (sibling.startsWith("/")) {
        newResources(sibling.substring(1), options().setOriginDescription(null))
      } else {
        val parent = parent(resource)
        if (parent == null) newResources(sibling, options().setOriginDescription(null)) else newResources(parent + "/" + sibling, 
          options().setOriginDescription(null))
      }
    }

    protected override def createOrigin(): ConfigOrigin = {
      SimpleConfigOrigin.newResource(resource)
    }

    override def toString(): String = {
      getClass.getSimpleName + "(" + resource + ")"
    }
  }

  def newResources(klass: Class[_], resource: String, options: ConfigParseOptions): Parseable = {
    newResources(convertResourceName(klass, resource), options.setClassLoader(klass.getClassLoader))
  }

  private def convertResourceName(klass: Class[_], resource: String): String = {
    if (resource.startsWith("/")) {
      resource.substring(1)
    } else {
      val className = klass.getName
      val i = className.lastIndexOf('.')
      if (i < 0) {
        resource
      } else {
        val packageName = className.substring(0, i)
        val packagePath = packageName.replace('.', '/')
        packagePath + "/" + resource
      }
    }
  }

  def newResources(resource: String, options: ConfigParseOptions): Parseable = {
    if (options.getClassLoader == null) throw new ConfigException.BugOrBroken("null class loader; pass in a class loader or use Thread.currentThread().setContextClassLoader()")
    new ParseableResources(resource, options)
  }

  private class ParseableProperties(val props: Properties, options: ConfigParseOptions)
      extends Parseable {

    postConstruct(options)

    protected override def reader(): Reader = {
      throw new ConfigException.BugOrBroken("reader() should not be called on props")
    }

    protected override def rawParseValue(origin: ConfigOrigin, finalOptions: ConfigParseOptions): AbstractConfigObject = {
      if (ConfigImpl.traceLoadsEnabled()) trace("Loading config from properties " + props)
      PropertiesParser.fromProperties(origin, props)
    }

    override def guessSyntax(): ConfigSyntax = ConfigSyntax.PROPERTIES

    protected override def createOrigin(): ConfigOrigin = {
      SimpleConfigOrigin.newSimple("properties")
    }

    override def toString(): String = {
      getClass.getSimpleName + "(" + props.size + " props)"
    }
  }

  def newProperties(properties: Properties, options: ConfigParseOptions): Parseable = {
    new ParseableProperties(properties, options)
  }
}

/**
 * Internal implementation detail, not ABI stable, do not touch.
 * For use only by the {@link com.typesafe.config} package.
 * The point of this class is to avoid "propagating" each
 * overload on "thing which can be parsed" through multiple
 * interfaces. Most interfaces can have just one overload that
 * takes a Parseable. Also it's used as an abstract "resource
 * handle" in the ConfigIncluder interface.
 */
abstract class Parseable protected () extends ConfigParseable {

  var includeContext: ConfigIncludeContext = _

  private var initialOptions: ConfigParseOptions = _

  private var initialOrigin: ConfigOrigin = _

  private def fixupOptions(baseOptions: ConfigParseOptions): ConfigParseOptions = {
    var syntax = baseOptions.getSyntax
    if (syntax == null) {
      syntax = guessSyntax()
    }
    if (syntax == null) {
      syntax = ConfigSyntax.CONF
    }
    var modified = baseOptions.setSyntax(syntax)
    modified = modified.appendIncluder(ConfigImpl.defaultIncluder())
    modified = modified.setIncluder(SimpleIncluder.makeFull(modified.getIncluder))
    modified
  }

  protected def postConstruct(baseOptions: ConfigParseOptions) {
    this.initialOptions = fixupOptions(baseOptions)
    this.includeContext = new SimpleIncludeContext(this)
    initialOrigin = if (initialOptions.getOriginDescription != null) SimpleConfigOrigin.newSimple(initialOptions.getOriginDescription) else createOrigin()
  }

  protected def reader(): Reader

  protected def reader(options: ConfigParseOptions): Reader = reader()

  def guessSyntax(): ConfigSyntax = null

  def contentType(): ConfigSyntax = null

  def relativeTo(filename: String): ConfigParseable = {
    var resource = filename
    if (filename.startsWith("/")) resource = filename.substring(1)
    newResources(resource, options().setOriginDescription(null))
  }

  override def parse(baseOptions: ConfigParseOptions): ConfigObject = {
    val stack = parseStack.get
    if (stack.size >= MAX_INCLUDE_DEPTH) {
      throw new ConfigException.Parse(initialOrigin, "include statements nested more than " + MAX_INCLUDE_DEPTH + 
        " times, you probably have a cycle in your includes. Trace: " + 
        stack)
    }
    stack.addFirst(this)
    try {
      forceParsedToObject(parseValue(baseOptions))
    } finally {
      stack.removeFirst()
      if (stack.isEmpty) {
        parseStack.remove()
      }
    }
  }

  def parseValue(baseOptions: ConfigParseOptions): AbstractConfigValue = {
    val options = fixupOptions(baseOptions)
    var origin: ConfigOrigin = null
    origin = if (options.getOriginDescription != null) SimpleConfigOrigin.newSimple(options.getOriginDescription) else initialOrigin
    parseValue(origin, options)
  }

  private def parseValue(origin: ConfigOrigin, finalOptions: ConfigParseOptions): AbstractConfigValue = {
    try {
      rawParseValue(origin, finalOptions)
    } catch {
      case e: IOException => if (finalOptions.getAllowMissing) {
        SimpleConfigObject.emptyMissing(origin)
      } else {
        trace("exception loading " + origin.description() + ": " + e.getClass.getName + 
          ": " + 
          e.getMessage)
        throw new ConfigException.IO(origin, e.getClass.getName + ": " + e.getMessage, e)
      }
    }
  }

  def parseDocument(baseOptions: ConfigParseOptions): ConfigDocument = {
    val options = fixupOptions(baseOptions)
    var origin: ConfigOrigin = null
    origin = if (options.getOriginDescription != null) SimpleConfigOrigin.newSimple(options.getOriginDescription) else initialOrigin
    parseDocument(origin, options)
  }

  private def parseDocument(origin: ConfigOrigin, finalOptions: ConfigParseOptions): ConfigDocument = {
    try {
      rawParseDocument(origin, finalOptions)
    } catch {
      case e: IOException => if (finalOptions.getAllowMissing) {
        val children = new ArrayList[AbstractConfigNode]()
        children.add(new ConfigNodeObject(new ArrayList[AbstractConfigNode]()))
        new SimpleConfigDocument(new ConfigNodeRoot(children, origin), finalOptions)
      } else {
        trace("exception loading " + origin.description() + ": " + e.getClass.getName + 
          ": " + 
          e.getMessage)
        throw new ConfigException.IO(origin, e.getClass.getName + ": " + e.getMessage, e)
      }
    }
  }

  protected def rawParseValue(origin: ConfigOrigin, finalOptions: ConfigParseOptions): AbstractConfigValue = {
    val reader = reader(finalOptions)
    val contentType = contentType()
    var optionsWithContentType: ConfigParseOptions = null
    if (contentType != null) {
      if (ConfigImpl.traceLoadsEnabled() && finalOptions.getSyntax != null) trace("Overriding syntax " + finalOptions.getSyntax + " with Content-Type which specified " + 
        contentType)
      optionsWithContentType = finalOptions.setSyntax(contentType)
    } else {
      optionsWithContentType = finalOptions
    }
    try {
      rawParseValue(reader, origin, optionsWithContentType)
    } finally {
      reader.close()
    }
  }

  private def rawParseValue(reader: Reader, origin: ConfigOrigin, finalOptions: ConfigParseOptions): AbstractConfigValue = {
    if (finalOptions.getSyntax == ConfigSyntax.PROPERTIES) {
      PropertiesParser.parse(reader, origin)
    } else {
      val tokens = Tokenizer.tokenize(origin, reader, finalOptions.getSyntax)
      val document = ConfigDocumentParser.parse(tokens, origin, finalOptions)
      ConfigParser.parse(document, origin, finalOptions, includeContext())
    }
  }

  protected def rawParseDocument(origin: ConfigOrigin, finalOptions: ConfigParseOptions): ConfigDocument = {
    val reader = reader(finalOptions)
    val contentType = contentType()
    var optionsWithContentType: ConfigParseOptions = null
    if (contentType != null) {
      if (ConfigImpl.traceLoadsEnabled() && finalOptions.getSyntax != null) trace("Overriding syntax " + finalOptions.getSyntax + " with Content-Type which specified " + 
        contentType)
      optionsWithContentType = finalOptions.setSyntax(contentType)
    } else {
      optionsWithContentType = finalOptions
    }
    try {
      rawParseDocument(reader, origin, optionsWithContentType)
    } finally {
      reader.close()
    }
  }

  private def rawParseDocument(reader: Reader, origin: ConfigOrigin, finalOptions: ConfigParseOptions): ConfigDocument = {
    val tokens = Tokenizer.tokenize(origin, reader, finalOptions.getSyntax)
    new SimpleConfigDocument(ConfigDocumentParser.parse(tokens, origin, finalOptions), finalOptions)
  }

  def parse(): ConfigObject = {
    forceParsedToObject(parseValue(options()))
  }

  def parseConfigDocument(): ConfigDocument = parseDocument(options())

  def parseValue(): AbstractConfigValue = parseValue(options())

  override def origin(): ConfigOrigin = initialOrigin

  protected def createOrigin(): ConfigOrigin

  override def options(): ConfigParseOptions = initialOptions

  override def toString(): String = getClass.getSimpleName
}
