package com.typesafe.config.impl

import com.typesafe.config._
import com.typesafe.config.parser.ConfigDocument
import java.io.StringReader
import java.util.Iterator
//remove if not needed
import scala.collection.JavaConversions._

class SimpleConfigDocument(var configNodeTree: ConfigNodeRoot, var parseOptions: ConfigParseOptions)
    extends ConfigDocument {

  override def withValueText(path: String, newValue: String): ConfigDocument = {
    if (newValue == null) throw new ConfigException.BugOrBroken("null value for " + path + " passed to withValueText")
    val origin = SimpleConfigOrigin.newSimple("single value parsing")
    val reader = new StringReader(newValue)
    val tokens = Tokenizer.tokenize(origin, reader, parseOptions.getSyntax)
    val parsedValue = ConfigDocumentParser.parseValue(tokens, origin, parseOptions)
    reader.close()
    new SimpleConfigDocument(configNodeTree.setValue(path, parsedValue, parseOptions.getSyntax), parseOptions)
  }

  override def withValue(path: String, newValue: ConfigValue): ConfigDocument = {
    if (newValue == null) throw new ConfigException.BugOrBroken("null value for " + path + " passed to withValue")
    var options = ConfigRenderOptions.defaults()
    options = options.setOriginComments(false)
    withValueText(path, newValue.render(options).trim())
  }

  override def withoutPath(path: String): ConfigDocument = {
    new SimpleConfigDocument(configNodeTree.setValue(path, null, parseOptions.getSyntax), parseOptions)
  }

  override def hasPath(path: String): Boolean = configNodeTree.hasValue(path)

  def render(): String = configNodeTree.render()

  override def equals(other: Any): Boolean = other match {
    case other: ConfigDocument => render() == other.render()
    case _ => false
  }

  override def hashCode(): Int = render().hashCode
}
