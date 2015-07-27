package com.typesafe.config.impl

import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigSyntax
import com.typesafe.config.ConfigSyntax._
import com.typesafe.config.ConfigValueType
import com.typesafe.config.ConfigValueType._
import java.io.StringReader
import java.util._
//remove if not needed
import scala.collection.JavaConversions._

object PathParser {

  class Element(initial: String, var canBeEmpty: Boolean) {

    var sb: StringBuilder = new StringBuilder(initial)

    override def toString(): String = {
      "Element(" + sb.toString + "," + canBeEmpty + ")"
    }
  }

  var apiOrigin: ConfigOrigin = SimpleConfigOrigin.newSimple("path parameter")

  def parsePathNode(path: String): ConfigNodePath = parsePathNode(path, ConfigSyntax.CONF)

  def parsePathNode(path: String, flavor: ConfigSyntax): ConfigNodePath = {
    val reader = new StringReader(path)
    try {
      val tokens = Tokenizer.tokenize(apiOrigin, reader, flavor)
      tokens.next()
      parsePathNodeExpression(tokens, apiOrigin, path, flavor)
    } finally {
      reader.close()
    }
  }

  def parsePath(path: String): Path = {
    val speculated = speculativeFastParsePath(path)
    if (speculated != null) return speculated
    val reader = new StringReader(path)
    try {
      val tokens = Tokenizer.tokenize(apiOrigin, reader, ConfigSyntax.CONF)
      tokens.next()
      parsePathExpression(tokens, apiOrigin, path)
    } finally {
      reader.close()
    }
  }

  protected def parsePathExpression(expression: Iterator[Token], origin: ConfigOrigin): Path = {
    parsePathExpression(expression, origin, null, null, ConfigSyntax.CONF)
  }

  protected def parsePathExpression(expression: Iterator[Token], origin: ConfigOrigin, originalText: String): Path = {
    parsePathExpression(expression, origin, originalText, null, ConfigSyntax.CONF)
  }

  protected def parsePathNodeExpression(expression: Iterator[Token], origin: ConfigOrigin): ConfigNodePath = {
    parsePathNodeExpression(expression, origin, null, ConfigSyntax.CONF)
  }

  protected def parsePathNodeExpression(expression: Iterator[Token], 
      origin: ConfigOrigin, 
      originalText: String, 
      flavor: ConfigSyntax): ConfigNodePath = {
    val pathTokens = new ArrayList[Token]()
    val path = parsePathExpression(expression, origin, originalText, pathTokens, flavor)
    new ConfigNodePath(path, pathTokens)
  }

  protected def parsePathExpression(expression: Iterator[Token], 
      origin: ConfigOrigin, 
      originalText: String, 
      pathTokens: ArrayList[Token], 
      flavor: ConfigSyntax): Path = {
    val buf = new ArrayList[Element]()
    buf.add(new Element("", false))
    if (!expression.hasNext) {
      throw new ConfigException.BadPath(origin, originalText, "Expecting a field name or path here, but got nothing")
    }
    while (expression.hasNext) {
      val t = expression.next()
      if (pathTokens != null) pathTokens.add(t)
      if (Tokens.isIgnoredWhitespace(t)) //continue
      if (Tokens.isValueWithType(t, ConfigValueType.STRING)) {
        val v = Tokens.getValue(t)
        val s = v.transformToString()
        addPathText(buf, true, s)
      } else if (t == Tokens.END) {
      } else {
        var text: String = null
        if (Tokens.isValue(t)) {
          val v = Tokens.getValue(t)
          if (pathTokens != null) {
            pathTokens.remove(pathTokens.size - 1)
            pathTokens.addAll(splitTokenOnPeriod(t, flavor))
          }
          text = v.transformToString()
        } else if (Tokens.isUnquotedText(t)) {
          if (pathTokens != null) {
            pathTokens.remove(pathTokens.size - 1)
            pathTokens.addAll(splitTokenOnPeriod(t, flavor))
          }
          text = Tokens.getUnquotedText(t)
        } else {
          throw new ConfigException.BadPath(origin, originalText, "Token not allowed in path expression: " + t + 
            " (you can double-quote this token if you really want it here)")
        }
        addPathText(buf, false, text)
      }
    }
    val pb = new PathBuilder()
    for (e <- buf) {
      if (e.sb.length == 0 && !e.canBeEmpty) {
        throw new ConfigException.BadPath(origin, originalText, "path has a leading, trailing, or two adjacent period '.' (use quoted \"\" empty string if you want an empty element)")
      } else {
        pb.appendKey(e.sb.toString)
      }
    }
    pb.result()
  }

  private def splitTokenOnPeriod(t: Token, flavor: ConfigSyntax): Collection[Token] = {
    val tokenText = t.tokenText()
    if (tokenText == ".") {
      return Collections.singletonList(t)
    }
    val splitToken = tokenText.split("\\.")
    val splitTokens = new ArrayList[Token]()
    for (s <- splitToken) {
      if (flavor == ConfigSyntax.CONF) splitTokens.add(Tokens.newUnquotedText(t.origin(), s)) else splitTokens.add(Tokens.newString(t.origin(), 
        s, "\"" + s + "\""))
      splitTokens.add(Tokens.newUnquotedText(t.origin(), "."))
    }
    if (tokenText.charAt(tokenText.length - 1) != '.') splitTokens.remove(splitTokens.size - 1)
    splitTokens
  }

  private def addPathText(buf: List[Element], wasQuoted: Boolean, newText: String) {
    val i = if (wasQuoted) -1 else newText.indexOf('.')
    val current = buf.get(buf.size - 1)
    if (i < 0) {
      current.sb.append(newText)
      if (wasQuoted && current.sb.length == 0) current.canBeEmpty = true
    } else {
      current.sb.append(newText.substring(0, i))
      buf.add(new Element("", false))
      addPathText(buf, false, newText.substring(i + 1))
    }
  }

  private def looksUnsafeForFastParser(s: String): Boolean = {
    var lastWasDot = true
    val len = s.length
    if (s.isEmpty) return true
    if (s.charAt(0) == '.') return true
    if (s.charAt(len - 1) == '.') return true
    for (i <- 0 until len) {
      val c = s.charAt(i)
      if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') {
        lastWasDot = false
        //continue
      } else if (c == '.') {
        if (lastWasDot) return true
        lastWasDot = true
      } else if (c == '-') {
        if (lastWasDot) return true
        //continue
      } else {
        return true
      }
    }
    if (lastWasDot) return true
    false
  }

  private def fastPathBuild(tail: Path, s: String, end: Int): Path = {
    val splitAt = s.lastIndexOf('.', end - 1)
    val tokens = new ArrayList[Token]()
    tokens.add(Tokens.newUnquotedText(null, s))
    val withOneMoreElement = new Path(s.substring(splitAt + 1, end), tail)
    if (splitAt < 0) {
      withOneMoreElement
    } else {
      fastPathBuild(withOneMoreElement, s, splitAt)
    }
  }

  private def speculativeFastParsePath(path: String): Path = {
    val s = ConfigImplUtil.unicodeTrim(path)
    if (looksUnsafeForFastParser(s)) return null
    fastPathBuild(null, s, s.length)
  }
}
