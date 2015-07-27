package com.typesafe.config.impl

import java.util._
import com.typesafe.config._
import ParseContext._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigDocumentParser {

  def parse(tokens: Iterator[Token], origin: ConfigOrigin, options: ConfigParseOptions): ConfigNodeRoot = {
    val syntax = if (options.getSyntax == null) ConfigSyntax.CONF else options.getSyntax
    val context = new ParseContext(syntax, origin, tokens)
    context.parse()
  }

  def parseValue(tokens: Iterator[Token], origin: ConfigOrigin, options: ConfigParseOptions): AbstractConfigNodeValue = {
    val syntax = if (options.getSyntax == null) ConfigSyntax.CONF else options.getSyntax
    val context = new ParseContext(syntax, origin, tokens)
    context.parseSingleValue()
  }

  object ParseContext {

    private def isIncludeKeyword(t: Token): Boolean = {
      Tokens.isUnquotedText(t) && Tokens.getUnquotedText(t) == "include"
    }

    private def isUnquotedWhitespace(t: Token): Boolean = {
      if (!Tokens.isUnquotedText(t)) return false
      val s = Tokens.getUnquotedText(t)
      for (i <- 0 until s.length) {
        val c = s.charAt(i)
        if (!ConfigImplUtil.isWhitespace(c)) return false
      }
      true
    }
  }

  private class ParseContext(val flavor: ConfigSyntax, origin: ConfigOrigin, val tokens: Iterator[Token])
      {

    private var lineNumber: Int = 1

    private val buffer = new Stack[Token]()

    private val baseOrigin = origin

    var equalsCount: Int = 0

    private def popToken(): Token = {
      if (buffer.isEmpty) {
        return tokens.next()
      }
      buffer.pop()
    }

    private def nextToken(): Token = {
      val t = popToken()
      if (flavor == ConfigSyntax.JSON) {
        if (Tokens.isUnquotedText(t) && !isUnquotedWhitespace(t)) {
          throw parseError("Token not allowed in valid JSON: '" + Tokens.getUnquotedText(t) + 
            "'")
        } else if (Tokens.isSubstitution(t)) {
          throw parseError("Substitutions (${} syntax) not allowed in JSON")
        }
      }
      t
    }

    private def nextTokenCollectingWhitespace(nodes: Collection[AbstractConfigNode]): Token = {
      while (true) {
        val t = nextToken()
        if (Tokens.isIgnoredWhitespace(t) || Tokens.isNewline(t) || 
          isUnquotedWhitespace(t)) {
          nodes.add(new ConfigNodeSingleToken(t))
          if (Tokens.isNewline(t)) {
            lineNumber = t.lineNumber() + 1
          }
        } else if (Tokens.isComment(t)) {
          nodes.add(new ConfigNodeComment(t))
        } else {
          val newNumber = t.lineNumber()
          if (newNumber >= 0) lineNumber = newNumber
          t
        }
      }
    }

    private def putBack(token: Token) {
      buffer.push(token)
    }

    private def checkElementSeparator(nodes: Collection[AbstractConfigNode]): Boolean = {
      if (flavor == ConfigSyntax.JSON) {
        val t = nextTokenCollectingWhitespace(nodes)
        if (t == Tokens.COMMA) {
          nodes.add(new ConfigNodeSingleToken(t))
          true
        } else {
          putBack(t)
          false
        }
      } else {
        var sawSeparatorOrNewline = false
        var t = nextToken()
        while (true) {
          if (Tokens.isIgnoredWhitespace(t) || isUnquotedWhitespace(t)) {
            nodes.add(new ConfigNodeSingleToken(t))
          } else if (Tokens.isComment(t)) {
            nodes.add(new ConfigNodeComment(t))
          } else if (Tokens.isNewline(t)) {
            sawSeparatorOrNewline = true
            lineNumber += 1
            nodes.add(new ConfigNodeSingleToken(t))
          } else if (t == Tokens.COMMA) {
            nodes.add(new ConfigNodeSingleToken(t))
            return true
          } else {
            putBack(t)
            return sawSeparatorOrNewline
          }
          t = nextToken()
        }
      }
    }

    private def consolidateValues(nodes: Collection[AbstractConfigNode]): AbstractConfigNodeValue = {
      if (flavor == ConfigSyntax.JSON) return null
      val values = new ArrayList[AbstractConfigNode]()
      var valueCount = 0
      var t = nextTokenCollectingWhitespace(nodes)
      while (true) {
        var v: AbstractConfigNodeValue = null
        if (Tokens.isIgnoredWhitespace(t)) {
          values.add(new ConfigNodeSingleToken(t))
          t = nextToken()
          //continue
        } else if (Tokens.isValue(t) || Tokens.isUnquotedText(t) || Tokens.isSubstitution(t) || 
          t == Tokens.OPEN_CURLY || 
          t == Tokens.OPEN_SQUARE) {
          v = parseValue(t)
          valueCount += 1
        } else {
          //break
        }
        if (v == null) throw new ConfigException.BugOrBroken("no value")
        values.add(v)
        t = nextToken()
      }
      putBack(t)
      if (valueCount < 2) {
        var value: AbstractConfigNodeValue = null
        for (node <- values) {
          if (node.isInstanceOf[AbstractConfigNodeValue]) value = node.asInstanceOf[AbstractConfigNodeValue] else if (value == null) nodes.add(node) else putBack((new ArrayList[Token](node.tokens())).get(0))
        }
        return value
      }
      var i = values.size - 1
      while (i >= 0) {
        if (values.get(i).isInstanceOf[ConfigNodeSingleToken]) {
          putBack(values.get(i).asInstanceOf[ConfigNodeSingleToken].token())
          values.remove(i)
        } else {
          //break
        }
        i -= 1
      }
      new ConfigNodeConcatenation(values)
    }

    private def parseError(message: String): ConfigException = parseError(message, null)

    private def parseError(message: String, cause: Throwable): ConfigException = {
      new ConfigException.Parse(baseOrigin.withLineNumber(lineNumber), message, cause)
    }

    private def addQuoteSuggestion(badToken: String, message: String): String = {
      addQuoteSuggestion(null, equalsCount > 0, badToken, message)
    }

    private def addQuoteSuggestion(lastPath: Path, 
        insideEquals: Boolean, 
        badToken: String, 
        message: String): String = {
      val previousFieldName = if (lastPath != null) lastPath.render() else null
      var part: String = null
      if (badToken == Tokens.END.toString) {
        if (previousFieldName != null) part = message + " (if you intended '" + previousFieldName + 
          "' to be part of a value, instead of a key, " + 
          "try adding double quotes around the whole value" else return message
      } else {
        part = if (previousFieldName != null) message + " (if you intended " + badToken + " to be part of the value for '" + 
          previousFieldName + 
          "', " + 
          "try enclosing the value in double quotes" else message + " (if you intended " + badToken + " to be part of a key or string value, " + 
          "try enclosing the key or value in double quotes"
      }
      if (insideEquals) part + 
        ", or you may be able to rename the file .properties rather than .conf)" else part + ")"
    }

    private def parseValue(t: Token): AbstractConfigNodeValue = {
      var v: AbstractConfigNodeValue = null
      val startingEqualsCount = equalsCount
      if (Tokens.isValue(t) || Tokens.isUnquotedText(t) || Tokens.isSubstitution(t)) {
        v = new ConfigNodeSimpleValue(t)
      } else if (t == Tokens.OPEN_CURLY) {
        v = parseObject(true)
      } else if (t == Tokens.OPEN_SQUARE) {
        v = parseArray()
      } else {
        throw parseError(addQuoteSuggestion(t.toString, "Expecting a value but got wrong token: " + t))
      }
      if (equalsCount != startingEqualsCount) throw new ConfigException.BugOrBroken("Bug in config parser: unbalanced equals count")
      v
    }

    private def parseKey(token: Token): ConfigNodePath = {
      if (flavor == ConfigSyntax.JSON) {
        if (Tokens.isValueWithType(token, ConfigValueType.STRING)) {
          PathParser.parsePathNodeExpression(Collections.singletonList(token).iterator(), null)
        } else {
          throw parseError("Expecting close brace } or a field name here, got " + 
            token)
        }
      } else {
        val expression = new ArrayList[Token]()
        var t = token
        while (Tokens.isValue(t) || Tokens.isUnquotedText(t)) {
          expression.add(t)
          t = nextToken()
        }
        if (expression.isEmpty) {
          throw parseError("expecting a close brace or a field name here, got " + 
            t)
        }
        putBack(t)
        PathParser.parsePathNodeExpression(expression.iterator(), null)
      }
    }

    private def isKeyValueSeparatorToken(t: Token): Boolean = {
      if (flavor == ConfigSyntax.JSON) {
        t == Tokens.COLON
      } else {
        t == Tokens.COLON || t == Tokens.EQUALS || t == Tokens.PLUS_EQUALS
      }
    }

    private def parseInclude(children: ArrayList[AbstractConfigNode]): ConfigNodeInclude = {
      var t = nextTokenCollectingWhitespace(children)
      if (Tokens.isUnquotedText(t)) {
        val kindText = Tokens.getUnquotedText(t)
        var kind: ConfigIncludeKind = null
        if (kindText == "url(") {
          kind = ConfigIncludeKind.URL
        } else if (kindText == "file(") {
          kind = ConfigIncludeKind.FILE
        } else if (kindText == "classpath(") {
          kind = ConfigIncludeKind.CLASSPATH
        } else {
          throw parseError("expecting include parameter to be quoted filename, file(), classpath(), or url(). No spaces are allowed before the open paren. Not expecting: " + 
            t)
        }
        children.add(new ConfigNodeSingleToken(t))
        t = nextTokenCollectingWhitespace(children)
        if (!Tokens.isValueWithType(t, ConfigValueType.STRING)) {
          throw parseError("expecting a quoted string inside file(), classpath(), or url(), rather than: " + 
            t)
        }
        children.add(new ConfigNodeSimpleValue(t))
        t = nextTokenCollectingWhitespace(children)
        if (Tokens.isUnquotedText(t) && Tokens.getUnquotedText(t) == ")") {
        } else {
          throw parseError("expecting a close parentheses ')' here, not: " + t)
        }
        new ConfigNodeInclude(children, kind)
      } else if (Tokens.isValueWithType(t, ConfigValueType.STRING)) {
        children.add(new ConfigNodeSimpleValue(t))
        new ConfigNodeInclude(children, ConfigIncludeKind.HEURISTIC)
      } else {
        throw parseError("include keyword is not followed by a quoted string, but by: " + 
          t)
      }
    }

    private def parseObject(hadOpenCurly: Boolean): ConfigNodeComplexValue = {
      var afterComma = false
      val lastPath: Path = null
      var lastInsideEquals = false
      val objectNodes = new ArrayList[AbstractConfigNode]()
      var keyValueNodes: ArrayList[AbstractConfigNode] = null
      val keys = new HashMap[String, Boolean]()
      if (hadOpenCurly) objectNodes.add(new ConfigNodeSingleToken(Tokens.OPEN_CURLY))
      while (true) {
        var t = nextTokenCollectingWhitespace(objectNodes)
        if (t == Tokens.CLOSE_CURLY) {
          if (flavor == ConfigSyntax.JSON && afterComma) {
            throw parseError(addQuoteSuggestion(t.toString, "expecting a field name after a comma, got a close brace } instead"))
          } else if (!hadOpenCurly) {
            throw parseError(addQuoteSuggestion(t.toString, "unbalanced close brace '}' with no open brace"))
          }
          objectNodes.add(new ConfigNodeSingleToken(Tokens.CLOSE_CURLY))
          //break
        } else if (t == Tokens.END && !hadOpenCurly) {
          putBack(t)
          //break
        } else if (flavor != ConfigSyntax.JSON && isIncludeKeyword(t)) {
          val includeNodes = new ArrayList[AbstractConfigNode]()
          includeNodes.add(new ConfigNodeSingleToken(t))
          objectNodes.add(parseInclude(includeNodes))
          afterComma = false
        } else {
          keyValueNodes = new ArrayList[AbstractConfigNode]()
          val keyToken = t
          val path = parseKey(keyToken)
          keyValueNodes.add(path)
          val afterKey = nextTokenCollectingWhitespace(keyValueNodes)
          var insideEquals = false
          var nextValue: AbstractConfigNodeValue = null
          if (flavor == ConfigSyntax.CONF && afterKey == Tokens.OPEN_CURLY) {
            nextValue = parseValue(afterKey)
          } else {
            if (!isKeyValueSeparatorToken(afterKey)) {
              throw parseError(addQuoteSuggestion(afterKey.toString, "Key '" + path.render() + "' may not be followed by token: " + 
                afterKey))
            }
            keyValueNodes.add(new ConfigNodeSingleToken(afterKey))
            if (afterKey == Tokens.EQUALS) {
              insideEquals = true
              equalsCount += 1
            }
            nextValue = consolidateValues(keyValueNodes)
            if (nextValue == null) {
              nextValue = parseValue(nextTokenCollectingWhitespace(keyValueNodes))
            }
          }
          keyValueNodes.add(nextValue)
          if (insideEquals) {
            equalsCount -= 1
          }
          lastInsideEquals = insideEquals
          val key = path.value().first()
          val remaining = path.value().remainder()
          if (remaining == null) {
            val existing = keys.get(key)
            if (existing != null) {
              if (flavor == ConfigSyntax.JSON) {
                throw parseError("JSON does not allow duplicate fields: '" + key + "' was already seen")
              }
            }
            keys.put(key, true)
          } else {
            if (flavor == ConfigSyntax.JSON) {
              throw new ConfigException.BugOrBroken("somehow got multi-element path in JSON mode")
            }
            keys.put(key, true)
          }
          afterComma = false
          objectNodes.add(new ConfigNodeField(keyValueNodes))
        }
        if (checkElementSeparator(objectNodes)) {
          afterComma = true
        } else {
          t = nextTokenCollectingWhitespace(objectNodes)
          if (t == Tokens.CLOSE_CURLY) {
            if (!hadOpenCurly) {
              throw parseError(addQuoteSuggestion(lastPath, lastInsideEquals, t.toString, "unbalanced close brace '}' with no open brace"))
            }
            objectNodes.add(new ConfigNodeSingleToken(t))
            //break
          } else if (hadOpenCurly) {
            throw parseError(addQuoteSuggestion(lastPath, lastInsideEquals, t.toString, "Expecting close brace } or a comma, got " + t))
          } else {
            if (t == Tokens.END) {
              putBack(t)
              //break
            } else {
              throw parseError(addQuoteSuggestion(lastPath, lastInsideEquals, t.toString, "Expecting end of input or a comma, got " + t))
            }
          }
        }
      }
      new ConfigNodeObject(objectNodes)
    }

    private def parseArray(): ConfigNodeComplexValue = {
      val children = new ArrayList[AbstractConfigNode]()
      children.add(new ConfigNodeSingleToken(Tokens.OPEN_SQUARE))
      var t: Token = null
      var nextValue = consolidateValues(children)
      if (nextValue != null) {
        children.add(nextValue)
      } else {
        t = nextTokenCollectingWhitespace(children)
        if (t == Tokens.CLOSE_SQUARE) {
          children.add(new ConfigNodeSingleToken(t))
          return new ConfigNodeArray(children)
        } else if (Tokens.isValue(t) || t == Tokens.OPEN_CURLY || t == Tokens.OPEN_SQUARE || 
          Tokens.isUnquotedText(t) || 
          Tokens.isSubstitution(t)) {
          nextValue = parseValue(t)
          children.add(nextValue)
        } else {
          throw parseError("List should have ] or a first element after the open [, instead had token: " + 
            t + 
            " (if you want " + 
            t + 
            " to be part of a string value, then double-quote it)")
        }
      }
      while (true) {
        if (checkElementSeparator(children)) {
        } else {
          t = nextTokenCollectingWhitespace(children)
          if (t == Tokens.CLOSE_SQUARE) {
            children.add(new ConfigNodeSingleToken(t))
            return new ConfigNodeArray(children)
          } else {
            throw parseError("List should have ended with ] or had a comma, instead had token: " + 
              t + 
              " (if you want " + 
              t + 
              " to be part of a string value, then double-quote it)")
          }
        }
        nextValue = consolidateValues(children)
        if (nextValue != null) {
          children.add(nextValue)
        } else {
          t = nextTokenCollectingWhitespace(children)
          if (Tokens.isValue(t) || t == Tokens.OPEN_CURLY || t == Tokens.OPEN_SQUARE || 
            Tokens.isUnquotedText(t) || 
            Tokens.isSubstitution(t)) {
            nextValue = parseValue(t)
            children.add(nextValue)
          } else if (flavor != ConfigSyntax.JSON && t == Tokens.CLOSE_SQUARE) {
            putBack(t)
          } else {
            throw parseError("List should have had new element after a comma, instead had token: " + 
              t + 
              " (if you want the comma or " + 
              t + 
              " to be part of a string value, then double-quote it)")
          }
        }
      }
    }

    def parse(): ConfigNodeRoot = {
      val children = new ArrayList[AbstractConfigNode]()
      var t = nextToken()
      if (t == Tokens.START) {
      } else {
        throw new ConfigException.BugOrBroken("token stream did not begin with START, had " + t)
      }
      t = nextTokenCollectingWhitespace(children)
      var result: AbstractConfigNode = null
      var missingCurly = false
      if (t == Tokens.OPEN_CURLY || t == Tokens.OPEN_SQUARE) {
        result = parseValue(t)
      } else {
        if (flavor == ConfigSyntax.JSON) {
          if (t == Tokens.END) {
            throw parseError("Empty document")
          } else {
            throw parseError("Document must have an object or array at root, unexpected token: " + 
              t)
          }
        } else {
          putBack(t)
          missingCurly = true
          result = parseObject(false)
        }
      }
      if (result.isInstanceOf[ConfigNodeObject] && missingCurly) {
        children.addAll(result.asInstanceOf[ConfigNodeComplexValue].children())
      } else {
        children.add(result)
      }
      t = nextTokenCollectingWhitespace(children)
      if (t == Tokens.END) {
        if (missingCurly) {
          new ConfigNodeRoot(Collections.singletonList(new ConfigNodeObject(children).asInstanceOf[AbstractConfigNode]), 
            baseOrigin)
        } else {
          new ConfigNodeRoot(children, baseOrigin)
        }
      } else {
        throw parseError("Document has trailing tokens after first object or array: " + 
          t)
      }
    }

    def parseSingleValue(): AbstractConfigNodeValue = {
      var t = nextToken()
      if (t == Tokens.START) {
      } else {
        throw new ConfigException.BugOrBroken("token stream did not begin with START, had " + t)
      }
      t = nextToken()
      if (Tokens.isIgnoredWhitespace(t) || Tokens.isNewline(t) || 
        isUnquotedWhitespace(t) || 
        Tokens.isComment(t)) {
        throw parseError("The value from withValueText cannot have leading or trailing newlines, whitespace, or comments")
      }
      if (t == Tokens.END) {
        throw parseError("Empty value")
      }
      if (flavor == ConfigSyntax.JSON) {
        val node = parseValue(t)
        t = nextToken()
        if (t == Tokens.END) {
          node
        } else {
          throw parseError("Parsing JSON and the value set in withValueText was either a concatenation or " + 
            "had trailing whitespace, newlines, or comments")
        }
      } else {
        putBack(t)
        val nodes = new ArrayList[AbstractConfigNode]()
        val node = consolidateValues(nodes)
        t = nextToken()
        if (t == Tokens.END) {
          node
        } else {
          throw parseError("The value from withValueText cannot have leading or trailing newlines, whitespace, or comments")
        }
      }
    }
  }
}
