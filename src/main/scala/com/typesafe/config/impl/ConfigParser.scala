package com.typesafe.config.impl

import java.io.File
import java.net.MalformedURLException
import java.net.URL
import java.util.ArrayList
import java.util.Collections
import java.util.HashMap
import java.util.LinkedList
import java.util.List
import java.util.ListIterator
import java.util.Map
import com.typesafe.config._
import ParseContext._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigParser {

  def parse(document: ConfigNodeRoot, 
      origin: ConfigOrigin, 
      options: ConfigParseOptions, 
      includeContext: ConfigIncludeContext): AbstractConfigValue = {
    val context = new ParseContext(options.getSyntax, origin, document, SimpleIncluder.makeFull(options.getIncluder), 
      includeContext)
    context.parse()
  }

  object ParseContext {

    private def createValueUnderPath(path: Path, value: AbstractConfigValue): AbstractConfigObject = {
      val keys = new ArrayList[String]()
      var key = path.first()
      var remaining = path.remainder()
      while (key != null) {
        keys.add(key)
        if (remaining == null) {
          //break
        } else {
          key = remaining.first()
          remaining = remaining.remainder()
        }
      }
      val i = keys.listIterator(keys.size)
      val deepest = i.previous()
      var o = new SimpleConfigObject(value.origin().withComments(null), Collections.singletonMap[String, AbstractConfigValue](deepest, 
        value))
      while (i.hasPrevious()) {
        val m = Collections.singletonMap[String, AbstractConfigValue](i.previous(), o)
        o = new SimpleConfigObject(value.origin().withComments(null), m)
      }
      o
    }
  }

  private class ParseContext(val flavor: ConfigSyntax, 
      origin: ConfigOrigin, 
      val document: ConfigNodeRoot, 
      val includer: FullIncluder, 
      val includeContext: ConfigIncludeContext) {

    private var lineNumber: Int = 1

    private val baseOrigin = origin

    private val pathStack = new LinkedList[Path]()

    var arrayCount: Int = 0

    private def parseConcatenation(n: ConfigNodeConcatenation): AbstractConfigValue = {
      if (flavor == ConfigSyntax.JSON) throw new ConfigException.BugOrBroken("Found a concatenation node in JSON")
      val values = new ArrayList[AbstractConfigValue]()
      for (node <- n.children()) {
        var v: AbstractConfigValue = null
        if (node.isInstanceOf[AbstractConfigNodeValue]) {
          v = parseValue(node.asInstanceOf[AbstractConfigNodeValue], null)
          values.add(v)
        }
      }
      ConfigConcatenation.concatenate(values)
    }

    private def lineOrigin(): SimpleConfigOrigin = {
      baseOrigin.asInstanceOf[SimpleConfigOrigin].withLineNumber(lineNumber)
    }

    private def parseError(message: String): ConfigException = parseError(message, null)

    private def parseError(message: String, cause: Throwable): ConfigException = {
      new ConfigException.Parse(lineOrigin(), message, cause)
    }

    private def fullCurrentPath(): Path = {
      if (pathStack.isEmpty) throw new ConfigException.BugOrBroken("Bug in parser; tried to get current path when at root") else new Path(pathStack.descendingIterator())
    }

    private def parseValue(n: AbstractConfigNodeValue, comments: List[String]): AbstractConfigValue = {
      var v: AbstractConfigValue = null
      val startingArrayCount = arrayCount
      if (n.isInstanceOf[ConfigNodeSimpleValue]) {
        v = n.asInstanceOf[ConfigNodeSimpleValue].value()
      } else if (n.isInstanceOf[ConfigNodeObject]) {
        v = parseObject(n.asInstanceOf[ConfigNodeObject])
      } else if (n.isInstanceOf[ConfigNodeArray]) {
        v = parseArray(n.asInstanceOf[ConfigNodeArray])
      } else if (n.isInstanceOf[ConfigNodeConcatenation]) {
        v = parseConcatenation(n.asInstanceOf[ConfigNodeConcatenation])
      } else {
        throw parseError("Expecting a value but got wrong node type: " + n.getClass)
      }
      if (comments != null && !comments.isEmpty) {
        v = v.withOrigin(v.origin().prependComments(new ArrayList[String](comments)))
        comments.clear()
      }
      if (arrayCount != startingArrayCount) throw new ConfigException.BugOrBroken("Bug in config parser: unbalanced array count")
      v
    }

    private def parseInclude(values: Map[String, AbstractConfigValue], n: ConfigNodeInclude) {
      var obj: AbstractConfigObject = null
      n.kind() match {
        case URL => 
          var url: URL = null
          url = new URL(n.name())
          obj = includer.includeURL(includeContext, url).asInstanceOf[AbstractConfigObject]

        case FILE => obj = includer.includeFile(includeContext, new File(n.name())).asInstanceOf[AbstractConfigObject]
        case CLASSPATH => obj = includer.includeResources(includeContext, n.name()).asInstanceOf[AbstractConfigObject]
        case HEURISTIC => obj = includer.include(includeContext, n.name()).asInstanceOf[AbstractConfigObject]
        case _ => throw new ConfigException.BugOrBroken("should not be reached")
      }
      if (arrayCount > 0 && obj.resolveStatus() != ResolveStatus.RESOLVED) throw parseError("Due to current limitations of the config parser, when an include statement is nested inside a list value, " + 
        "${} substitutions inside the included file cannot be resolved correctly. Either move the include outside of the list value or " + 
        "remove the ${} statements from the included file.")
      if (!pathStack.isEmpty) {
        val prefix = fullCurrentPath()
        obj = obj.relativized(prefix)
      }
      for (key <- obj.keySet) {
        val v = obj.get(key)
        val existing = values.get(key)
        if (existing != null) {
          values.put(key, v.withFallback(existing))
        } else {
          values.put(key, v)
        }
      }
    }

    private def parseObject(n: ConfigNodeObject): AbstractConfigObject = {
      val values = new HashMap[String, AbstractConfigValue]()
      val objectOrigin = lineOrigin()
      var lastWasNewline = false
      val nodes = new ArrayList[AbstractConfigNode](n.children())
      val comments = new ArrayList[String]()
      for (i <- 0 until nodes.size) {
        val node = nodes.get(i)
        if (node.isInstanceOf[ConfigNodeComment]) {
          lastWasNewline = false
          comments.add(node.asInstanceOf[ConfigNodeComment].commentText())
        } else if (node.isInstanceOf[ConfigNodeSingleToken] && 
          Tokens.isNewline(node.asInstanceOf[ConfigNodeSingleToken].token())) {
          lineNumber += 1
          if (lastWasNewline) {
            comments.clear()
          }
          lastWasNewline = true
        } else if (flavor != ConfigSyntax.JSON && node.isInstanceOf[ConfigNodeInclude]) {
          parseInclude(values, node.asInstanceOf[ConfigNodeInclude])
          lastWasNewline = false
        } else if (node.isInstanceOf[ConfigNodeField]) {
          lastWasNewline = false
          val path = node.asInstanceOf[ConfigNodeField].path().value()
          comments.addAll(node.asInstanceOf[ConfigNodeField].comments())
          pathStack.push(path)
          if (node.asInstanceOf[ConfigNodeField].separator() == Tokens.PLUS_EQUALS) {
            if (arrayCount > 0) throw parseError("Due to current limitations of the config parser, += does not work nested inside a list. " + 
              "+= expands to a ${} substitution and the path in ${} cannot currently refer to list elements. " + 
              "You might be able to move the += outside of the list and then refer to it from inside the list with ${}.")
            arrayCount += 1
          }
          var valueNode: AbstractConfigNodeValue = null
          var newValue: AbstractConfigValue = null
          valueNode = node.asInstanceOf[ConfigNodeField].value()
          newValue = parseValue(valueNode, comments)
          if (node.asInstanceOf[ConfigNodeField].separator() == Tokens.PLUS_EQUALS) {
            arrayCount -= 1
            val concat = new ArrayList[AbstractConfigValue](2)
            val previousRef = new ConfigReference(newValue.origin(), new SubstitutionExpression(fullCurrentPath(), 
              true))
            val list = new SimpleConfigList(newValue.origin(), Collections.singletonList(newValue))
            concat.add(previousRef)
            concat.add(list)
            newValue = ConfigConcatenation.concatenate(concat)
          }
          if (i < nodes.size - 1) {
            i += 1
            while (i < nodes.size) {
              if (nodes.get(i).isInstanceOf[ConfigNodeComment]) {
                val comment = nodes.get(i).asInstanceOf[ConfigNodeComment]
                newValue = newValue.withOrigin(newValue.origin().appendComments(Collections.singletonList(comment.commentText())))
                //break
              } else if (nodes.get(i).isInstanceOf[ConfigNodeSingleToken]) {
                val curr = nodes.get(i).asInstanceOf[ConfigNodeSingleToken]
                if (curr.token() == Tokens.COMMA || Tokens.isIgnoredWhitespace(curr.token())) {
                } else {
                  i -= 1
                  //break
                }
              } else {
                i -= 1
                //break
              }
              i += 1
            }
          }
          pathStack.pop()
          val key = path.first()
          val remaining = path.remainder()
          if (remaining == null) {
            val existing = values.get(key)
            if (existing != null) {
              if (flavor == ConfigSyntax.JSON) {
                throw parseError("JSON does not allow duplicate fields: '" + key + "' was already seen at " + 
                  existing.origin().description())
              } else {
                newValue = newValue.withFallback(existing)
              }
            }
            values.put(key, newValue)
          } else {
            if (flavor == ConfigSyntax.JSON) {
              throw new ConfigException.BugOrBroken("somehow got multi-element path in JSON mode")
            }
            var obj = createValueUnderPath(remaining, newValue)
            val existing = values.get(key)
            if (existing != null) {
              obj = obj.withFallback(existing)
            }
            values.put(key, obj)
          }
        }
      }
      new SimpleConfigObject(objectOrigin, values)
    }

    private def parseArray(n: ConfigNodeArray): SimpleConfigList = {
      arrayCount += 1
      val arrayOrigin = lineOrigin()
      val values = new ArrayList[AbstractConfigValue]()
      var lastWasNewLine = false
      val comments = new ArrayList[String]()
      var v: AbstractConfigValue = null
      for (node <- n.children()) {
        if (node.isInstanceOf[ConfigNodeComment]) {
          comments.add(node.asInstanceOf[ConfigNodeComment].commentText())
          lastWasNewLine = false
        } else if (node.isInstanceOf[ConfigNodeSingleToken] && 
          Tokens.isNewline(node.asInstanceOf[ConfigNodeSingleToken].token())) {
          lineNumber += 1
          if (lastWasNewLine && v == null) {
            comments.clear()
          } else if (v != null) {
            values.add(v.withOrigin(v.origin().appendComments(new ArrayList[String](comments))))
            comments.clear()
            v = null
          }
          lastWasNewLine = true
        } else if (node.isInstanceOf[AbstractConfigNodeValue]) {
          lastWasNewLine = false
          if (v != null) {
            values.add(v.withOrigin(v.origin().appendComments(new ArrayList[String](comments))))
            comments.clear()
          }
          v = parseValue(node.asInstanceOf[AbstractConfigNodeValue], comments)
        }
      }
      if (v != null) {
        values.add(v.withOrigin(v.origin().appendComments(new ArrayList[String](comments))))
      }
      arrayCount -= 1
      new SimpleConfigList(arrayOrigin, values)
    }

    def parse(): AbstractConfigValue = {
      var result: AbstractConfigValue = null
      val comments = new ArrayList[String]()
      var lastWasNewLine = false
      for (node <- document.children()) {
        if (node.isInstanceOf[ConfigNodeComment]) {
          comments.add(node.asInstanceOf[ConfigNodeComment].commentText())
          lastWasNewLine = false
        } else if (node.isInstanceOf[ConfigNodeSingleToken]) {
          val t = node.asInstanceOf[ConfigNodeSingleToken].token()
          if (Tokens.isNewline(t)) {
            lineNumber += 1
            if (lastWasNewLine && result == null) {
              comments.clear()
            } else if (result != null) {
              result = result.withOrigin(result.origin().appendComments(new ArrayList[String](comments)))
              comments.clear()
              //break
            }
            lastWasNewLine = true
          }
        } else if (node.isInstanceOf[ConfigNodeComplexValue]) {
          result = parseValue(node.asInstanceOf[ConfigNodeComplexValue], comments)
          lastWasNewLine = false
        }
      }
      result
    }
  }
}
