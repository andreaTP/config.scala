package com.typesafe.config.impl

import com.typesafe.config.ConfigSyntax
import java.util.ArrayList
import java.util.Collection
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodeObject(children: Collection[AbstractConfigNode]) extends ConfigNodeComplexValue(children) {

  protected override def newNode(nodes: Collection[AbstractConfigNode]): ConfigNodeObject = new ConfigNodeObject(nodes)

  def hasValue(desiredPath: Path): Boolean = {
    for (node <- children if node.isInstanceOf[ConfigNodeField]) {
      val field = node.asInstanceOf[ConfigNodeField]
      val key = field.path().value()
      if (key == desiredPath || key.startsWith(desiredPath)) {
        return true
      } else if (desiredPath.startsWith(key)) {
        if (field.value().isInstanceOf[ConfigNodeObject]) {
          val obj = field.value().asInstanceOf[ConfigNodeObject]
          val remainingPath = desiredPath.subPath(key.length)
          if (obj.hasValue(remainingPath)) {
            return true
          }
        }
      }
    }
    false
  }

  protected def changeValueOnPath(desiredPath: Path, value: AbstractConfigNodeValue, flavor: ConfigSyntax): ConfigNodeObject = {
    val childrenCopy = new ArrayList[AbstractConfigNode](super.children)
    var seenNonMatching = false
    var valueCopy = value
    var i = childrenCopy.size - 1
    while (i >= 0) {
      if (childrenCopy.get(i).isInstanceOf[ConfigNodeSingleToken]) {
        val t = childrenCopy.get(i).asInstanceOf[ConfigNodeSingleToken]
          .token()
        if (flavor == ConfigSyntax.JSON && !seenNonMatching && t == Tokens.COMMA) {
          childrenCopy.remove(i)
        }
        //continue
      } else if (!(childrenCopy.get(i).isInstanceOf[ConfigNodeField])) {
        //continue
      }
      val node = childrenCopy.get(i).asInstanceOf[ConfigNodeField]
      val key = node.path().value()
      if ((valueCopy == null && key == desiredPath) || 
        (key.startsWith(desiredPath) && key != desiredPath)) {
        childrenCopy.remove(i)
        for (j <- i until childrenCopy.size) {
          if (childrenCopy.get(j).isInstanceOf[ConfigNodeSingleToken]) {
            val t = childrenCopy.get(j).asInstanceOf[ConfigNodeSingleToken]
              .token()
            if (Tokens.isIgnoredWhitespace(t) || t == Tokens.COMMA) {
              childrenCopy.remove(j)
              j -= 1
            } else {
              //break
            }
          } else {
            //break
          }
        }
      } else if (key == desiredPath) {
        seenNonMatching = true
        var indentedValue: AbstractConfigNodeValue = null
        val before = if (i - 1 > 0) childrenCopy.get(i - 1) else null
        indentedValue = if (value.isInstanceOf[ConfigNodeComplexValue] && before.isInstanceOf[ConfigNodeSingleToken] && 
          Tokens.isIgnoredWhitespace(before.asInstanceOf[ConfigNodeSingleToken].token())) value.asInstanceOf[ConfigNodeComplexValue].indentText(before) else value
        childrenCopy.set(i, node.replaceValue(indentedValue))
        valueCopy = null
      } else if (desiredPath.startsWith(key)) {
        seenNonMatching = true
        if (node.value().isInstanceOf[ConfigNodeObject]) {
          val remainingPath = desiredPath.subPath(key.length)
          childrenCopy.set(i, node.replaceValue(node.value().asInstanceOf[ConfigNodeObject].changeValueOnPath(remainingPath, 
            valueCopy, flavor)))
          if (valueCopy != null && node != super.children.get(i)) valueCopy = null
        }
      } else {
        seenNonMatching = true
      }
      i -= 1
    }
    new ConfigNodeObject(childrenCopy)
  }

  def setValueOnPath(desiredPath: String, value: AbstractConfigNodeValue): ConfigNodeObject = {
    setValueOnPath(desiredPath, value, ConfigSyntax.CONF)
  }

  def setValueOnPath(desiredPath: String, value: AbstractConfigNodeValue, flavor: ConfigSyntax): ConfigNodeObject = {
    val path = PathParser.parsePathNode(desiredPath, flavor)
    setValueOnPath(path, value, flavor)
  }

  private def setValueOnPath(desiredPath: ConfigNodePath, value: AbstractConfigNodeValue, flavor: ConfigSyntax): ConfigNodeObject = {
    val node = changeValueOnPath(desiredPath.value(), value, flavor)
    if (!node.hasValue(desiredPath.value())) {
      return node.addValueOnPath(desiredPath, value, flavor)
    }
    node
  }

  private def indentation(): Collection[AbstractConfigNode] = {
    var seenNewLine = false
    val indentation = new ArrayList[AbstractConfigNode]()
    if (children.isEmpty) {
      return indentation
    }
    for (i <- 0 until children.size) {
      if (!seenNewLine) {
        if (children.get(i).isInstanceOf[ConfigNodeSingleToken] && 
          Tokens.isNewline(children.get(i).asInstanceOf[ConfigNodeSingleToken]
          .token())) {
          seenNewLine = true
          indentation.add(new ConfigNodeSingleToken(Tokens.newLine(null)))
        }
      } else {
        if (children.get(i).isInstanceOf[ConfigNodeSingleToken] && 
          Tokens.isIgnoredWhitespace(children.get(i).asInstanceOf[ConfigNodeSingleToken]
          .token()) && 
          i + 1 < children.size && 
          (children.get(i + 1).isInstanceOf[ConfigNodeField] || 
          children.get(i + 1).isInstanceOf[ConfigNodeInclude])) {
          indentation.add(children.get(i))
          return indentation
        }
      }
    }
    if (indentation.isEmpty) {
      indentation.add(new ConfigNodeSingleToken(Tokens.newIgnoredWhitespace(null, " ")))
    } else {
      val last = children.get(children.size - 1)
      if (last.isInstanceOf[ConfigNodeSingleToken] && 
        last.asInstanceOf[ConfigNodeSingleToken].token() == Tokens.CLOSE_CURLY) {
        val beforeLast = children.get(children.size - 2)
        var indent = ""
        if (beforeLast.isInstanceOf[ConfigNodeSingleToken] && 
          Tokens.isIgnoredWhitespace(beforeLast.asInstanceOf[ConfigNodeSingleToken].token())) indent = beforeLast.asInstanceOf[ConfigNodeSingleToken].token()
          .tokenText()
        indent += "  "
        indentation.add(new ConfigNodeSingleToken(Tokens.newIgnoredWhitespace(null, indent)))
        return indentation
      }
    }
    indentation
  }

  protected def addValueOnPath(desiredPath: ConfigNodePath, value: AbstractConfigNodeValue, flavor: ConfigSyntax): ConfigNodeObject = {
    val path = desiredPath.value()
    val childrenCopy = new ArrayList[AbstractConfigNode](super.children)
    val indentation = new ArrayList[AbstractConfigNode](indentation())
    var indentedValue: AbstractConfigNodeValue = null
    indentedValue = if (value.isInstanceOf[ConfigNodeComplexValue] && !indentation.isEmpty) value.asInstanceOf[ConfigNodeComplexValue].indentText(indentation.get(indentation.size - 1)) else value
    val sameLine = !(indentation.size > 0 && 
      indentation.get(0).isInstanceOf[ConfigNodeSingleToken] && 
      Tokens.isNewline(indentation.get(0).asInstanceOf[ConfigNodeSingleToken]
      .token()))
    if (path.length > 1) {
      var i = super.children.size - 1
      while (i >= 0) {
        if (!(super.children.get(i).isInstanceOf[ConfigNodeField])) {
          //continue
        }
        val node = super.children.get(i).asInstanceOf[ConfigNodeField]
        val key = node.path().value()
        if (path.startsWith(key) && node.value().isInstanceOf[ConfigNodeObject]) {
          val remainingPath = desiredPath.subPath(key.length)
          val newValue = node.value().asInstanceOf[ConfigNodeObject]
          childrenCopy.set(i, node.replaceValue(newValue.addValueOnPath(remainingPath, value, flavor)))
          return new ConfigNodeObject(childrenCopy)
        }
        i -= 1
      }
    }
    val startsWithBrace = !super.children.isEmpty && 
      super.children.get(0).isInstanceOf[ConfigNodeSingleToken] && 
      super.children.get(0).asInstanceOf[ConfigNodeSingleToken]
      .token() == 
      Tokens.OPEN_CURLY
    val newNodes = new ArrayList[AbstractConfigNode]()
    newNodes.addAll(indentation)
    newNodes.add(desiredPath.first())
    newNodes.add(new ConfigNodeSingleToken(Tokens.newIgnoredWhitespace(null, " ")))
    newNodes.add(new ConfigNodeSingleToken(Tokens.COLON))
    newNodes.add(new ConfigNodeSingleToken(Tokens.newIgnoredWhitespace(null, " ")))
    if (path.length == 1) {
      newNodes.add(indentedValue)
    } else {
      val newObjectNodes = new ArrayList[AbstractConfigNode]()
      newObjectNodes.add(new ConfigNodeSingleToken(Tokens.OPEN_CURLY))
      if (indentation.isEmpty) {
        newObjectNodes.add(new ConfigNodeSingleToken(Tokens.newLine(null)))
      }
      newObjectNodes.addAll(indentation)
      newObjectNodes.add(new ConfigNodeSingleToken(Tokens.CLOSE_CURLY))
      val newObject = new ConfigNodeObject(newObjectNodes)
      newNodes.add(newObject.addValueOnPath(desiredPath.subPath(1), indentedValue, flavor))
    }
    if (flavor == ConfigSyntax.JSON || startsWithBrace || sameLine) {
      var i = childrenCopy.size - 1
      while (i >= 0) {
        if ((flavor == ConfigSyntax.JSON || sameLine) && childrenCopy.get(i).isInstanceOf[ConfigNodeField]) {
          if (i + 1 >= childrenCopy.size || 
            !(childrenCopy.get(i + 1).isInstanceOf[ConfigNodeSingleToken] && 
            childrenCopy.get(i + 1).asInstanceOf[ConfigNodeSingleToken]
            .token() == 
            Tokens.COMMA)) childrenCopy.add(i + 1, new ConfigNodeSingleToken(Tokens.COMMA))
          //break
        }
        if (startsWithBrace && 
          childrenCopy.get(i).isInstanceOf[ConfigNodeSingleToken] && 
          childrenCopy.get(i).asInstanceOf[ConfigNodeSingleToken].token == 
          Tokens.CLOSE_CURLY) {
          val previous = childrenCopy.get(i - 1)
          if (previous.isInstanceOf[ConfigNodeSingleToken] && 
            Tokens.isNewline(previous.asInstanceOf[ConfigNodeSingleToken].token())) {
            childrenCopy.add(i - 1, new ConfigNodeField(newNodes))
            i -= 1
          } else if (previous.isInstanceOf[ConfigNodeSingleToken] && 
            Tokens.isIgnoredWhitespace(previous.asInstanceOf[ConfigNodeSingleToken].token())) {
            val beforePrevious = childrenCopy.get(i - 2)
            if (sameLine) {
              childrenCopy.add(i - 1, new ConfigNodeField(newNodes))
              i -= 1
            } else if (beforePrevious.isInstanceOf[ConfigNodeSingleToken] && 
              Tokens.isNewline(beforePrevious.asInstanceOf[ConfigNodeSingleToken].token())) {
              childrenCopy.add(i - 2, new ConfigNodeField(newNodes))
              i -= 2
            } else {
              childrenCopy.add(i, new ConfigNodeField(newNodes))
            }
          } else childrenCopy.add(i, new ConfigNodeField(newNodes))
        }
        i -= 1
      }
    }
    if (!startsWithBrace) {
      if (!childrenCopy.isEmpty && 
        childrenCopy.get(childrenCopy.size - 1).isInstanceOf[ConfigNodeSingleToken] && 
        Tokens.isNewline(childrenCopy.get(childrenCopy.size - 1).asInstanceOf[ConfigNodeSingleToken]
        .token())) childrenCopy.add(childrenCopy.size - 1, new ConfigNodeField(newNodes)) else childrenCopy.add(new ConfigNodeField(newNodes))
    }
    new ConfigNodeObject(childrenCopy)
  }

  def removeValueOnPath(desiredPath: String, flavor: ConfigSyntax): ConfigNodeObject = {
    val path = PathParser.parsePathNode(desiredPath, flavor).value()
    changeValueOnPath(path, null, flavor)
  }
}
