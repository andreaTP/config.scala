package com.typesafe.config.impl

import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigSyntax
import com.typesafe.config.ConfigSyntax._
import java.util.ArrayList
import java.util.Collection
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodeRoot(_children: Collection[AbstractConfigNode], val origin: ConfigOrigin)
    extends ConfigNodeComplexValue(children) {

  protected override def newNode(nodes: Collection[AbstractConfigNode]): ConfigNodeRoot = {
    throw new ConfigException.BugOrBroken("Tried to indent the root object")
  }

  protected def value(): ConfigNodeComplexValue = {
    for (node <- children if node.isInstanceOf[ConfigNodeComplexValue]) {
      return node.asInstanceOf[ConfigNodeComplexValue]
    }
    throw new ConfigException.BugOrBroken("ConfigNodeRoot did not contain a value")
  }

  protected def setValue(desiredPath: String, value: AbstractConfigNodeValue, flavor: ConfigSyntax): ConfigNodeRoot = {
    val childrenCopy = new ArrayList[AbstractConfigNode](_children)
    for (i <- 0 until childrenCopy.size) {
      val node = childrenCopy.get(i)
      if (node.isInstanceOf[ConfigNodeComplexValue]) {
        if (node.isInstanceOf[ConfigNodeArray]) {
          throw new ConfigException.WrongType(origin, "The ConfigDocument had an array at the root level, and values cannot be modified inside an array.")
        } else if (node.isInstanceOf[ConfigNodeObject]) {
          if (value == null) {
            childrenCopy.set(i, node.asInstanceOf[ConfigNodeObject].removeValueOnPath(desiredPath, flavor))
          } else {
            childrenCopy.set(i, node.asInstanceOf[ConfigNodeObject].setValueOnPath(desiredPath, value, 
              flavor))
          }
          return new ConfigNodeRoot(childrenCopy, origin)
        }
      }
    }
    throw new ConfigException.BugOrBroken("ConfigNodeRoot did not contain a value")
  }

  protected def hasValue(desiredPath: String): Boolean = {
    val path = PathParser.parsePath(desiredPath)
    val childrenCopy = new ArrayList[AbstractConfigNode](children)
    for (i <- 0 until childrenCopy.size) {
      val node = childrenCopy.get(i)
      if (node.isInstanceOf[ConfigNodeComplexValue]) {
        if (node.isInstanceOf[ConfigNodeArray]) {
          throw new ConfigException.WrongType(origin, "The ConfigDocument had an array at the root level, and values cannot be modified inside an array.")
        } else if (node.isInstanceOf[ConfigNodeObject]) {
          return node.asInstanceOf[ConfigNodeObject].hasValue(path)
        }
      }
    }
    throw new ConfigException.BugOrBroken("ConfigNodeRoot did not contain a value")
  }
}
