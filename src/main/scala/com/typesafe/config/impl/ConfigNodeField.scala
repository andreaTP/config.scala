package com.typesafe.config.impl

import com.typesafe.config.ConfigException
import java.util.ArrayList
import java.util.Collection
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodeField(children: Collection[AbstractConfigNode]) extends AbstractConfigNode {

  private val children = new ArrayList[AbstractConfigNode](children)

  protected override def tokens(): Collection[Token] = {
    val tokens = new ArrayList[Token]()
    for (child <- children) {
      tokens.addAll(child.tokens())
    }
    tokens
  }

  def replaceValue(newValue: AbstractConfigNodeValue): ConfigNodeField = {
    val childrenCopy = new ArrayList[AbstractConfigNode](children)
    for (i <- 0 until childrenCopy.size if childrenCopy.get(i).isInstanceOf[AbstractConfigNodeValue]) {
      childrenCopy.set(i, newValue)
      return new ConfigNodeField(childrenCopy)
    }
    throw new ConfigException.BugOrBroken("Field node doesn't have a value")
  }

  def value(): AbstractConfigNodeValue = {
    for (i <- 0 until children.size if children.get(i).isInstanceOf[AbstractConfigNodeValue]) {
      return children.get(i).asInstanceOf[AbstractConfigNodeValue]
    }
    throw new ConfigException.BugOrBroken("Field node doesn't have a value")
  }

  def path(): ConfigNodePath = {
    for (i <- 0 until children.size if children.get(i).isInstanceOf[ConfigNodePath]) {
      return children.get(i).asInstanceOf[ConfigNodePath]
    }
    throw new ConfigException.BugOrBroken("Field node doesn't have a path")
  }

  protected def separator(): Token = {
    for (child <- children if child.isInstanceOf[ConfigNodeSingleToken]) {
      val t = child.asInstanceOf[ConfigNodeSingleToken].token()
      if (t == Tokens.PLUS_EQUALS || t == Tokens.COLON || t == Tokens.EQUALS) {
        return t
      }
    }
    null
  }

  protected def comments(): List[String] = {
    val comments = new ArrayList[String]()
    for (child <- children if child.isInstanceOf[ConfigNodeComment]) {
      comments.add(child.asInstanceOf[ConfigNodeComment].commentText())
    }
    comments
  }
}
