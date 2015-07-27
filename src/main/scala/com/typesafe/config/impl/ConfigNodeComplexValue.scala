package com.typesafe.config.impl

import java.util._
//remove if not needed
import scala.collection.JavaConversions._

abstract class ConfigNodeComplexValue(children: Collection[AbstractConfigNode])
    extends AbstractConfigNodeValue {

  protected val children = new ArrayList[AbstractConfigNode](children)

  def children(): Collection[AbstractConfigNode] = children

  protected override def tokens(): Collection[Token] = {
    val tokens = new ArrayList[Token]()
    for (child <- children) {
      tokens.addAll(child.tokens())
    }
    tokens
  }

  protected def indentText(indentation: AbstractConfigNode): ConfigNodeComplexValue = {
    val childrenCopy = new ArrayList[AbstractConfigNode](children)
    for (i <- 0 until childrenCopy.size) {
      val child = childrenCopy.get(i)
      if (child.isInstanceOf[ConfigNodeSingleToken] && 
        Tokens.isNewline(child.asInstanceOf[ConfigNodeSingleToken].token())) {
        childrenCopy.add(i + 1, indentation)
        i += 1
      } else if (child.isInstanceOf[ConfigNodeField]) {
        val value = child.asInstanceOf[ConfigNodeField].value()
        if (value.isInstanceOf[ConfigNodeComplexValue]) {
          childrenCopy.set(i, child.asInstanceOf[ConfigNodeField].replaceValue(value.asInstanceOf[ConfigNodeComplexValue].indentText(indentation)))
        }
      } else if (child.isInstanceOf[ConfigNodeComplexValue]) {
        childrenCopy.set(i, child.asInstanceOf[ConfigNodeComplexValue].indentText(indentation))
      }
    }
    newNode(childrenCopy)
  }

  def newNode(nodes: Collection[AbstractConfigNode]): ConfigNodeComplexValue
}
