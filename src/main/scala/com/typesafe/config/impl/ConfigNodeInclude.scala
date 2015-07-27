package com.typesafe.config.impl

import java.util.ArrayList
import java.util.Collection
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodeInclude(children: Collection[AbstractConfigNode], protected var kind: ConfigIncludeKind)
    extends AbstractConfigNode {

  val children = new ArrayList[AbstractConfigNode](children)

  protected override def tokens(): Collection[Token] = {
    val tokens = new ArrayList[Token]()
    for (child <- children) {
      tokens.addAll(child.tokens())
    }
    tokens
  }

  protected def name(): String = {
    children.find(_.isInstanceOf[ConfigNodeSimpleValue])
      .map(Tokens.getValue(_.asInstanceOf[ConfigNodeSimpleValue].token())
      .unwrapped().asInstanceOf[String])
      .getOrElse(null)
  }
}
