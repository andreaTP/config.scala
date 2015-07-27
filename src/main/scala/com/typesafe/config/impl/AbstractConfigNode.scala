package com.typesafe.config.impl

import com.typesafe.config.parser.ConfigNode
import java.util.Collection
//remove if not needed
import scala.collection.JavaConversions._

abstract class AbstractConfigNode extends ConfigNode {

  def tokens(): Collection[Token]

  def render(): String = {
    val origText = new StringBuilder()
    val tokens = tokens()
    for (t <- tokens) {
      origText.append(t.tokenText())
    }
    origText.toString
  }

  override def equals(other: Any): Boolean = other match {
    case other: AbstractConfigNode => render() == other.render()
    case _ => false
  }

  override def hashCode(): Int = render().hashCode
}
