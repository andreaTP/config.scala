package com.typesafe.config.impl

import com.typesafe.config.ConfigException
import java.util.ArrayList
import java.util.Collection
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodePath(val path: Path, tokens: Collection[Token]) extends AbstractConfigNode {

  val tokens = new ArrayList[Token](tokens)

  protected override def tokens(): Collection[Token] = tokens

  protected def value(): Path = path

  protected def subPath(toRemove: Int): ConfigNodePath = {
    var periodCount = 0
    val tokensCopy = new ArrayList[Token](tokens)
    for (i <- 0 until tokensCopy.size) {
      if (Tokens.isUnquotedText(tokensCopy.get(i)) && tokensCopy.get(i).tokenText() == ".") periodCount += 1
      if (periodCount == toRemove) {
        return new ConfigNodePath(path.subPath(toRemove), tokensCopy.subList(i + 1, tokensCopy.size))
      }
    }
    throw new ConfigException.BugOrBroken("Tried to remove too many elements from a Path node")
  }

  protected def first(): ConfigNodePath = {
    val tokensCopy = new ArrayList[Token](tokens)
    for (i <- 0 until tokensCopy.size if Tokens.isUnquotedText(tokensCopy.get(i)) && tokensCopy.get(i).tokenText() == ".") return new ConfigNodePath(path.subPath(0, 
      1), tokensCopy.subList(0, i))
    this
  }
}
