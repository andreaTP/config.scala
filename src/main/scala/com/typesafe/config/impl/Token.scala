package com.typesafe.config.impl

import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import Token._
//remove if not needed
import scala.collection.JavaConversions._

object Token {

  def newWithoutOrigin(tokenType: TokenType, debugString: String, tokenText: String): Token = {
    new Token(tokenType, null, tokenText, debugString)
  }
}

class Token(val tokenType: TokenType, 
    val origin: ConfigOrigin, 
    var tokenText: String, 
    val debugString: String) {

  def this(tokenType: TokenType, origin: ConfigOrigin) {
    this(tokenType, origin, null)
  }

  def this(tokenType: TokenType, origin: ConfigOrigin, tokenText: String) {
    this(tokenType, origin, tokenText, null)
  }

  def origin(): ConfigOrigin = {
    if (origin == null) throw new ConfigException.BugOrBroken("tried to get origin from token that doesn't have one: " + 
      this)
    origin
  }

  def lineNumber(): Int = {
    if (origin != null) origin.lineNumber() else -1
  }

  override def toString(): String = {
    if (debugString != null) debugString else tokenType.name()
  }

  protected def canEqual(other: AnyRef): Boolean = other.isInstanceOf[Token]

  override def equals(other: Any): Boolean = other match {
    case other: Token => canEqual(other) && this.tokenType == other.tokenType
    case _ => false
  }

  override def hashCode(): Int = tokenType.hashCode
}
