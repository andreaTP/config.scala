package com.typesafe.config.impl

//remove if not needed
import scala.collection.JavaConversions._

object TokenType extends Enumeration {

  val START = new TokenType()

  val END = new TokenType()

  val COMMA = new TokenType()

  val EQUALS = new TokenType()

  val COLON = new TokenType()

  val OPEN_CURLY = new TokenType()

  val CLOSE_CURLY = new TokenType()

  val OPEN_SQUARE = new TokenType()

  val CLOSE_SQUARE = new TokenType()

  val VALUE = new TokenType()

  val NEWLINE = new TokenType()

  val UNQUOTED_TEXT = new TokenType()

  val IGNORED_WHITESPACE = new TokenType()

  val SUBSTITUTION = new TokenType()

  val PROBLEM = new TokenType()

  val COMMENT = new TokenType()

  val PLUS_EQUALS = new TokenType()

  class TokenType extends Val

  implicit def convertValue(v: Value): TokenType = v.asInstanceOf[TokenType]
}
