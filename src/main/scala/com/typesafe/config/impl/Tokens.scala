package com.typesafe.config.impl

import java.util.List
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigValueType
import Comment._
//remove if not needed
import scala.collection.JavaConversions._

object Tokens {

  private class Value(var value: AbstractConfigValue, origText: String) extends Token(TokenType.VALUE,
    value.origin(), origText) {

    def this(value: AbstractConfigValue) {
      this(value, null)
    }

    override def toString(): String = {
      if (value().resolveStatus() == ResolveStatus.RESOLVED) "'" + value().unwrapped() + "' (" + value.valueType().name() +
        ")" else "'<unresolved value>' (" + value.valueType().name() +
        ")"
    }

    protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[Value]

    override def equals(other: Any): Boolean = {
      this == other && other.asInstanceOf[Value].value == value
    }

    override def hashCode(): Int = {
      41 * (41 + super.hashCode) + value.hashCode
    }
  }

  private class Line(origin: ConfigOrigin) extends Token(TokenType.NEWLINE, origin) {

    override def toString(): String = "'\\n'@" + lineNumber()

    protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[Line]

    override def equals(other: Any): Boolean = {
      this == other && other.asInstanceOf[Line].lineNumber() == lineNumber()
    }

    override def hashCode(): Int = {
      41 * (41 + super.hashCode) + lineNumber()
    }

    override def tokenText(): String = "\n"
  }

  private class UnquotedText(origin: ConfigOrigin, s: String) extends Token(TokenType.UNQUOTED_TEXT,
    origin) {

    var value: String = s

    override def toString(): String = "'" + value + "'"

    protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[UnquotedText]

    override def equals(other: Any): Boolean = {
      this == other && other.asInstanceOf[UnquotedText].value == value
    }

    override def hashCode(): Int = {
      41 * (41 + super.hashCode) + value.hashCode
    }

    override def tokenText(): String = value
  }

  private class IgnoredWhitespace(origin: ConfigOrigin, s: String) extends Token(TokenType.IGNORED_WHITESPACE,
    origin) {

    private val value = s

    override def toString(): String = "'" + value + "' (WHITESPACE)"

    protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[IgnoredWhitespace]

    override def equals(other: Any): Boolean = {
      this == other &&
        other.asInstanceOf[IgnoredWhitespace].value == value
    }

    override def hashCode(): Int = {
      41 * (41 + super.hashCode) + value.hashCode
    }

    override def tokenText(): String = value
  }

  private class Problem(origin: ConfigOrigin,
      var what: String,
      var message: String,
      var suggestQuotes: Boolean,
      var cause: Throwable) extends Token(TokenType.PROBLEM, origin) {

    override def toString(): String = {
      val sb = new StringBuilder()
      sb.append('\'')
      sb.append(what)
      sb.append('\'')
      sb.append(" (")
      sb.append(message)
      sb.append(")")
      sb.toString
    }

    protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[Problem]

    override def equals(other: Any): Boolean = {
      this == other && other.asInstanceOf[Problem].what == what &&
        other.asInstanceOf[Problem].message == message &&
        other.asInstanceOf[Problem].suggestQuotes == suggestQuotes &&
        ConfigImplUtil.equalsHandlingNull(other.asInstanceOf[Problem].cause, cause)
    }

    override def hashCode(): Int = {
      var h = 41 * (41 + super.hashCode)
      h = 41 * (h + what.hashCode)
      h = 41 * (h + message.hashCode)
      h = 41 * (h + suggestQuotes.hashCode)
      if (cause != null) h = 41 * (h + cause.hashCode)
      h
    }
  }

  object Comment {

    class DoubleSlashComment(origin: ConfigOrigin, text: String) extends Comment(origin, text) {

      override def tokenText(): String = "//" + super.text
    }

    class HashComment(origin: ConfigOrigin, text: String) extends Comment(origin, text) {

      override def tokenText(): String = "#" + super.text
    }
  }

  private abstract class Comment(origin: ConfigOrigin, var text: String) extends Token(TokenType.COMMENT,
    origin) {

    override def toString(): String = {
      val sb = new StringBuilder()
      sb.append("'#")
      sb.append(text)
      sb.append("' (COMMENT)")
      sb.toString
    }

    protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[Comment]

    override def equals(other: Any): Boolean = {
      this == other && other.asInstanceOf[Comment].text == text
    }

    override def hashCode(): Int = {
      var h = 41 * (41 + super.hashCode)
      h = 41 * (h + text.hashCode)
      h
    }
  }

  private class Substitution(origin: ConfigOrigin, var optional: Boolean, expression: List[Token])
      extends Token(TokenType.SUBSTITUTION, origin) {

    var value: List[Token] = expression

    override def tokenText(): String = {
      "${" + (if (this.optional) "?" else "") + Tokenizer.render(this.value.iterator()) +
        "}"
    }

    override def toString(): String = {
      val sb = new StringBuilder()
      for (t <- value) {
        sb.append(t.toString)
      }
      "'${" + sb.toString + "}'"
    }

    protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[Substitution]

    override def equals(other: Any): Boolean = {
      this == other && other.asInstanceOf[Substitution].value == value
    }

    override def hashCode(): Int = {
      41 * (41 + super.hashCode) + value.hashCode
    }
  }

  def isValue(token: Token): Boolean = token.isInstanceOf[Value]

  def getValue(token: Token): AbstractConfigValue = {
    if (token.isInstanceOf[Value]) {
      token.asInstanceOf[Value].value()
    } else {
      throw new ConfigException.BugOrBroken("tried to get value of non-value token " + token)
    }
  }

  def isValueWithType(t: Token, valueType: ConfigValueType): Boolean = {
    isValue(t) && getValue(t).valueType() == valueType
  }

  def isNewline(token: Token): Boolean = token.isInstanceOf[Line]

  def isProblem(token: Token): Boolean = token.isInstanceOf[Problem]

  def getProblemWhat(token: Token): String = {
    if (token.isInstanceOf[Problem]) {
      token.asInstanceOf[Problem].what()
    } else {
      throw new ConfigException.BugOrBroken("tried to get problem what from " + token)
    }
  }

  def getProblemMessage(token: Token): String = {
    if (token.isInstanceOf[Problem]) {
      token.asInstanceOf[Problem].message()
    } else {
      throw new ConfigException.BugOrBroken("tried to get problem message from " + token)
    }
  }

  def getProblemSuggestQuotes(token: Token): Boolean = {
    if (token.isInstanceOf[Problem]) {
      token.asInstanceOf[Problem].suggestQuotes()
    } else {
      throw new ConfigException.BugOrBroken("tried to get problem suggestQuotes from " + token)
    }
  }

  def getProblemCause(token: Token): Throwable = {
    if (token.isInstanceOf[Problem]) {
      token.asInstanceOf[Problem].cause()
    } else {
      throw new ConfigException.BugOrBroken("tried to get problem cause from " + token)
    }
  }

  def isComment(token: Token): Boolean = token.isInstanceOf[Comment]

  def getCommentText(token: Token): String = {
    if (token.isInstanceOf[Comment]) {
      token.asInstanceOf[Comment].text()
    } else {
      throw new ConfigException.BugOrBroken("tried to get comment text from " + token)
    }
  }

  def isUnquotedText(token: Token): Boolean = token.isInstanceOf[UnquotedText]

  def getUnquotedText(token: Token): String = {
    if (token.isInstanceOf[UnquotedText]) {
      token.asInstanceOf[UnquotedText].value()
    } else {
      throw new ConfigException.BugOrBroken("tried to get unquoted text from " + token)
    }
  }

  def isIgnoredWhitespace(token: Token): Boolean = token.isInstanceOf[IgnoredWhitespace]

  def isSubstitution(token: Token): Boolean = token.isInstanceOf[Substitution]

  def getSubstitutionPathExpression(token: Token): List[Token] = {
    if (token.isInstanceOf[Substitution]) {
      token.asInstanceOf[Substitution].value()
    } else {
      throw new ConfigException.BugOrBroken("tried to get substitution from " + token)
    }
  }

  def getSubstitutionOptional(token: Token): Boolean = {
    if (token.isInstanceOf[Substitution]) {
      token.asInstanceOf[Substitution].optional()
    } else {
      throw new ConfigException.BugOrBroken("tried to get substitution optionality from " + token)
    }
  }

  val START = Token.newWithoutOrigin(TokenType.START, "start of file", "")

  val END = Token.newWithoutOrigin(TokenType.END, "end of file", "")

  val COMMA = Token.newWithoutOrigin(TokenType.COMMA, "','", ",")

  val EQUALS = Token.newWithoutOrigin(TokenType.EQUALS, "'='", "=")

  val COLON = Token.newWithoutOrigin(TokenType.COLON, "':'", ":")

  val OPEN_CURLY = Token.newWithoutOrigin(TokenType.OPEN_CURLY, "'{'", "{")

  val CLOSE_CURLY = Token.newWithoutOrigin(TokenType.CLOSE_CURLY, "'}'", "}")

  val OPEN_SQUARE = Token.newWithoutOrigin(TokenType.OPEN_SQUARE, "'['", "[")

  val CLOSE_SQUARE = Token.newWithoutOrigin(TokenType.CLOSE_SQUARE, "']'", "]")

  val PLUS_EQUALS = Token.newWithoutOrigin(TokenType.PLUS_EQUALS, "'+='", "+=")

  def newLine(origin: ConfigOrigin): Token = new Line(origin)

  def newProblem(origin: ConfigOrigin,
      what: String,
      message: String,
      suggestQuotes: Boolean,
      cause: Throwable): Token = {
    new Problem(origin, what, message, suggestQuotes, cause)
  }

  def newCommentDoubleSlash(origin: ConfigOrigin, text: String): Token = {
    new Comment.DoubleSlashComment(origin, text)
  }

  def newCommentHash(origin: ConfigOrigin, text: String): Token = new Comment.HashComment(origin, text)

  def newUnquotedText(origin: ConfigOrigin, s: String): Token = new UnquotedText(origin, s)

  def newIgnoredWhitespace(origin: ConfigOrigin, s: String): Token = new IgnoredWhitespace(origin, s)

  def newSubstitution(origin: ConfigOrigin, optional: Boolean, expression: List[Token]): Token = {
    new Substitution(origin, optional, expression)
  }

  def newValue(value: AbstractConfigValue): Token = new Value(value)

  def newValue(value: AbstractConfigValue, origText: String): Token = new Value(value, origText)

  def newString(origin: ConfigOrigin, value: String, origText: String): Token = {
    newValue(new ConfigString.Quoted(origin, value), origText)
  }

  def newInt(origin: ConfigOrigin, value: Int, origText: String): Token = {
    newValue(ConfigNumber.newNumber(origin, value, origText), origText)
  }

  def newDouble(origin: ConfigOrigin, value: Double, origText: String): Token = {
    newValue(ConfigNumber.newNumber(origin, value, origText), origText)
  }

  def newLong(origin: ConfigOrigin, value: Long, origText: String): Token = {
    newValue(ConfigNumber.newNumber(origin, value, origText), origText)
  }

  def newNull(origin: ConfigOrigin): Token = {
    newValue(new ConfigNull(origin), "null")
  }

  def newBoolean(origin: ConfigOrigin, value: Boolean): Token = {
    newValue(new ConfigBoolean(origin, value), "" + value)
  }
}
