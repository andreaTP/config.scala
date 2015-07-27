package com.typesafe.config.impl

import java.io.IOException
import java.io.Reader
import java.util.ArrayList
import java.util.Iterator
import java.util.LinkedList
import java.util.List
import java.util.Queue
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigSyntax
import TokenIterator._
//remove if not needed
import scala.collection.JavaConversions._

object Tokenizer {

  @SerialVersionUID(1L)
  private class ProblemException(var problem: Token) extends Exception

  private def asString(codepoint: Int): String = {
    if (codepoint == '\n') "newline" else if (codepoint == '\t') "tab" else if (codepoint == -1) "end of file" else if (ConfigImplUtil.isC0Control(codepoint)) String.format("control character 0x%x", 
      codepoint) else String.format("%c", codepoint)
  }

  /**
   * Tokenizes a Reader. Does not close the reader; you have to arrange to do
   * that after you're done with the returned iterator.
   */
  def tokenize(origin: ConfigOrigin, input: Reader, flavor: ConfigSyntax): Iterator[Token] = {
    new TokenIterator(origin, input, flavor != ConfigSyntax.JSON)
  }

  def render(tokens: Iterator[Token]): String = {
    val renderedText = new StringBuilder()
    while (tokens.hasNext) {
      renderedText.append(tokens.next().tokenText())
    }
    renderedText.toString
  }

  object TokenIterator {

    private class WhitespaceSaver() {

      private var whitespace: StringBuilder = new StringBuilder()

      private var lastTokenWasSimpleValue: Boolean = false

      def add(c: Int) {
        whitespace.appendCodePoint(c)
      }

      def check(t: Token, baseOrigin: ConfigOrigin, lineNumber: Int): Token = {
        if (isSimpleValue(t)) {
          nextIsASimpleValue(baseOrigin, lineNumber)
        } else {
          nextIsNotASimpleValue(baseOrigin, lineNumber)
        }
      }

      private def nextIsNotASimpleValue(baseOrigin: ConfigOrigin, lineNumber: Int): Token = {
        lastTokenWasSimpleValue = false
        createWhitespaceTokenFromSaver(baseOrigin, lineNumber)
      }

      private def nextIsASimpleValue(baseOrigin: ConfigOrigin, lineNumber: Int): Token = {
        val t = createWhitespaceTokenFromSaver(baseOrigin, lineNumber)
        if (!lastTokenWasSimpleValue) {
          lastTokenWasSimpleValue = true
        }
        t
      }

      private def createWhitespaceTokenFromSaver(baseOrigin: ConfigOrigin, lineNumber: Int): Token = {
        if (whitespace.length > 0) {
          var t: Token = null
          t = if (lastTokenWasSimpleValue) Tokens.newUnquotedText(lineOrigin(baseOrigin, lineNumber), 
            whitespace.toString) else Tokens.newIgnoredWhitespace(lineOrigin(baseOrigin, lineNumber), 
            whitespace.toString)
          whitespace.setLength(0)
          return t
        }
        null
      }
    }

    def isWhitespace(c: Int): Boolean = ConfigImplUtil.isWhitespace(c)

    def isWhitespaceNotNewline(c: Int): Boolean = {
      c != '\n' && ConfigImplUtil.isWhitespace(c)
    }

    private def problem(origin: ConfigOrigin, 
        what: String, 
        message: String, 
        cause: Throwable): ProblemException = {
      problem(origin, what, message, false, cause)
    }

    private def problem(origin: ConfigOrigin, 
        what: String, 
        message: String, 
        suggestQuotes: Boolean, 
        cause: Throwable): ProblemException = {
      if (what == null || message == null) throw new ConfigException.BugOrBroken("internal error, creating bad ProblemException")
      new ProblemException(Tokens.newProblem(origin, what, message, suggestQuotes, cause))
    }

    private def problem(origin: ConfigOrigin, message: String): ProblemException = problem(origin, "", message, null)

    private def lineOrigin(baseOrigin: ConfigOrigin, lineNumber: Int): ConfigOrigin = {
      baseOrigin.asInstanceOf[SimpleConfigOrigin].withLineNumber(lineNumber)
    }

    val firstNumberChars = "0123456789-"

    val numberChars = "0123456789eE+-."

    val notInUnquotedText = "$\"{}[]:=,+#`^?!@*&\\"

    private def isSimpleValue(t: Token): Boolean = {
      if (Tokens.isSubstitution(t) || Tokens.isUnquotedText(t) || 
        Tokens.isValue(t)) {
        true
      } else {
        false
      }
    }
  }

  private class TokenIterator(origin: ConfigOrigin, val input: Reader, val allowComments: Boolean)
      extends Iterator[Token] {

    private val origin = origin.asInstanceOf[SimpleConfigOrigin]

    private val buffer = new LinkedList[Integer]()

    private var lineNumber: Int = 1

    private var lineOrigin: ConfigOrigin = this.origin.withLineNumber(lineNumber)

    private val tokens = new LinkedList[Token]()

    private val whitespaceSaver = new WhitespaceSaver()

    tokens.add(Tokens.START)

    private def nextCharRaw(): Int = {
      if (buffer.isEmpty) {
        input.read()
      } else {
        val c = buffer.pop()
        c
      }
    }

    private def putBack(c: Int) {
      if (buffer.size > 2) {
        throw new ConfigException.BugOrBroken("bug: putBack() three times, undesirable look-ahead")
      }
      buffer.push(c)
    }

    private def startOfComment(c: Int): Boolean = {
      if (c == -1) {
        false
      } else {
        if (allowComments) {
          if (c == '#') {
            true
          } else if (c == '/') {
            val maybeSecondSlash = nextCharRaw()
            putBack(maybeSecondSlash)
            if (maybeSecondSlash == '/') {
              true
            } else {
              false
            }
          } else {
            false
          }
        } else {
          false
        }
      }
    }

    private def nextCharAfterWhitespace(saver: WhitespaceSaver): Int = {
      while (true) {
        val c = nextCharRaw()
        if (c == -1) {
          -1
        } else {
          if (isWhitespaceNotNewline(c)) {
            saver.add(c)
            //continue
          } else {
            c
          }
        }
      }
    }

    private def problem(message: String): ProblemException = problem("", message, null)

    private def problem(what: String, message: String): ProblemException = problem(what, message, null)

    private def problem(what: String, message: String, suggestQuotes: Boolean): ProblemException = {
      problem(what, message, suggestQuotes, null)
    }

    private def problem(what: String, message: String, cause: Throwable): ProblemException = {
      problem(lineOrigin, what, message, cause)
    }

    private def problem(what: String, 
        message: String, 
        suggestQuotes: Boolean, 
        cause: Throwable): ProblemException = {
      problem(lineOrigin, what, message, suggestQuotes, cause)
    }

    private def pullComment(firstChar: Int): Token = {
      var doubleSlash = false
      if (firstChar == '/') {
        val discard = nextCharRaw()
        if (discard != '/') throw new ConfigException.BugOrBroken("called pullComment but // not seen")
        doubleSlash = true
      }
      val sb = new StringBuilder()
      while (true) {
        val c = nextCharRaw()
        if (c == -1 || c == '\n') {
          putBack(c)
          if (doubleSlash) Tokens.newCommentDoubleSlash(lineOrigin, sb.toString) else Tokens.newCommentHash(lineOrigin, 
            sb.toString)
        } else {
          sb.appendCodePoint(c)
        }
      }
    }

    private def pullUnquotedText(): Token = {
      val origin = lineOrigin
      val sb = new StringBuilder()
      var c = nextCharRaw()
      while (true) {
        if (c == -1) {
          //break
        } else if (notInUnquotedText.indexOf(c) >= 0) {
          //break
        } else if (isWhitespace(c)) {
          //break
        } else if (startOfComment(c)) {
          //break
        } else {
          sb.appendCodePoint(c)
        }
        if (sb.length == 4) {
          val s = sb.toString
          if (s == "true") return Tokens.newBoolean(origin, true) else if (s == "null") return Tokens.newNull(origin)
        } else if (sb.length == 5) {
          val s = sb.toString
          if (s == "false") return Tokens.newBoolean(origin, false)
        }
        c = nextCharRaw()
      }
      putBack(c)
      val s = sb.toString
      Tokens.newUnquotedText(origin, s)
    }

    private def pullNumber(firstChar: Int): Token = {
      val sb = new StringBuilder()
      sb.appendCodePoint(firstChar)
      var containedDecimalOrE = false
      var c = nextCharRaw()
      while (c != -1 && numberChars.indexOf(c) >= 0) {
        if (c == '.' || c == 'e' || c == 'E') containedDecimalOrE = true
        sb.appendCodePoint(c)
        c = nextCharRaw()
      }
      putBack(c)
      val s = sb.toString
      try {
        if (containedDecimalOrE) {
          Tokens.newDouble(lineOrigin, Double.parseDouble(s), s)
        } else {
          Tokens.newLong(lineOrigin, Long.parseLong(s), s)
        }
      } catch {
        case e: NumberFormatException => {
          for (u <- s.toCharArray() if notInUnquotedText.indexOf(u) >= 0) throw problem(asString(u), 
            "Reserved character '" + asString(u) + "' is not allowed outside quotes", true)
          Tokens.newUnquotedText(lineOrigin, s)
        }
      }
    }

    private def pullEscapeSequence(sb: StringBuilder, sbOrig: StringBuilder) {
      val escaped = nextCharRaw()
      if (escaped == -1) throw problem("End of input but backslash in string had nothing after it")
      sbOrig.appendCodePoint('\\')
      sbOrig.appendCodePoint(escaped)
      escaped match {
        case '"' => sb.append('"')
        case '\\' => sb.append('\\')
        case '/' => sb.append('/')
        case 'b' => sb.append('\b')
        case 'f' => sb.append('\f')
        case 'n' => sb.append('\n')
        case 'r' => sb.append('\r')
        case 't' => sb.append('\t')
        case 'u' => {
          val a = Array.ofDim[Char](4)
          for (i <- 0 until 4) {
            val c = nextCharRaw()
            if (c == -1) throw problem("End of input but expecting 4 hex digits for \\uXXXX escape")
            a(i) = c.toChar
          }
          val digits = new String(a)
          sbOrig.append(a)
          sb.appendCodePoint(Integer.parseInt(digits, 16))
        }
        case _ => throw problem(asString(escaped), String.format("backslash followed by '%s', this is not a valid escape sequence (quoted strings use JSON escaping, so use double-backslash \\\\ for literal backslash)", 
          asString(escaped)))
      }
    }

    private def appendTripleQuotedString(sb: StringBuilder, sbOrig: StringBuilder) {
      var consecutiveQuotes = 0
      while (true) {
        val c = nextCharRaw()
        if (c == '"') {
          consecutiveQuotes += 1
        } else if (consecutiveQuotes >= 3) {
          sb.setLength(sb.length - 3)
          putBack(c)
          //break
        } else {
          consecutiveQuotes = 0
          if (c == -1) throw problem("End of input but triple-quoted string was still open") else if (c == '\n') {
            lineNumber += 1
            lineOrigin = origin.withLineNumber(lineNumber)
          }
        }
        sb.appendCodePoint(c)
        sbOrig.appendCodePoint(c)
      }
    }

    private def pullQuotedString(): Token = {
      val sb = new StringBuilder()
      val sbOrig = new StringBuilder()
      sbOrig.appendCodePoint('"')
      while (true) {
        val c = nextCharRaw()
        if (c == -1) throw problem("End of input but string quote was still open")
        if (c == '\\') {
          pullEscapeSequence(sb, sbOrig)
        } else if (c == '"') {
          sbOrig.appendCodePoint(c)
          //break
        } else if (ConfigImplUtil.isC0Control(c)) {
          throw problem(asString(c), "JSON does not allow unescaped " + asString(c) + " in quoted strings, use a backslash escape")
        } else {
          sb.appendCodePoint(c)
          sbOrig.appendCodePoint(c)
        }
      }
      if (sb.length == 0) {
        val third = nextCharRaw()
        if (third == '"') {
          sbOrig.appendCodePoint(third)
          appendTripleQuotedString(sb, sbOrig)
        } else {
          putBack(third)
        }
      }
      Tokens.newString(lineOrigin, sb.toString, sbOrig.toString)
    }

    private def pullPlusEquals(): Token = {
      val c = nextCharRaw()
      if (c != '=') {
        throw problem(asString(c), "'+' not followed by =, '" + asString(c) + "' not allowed after '+'", 
          true)
      }
      Tokens.PLUS_EQUALS
    }

    private def pullSubstitution(): Token = {
      val origin = lineOrigin
      var c = nextCharRaw()
      if (c != '{') {
        throw problem(asString(c), "'$' not followed by {, '" + asString(c) + "' not allowed after '$'", 
          true)
      }
      var optional = false
      c = nextCharRaw()
      if (c == '?') {
        optional = true
      } else {
        putBack(c)
      }
      val saver = new WhitespaceSaver()
      val expression = new ArrayList[Token]()
      var t: Token = null
      do {
        t = pullNextToken(saver)
        if (t == Tokens.CLOSE_CURLY) {
          //break
        } else if (t == Tokens.END) {
          throw problem(origin, "Substitution ${ was not closed with a }")
        } else {
          val whitespace = saver.check(t, origin, lineNumber)
          if (whitespace != null) expression.add(whitespace)
          expression.add(t)
        }
      } while (true);
      Tokens.newSubstitution(origin, optional, expression)
    }

    private def pullNextToken(saver: WhitespaceSaver): Token = {
      val c = nextCharAfterWhitespace(saver)
      if (c == -1) {
        Tokens.END
      } else if (c == '\n') {
        val line = Tokens.newLine(lineOrigin)
        lineNumber += 1
        lineOrigin = origin.withLineNumber(lineNumber)
        line
      } else {
        var t: Token = null
        if (startOfComment(c)) {
          t = pullComment(c)
        } else c match {
          case '"' => t = pullQuotedString()
          case '$' => t = pullSubstitution()
          case ':' => t = Tokens.COLON
          case ',' => t = Tokens.COMMA
          case '=' => t = Tokens.EQUALS
          case '{' => t = Tokens.OPEN_CURLY
          case '}' => t = Tokens.CLOSE_CURLY
          case '[' => t = Tokens.OPEN_SQUARE
          case ']' => t = Tokens.CLOSE_SQUARE
          case '+' => t = pullPlusEquals()
          case _ => t = null
        }
        if (t == null) throw new ConfigException.BugOrBroken("bug: failed to generate next token")
        t
      }
    }

    private def queueNextToken() {
      val t = pullNextToken(whitespaceSaver)
      val whitespace = whitespaceSaver.check(t, origin, lineNumber)
      if (whitespace != null) tokens.add(whitespace)
      tokens.add(t)
    }

    override def hasNext(): Boolean = !tokens.isEmpty

    override def next(): Token = {
      val t = tokens.remove()
      if (tokens.isEmpty && t != Tokens.END) {
        try {
          queueNextToken()
        } catch {
          case e: ProblemException => tokens.add(e.problem())
        }
        if (tokens.isEmpty) throw new ConfigException.BugOrBroken("bug: tokens queue should not be empty here")
      }
      t
    }

    override def remove() {
      throw new UnsupportedOperationException("Does not make sense to remove items from token stream")
    }
  }
}
