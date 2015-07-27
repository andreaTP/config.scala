package com.typesafe.config.impl

import java.io.DataOutputStream
import java.io.File
import java.io.IOException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.net.URISyntaxException
import java.net.URL
import java.util.ArrayList
import java.util.List
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
//remove if not needed
import scala.collection.JavaConversions._

object ConfigImplUtil {

  def equalsHandlingNull(a: AnyRef, b: AnyRef): Boolean = {
    if (a == null && b != null) false else if (a != null && b == null) false else if (a == b) true else a == b
  }

  def isC0Control(codepoint: Int): Boolean = {
    (codepoint >= 0x0000 && codepoint <= 0x001F)
  }

  def renderJsonString(s: String): String = {
    val sb = new StringBuilder()
    sb.append('"')
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      c match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\n' => sb.append("\\n")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case _ => if (isC0Control(c)) sb.append(String.format("\\u%04x", c.toInt)) else sb.append(c)
      }
    }
    sb.append('"')
    sb.toString
  }

  def renderStringUnquotedIfPossible(s: String): String = {
    if (s.length == 0) return renderJsonString(s)
    val first = s.codePointAt(0)
    if (Character.isDigit(first) || first == '-') return renderJsonString(s)
    if (s.startsWith("include") || s.startsWith("true") || s.startsWith("false") ||
      s.startsWith("null") ||
      s.contains("//")) return renderJsonString(s)
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      if (!(Character.isLetter(c) || Character.isDigit(c) || c == '-')) return renderJsonString(s)
    }
    s
  }

  def isWhitespace(codepoint: Int): Boolean = codepoint match {
    case ' ' | '\n' | ' ' | ' ' | ' ' | '﻿' => true
    case _ => Character.isWhitespace(codepoint)
  }

  def unicodeTrim(s: String): String = {
    val length = s.length
    if (length == 0) return s
    var start = 0
    var done1 = false
    while (start < length && !done1) {
      val c = s.charAt(start)
      if (c == ' ' || c == '\n') {
        start += 1
      } else {
        val cp = s.codePointAt(start)
        if (isWhitespace(cp)) start += Character.charCount(cp) else done1 = true
      }
    }
    var end = length
    var done2 = false
    while (end > start && !done2) {
      val c = s.charAt(end - 1)
      if (c == ' ' || c == '\n') {
        end
      } else {
        var cp: Int = 0
        var delta: Int = 0
        if (Character.isLowSurrogate(c)) {
          cp = s.codePointAt(end - 2)
          delta = 2
        } else {
          cp = s.codePointAt(end - 1)
          delta = 1
        }
        if (isWhitespace(cp)) end -= delta else done2 = true
      }
    }
    s.substring(start, end)
  }

  def extractInitializerError(e: ExceptionInInitializerError): ConfigException = {
    val cause = e.getCause
    if (cause != null && cause.isInstanceOf[ConfigException]) {
      cause.asInstanceOf[ConfigException]
    } else {
      throw e
    }
  }

  def urlToFile(url: URL): File = {
    try {
      new File(url.toURI())
    } catch {
      case e: URISyntaxException => new File(url.getPath)
      case e: IllegalArgumentException => new File(url.getPath)
    }
  }

  def joinPath(elements: String*): String = (new Path(elements)).render()

  def joinPath(elements: List[String]): String = {
    joinPath(elements.toArray(Array.ofDim[String](0)))
  }

  def splitPath(path: String): List[String] = {
    var p = Path.newPath(path)
    val elements = new ArrayList[String]()
    while (p != null) {
      elements.add(p.first())
      p = p.remainder()
    }
    elements
  }

  def readOrigin(in: ObjectInputStream): ConfigOrigin = {
    SerializedConfigValue.readOrigin(in, null)
  }

  def writeOrigin(out: ObjectOutputStream, origin: ConfigOrigin) {
    SerializedConfigValue.writeOrigin(new DataOutputStream(out), origin.asInstanceOf[SimpleConfigOrigin],
      null)
  }

  def toCamelCase(originalName: String): String = {
    val words = originalName.split("-+")
    val nameBuilder = new StringBuilder(originalName.length)
    for (word <- words) {
      if (nameBuilder.length == 0) {
        nameBuilder.append(word)
      } else {
        nameBuilder.append(word.substring(0, 1).toUpperCase())
        nameBuilder.append(word.substring(1))
      }
    }
    nameBuilder.toString
  }
}
