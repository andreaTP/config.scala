package com.typesafe.config.impl

import java.util._
import com.typesafe.config.ConfigException
import Path._
//remove if not needed
import scala.collection.JavaConversions._

object Path {

  def hasFunkyChars(s: String): Boolean = !s.forall(c => Character.isLetterOrDigit(c) || c == '-' || c == '_')

  def newKey(key: String): Path = new Path(key, null)

  def newPath(path: String): Path = PathParser.parsePath(path)
}

class Path(var first: String, var remainder: Path) {

  def this(elements: String*) {
    this()
    if (elements.length == 0) throw new ConfigException.BugOrBroken("empty path")
    this.first = elements(0)
    if (elements.length > 1) {
      val pb = new PathBuilder()
      for (i <- 1 until elements.length) {
        pb.appendKey(elements(i))
      }
      this.remainder = pb.result()
    } else {
      this.remainder = null
    }
  }

  def this(pathsToConcat: List[Path]) {
    this(pathsToConcat.iterator())
  }

  def this(i: Iterator[Path]) {
    this()
    if (!i.hasNext) throw new ConfigException.BugOrBroken("empty path")
    val firstPath = i.next()
    this.first = firstPath.first
    val pb = new PathBuilder()
    if (firstPath.remainder != null) {
      pb.appendPath(firstPath.remainder)
    }
    while (i.hasNext) {
      pb.appendPath(i.next())
    }
    this.remainder = pb.result()
  }

  /**
   *
   * @return path minus the last element or null if we have just one element
   */
  def parent(): Path = {
    if (remainder == null) return null
    val pb = new PathBuilder()
    var p = this
    while (p.remainder != null) {
      pb.appendKey(p.first)
      p = p.remainder
    }
    pb.result()
  }

  /**
   *
   * @return last element in the path
   */
  def last(): String = {
    var p = this
    while (p.remainder != null) {
      p = p.remainder
    }
    p.first
  }

  def prepend(toPrepend: Path): Path = {
    val pb = new PathBuilder()
    pb.appendPath(toPrepend)
    pb.appendPath(this)
    pb.result()
  }

  def length(): Int = {
    var count = 1
    var p = remainder
    while (p != null) {
      count += 1
      p = p.remainder
    }
    count
  }

  def subPath(removeFromFront: Int): Path = {
    var count = removeFromFront
    var p = this
    while (p != null && count > 0) {
      count -= 1
      p = p.remainder
    }
    p
  }

  def subPath(firstIndex: Int, lastIndex: Int): Path = {
    if (lastIndex < firstIndex) throw new ConfigException.BugOrBroken("bad call to subPath")
    var from = subPath(firstIndex)
    val pb = new PathBuilder()
    var count = lastIndex - firstIndex
    while (count > 0) {
      count -= 1
      pb.appendKey(from.first())
      from = from.remainder()
      if (from == null) throw new ConfigException.BugOrBroken("subPath lastIndex out of range " + lastIndex)
    }
    pb.result()
  }

  def startsWith(other: Path): Boolean = {
    var myRemainder = this
    var otherRemainder = other
    if (otherRemainder.length <= myRemainder.length) {
      while (otherRemainder != null) {
        if (otherRemainder.first() != myRemainder.first()) return false
        myRemainder = myRemainder.remainder()
        otherRemainder = otherRemainder.remainder()
      }
      return true
    }
    false
  }

  override def equals(other: Any): Boolean = other match {
    case other: Path => {
      val that = other
      this.first == that.first &&
        ConfigImplUtil.equalsHandlingNull(this.remainder, that.remainder)
    }
    case _ => false
  }

  override def hashCode(): Int = {
    41 * (41 + first.hashCode) + (if (remainder == null) 0 else remainder.hashCode)
  }

  private def appendToStringBuilder(sb: StringBuilder) {
    if (hasFunkyChars(first) || first.isEmpty) sb.append(ConfigImplUtil.renderJsonString(first)) else sb.append(first)
    if (remainder != null) {
      sb.append(".")
      remainder.appendToStringBuilder(sb)
    }
  }

  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append("Path(")
    appendToStringBuilder(sb)
    sb.append(")")
    sb.toString
  }

  /**
   * toString() is a debugging-oriented version while this is an
   * error-message-oriented human-readable one.
   */
  def render(): String = {
    val sb = new StringBuilder()
    appendToStringBuilder(sb)
    sb.toString
  }
}
