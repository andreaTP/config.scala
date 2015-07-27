package com.typesafe.config.impl

import java.io.File
import java.io.IOException
import java.net.MalformedURLException
import java.net.URL
import java.util.ArrayList
import java.util.Collection
import java.util.Collections
import java.util.EnumMap
import java.util.Iterator
import java.util.List
import java.util.Map
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.impl.SerializedConfigValue.SerializedField
import SimpleConfigOrigin._
//remove if not needed
import scala.collection.JavaConversions._

object SimpleConfigOrigin {

  def newSimple(description: String): SimpleConfigOrigin = {
    new SimpleConfigOrigin(description, -1, -1, OriginType.GENERIC, null, null, null)
  }

  def newFile(filename: String): SimpleConfigOrigin = {
    var url: String = null
    try {
      url = (new File(filename)).toURI().toURL().toExternalForm()
    } catch {
      case e: MalformedURLException => url = null
    }
    new SimpleConfigOrigin(filename, -1, -1, OriginType.FILE, url, null, null)
  }

  def newURL(url: URL): SimpleConfigOrigin = {
    val u = url.toExternalForm()
    new SimpleConfigOrigin(u, -1, -1, OriginType.URL, u, null, null)
  }

  def newResource(resource: String, url: URL): SimpleConfigOrigin = {
    var desc: String = null
    desc = if (url != null) resource + " @ " + url.toExternalForm() else resource
    new SimpleConfigOrigin(desc, -1, -1, OriginType.RESOURCE, if (url != null) url.toExternalForm() else null, 
      resource, null)
  }

  def newResource(resource: String): SimpleConfigOrigin = newResource(resource, null)

  val MERGE_OF_PREFIX = "merge of "

  private def mergeTwo(a: SimpleConfigOrigin, b: SimpleConfigOrigin): SimpleConfigOrigin = {
    var mergedDesc: String = null
    var mergedStartLine: Int = 0
    var mergedEndLine: Int = 0
    var mergedComments: List[String] = null
    var mergedType: OriginType = null
    mergedType = if (a.originType == b.originType) a.originType else OriginType.GENERIC
    var aDesc = a.description
    var bDesc = b.description
    if (aDesc.startsWith(MERGE_OF_PREFIX)) aDesc = aDesc.substring(MERGE_OF_PREFIX.length)
    if (bDesc.startsWith(MERGE_OF_PREFIX)) bDesc = bDesc.substring(MERGE_OF_PREFIX.length)
    if (aDesc == bDesc) {
      mergedDesc = aDesc
      mergedStartLine = if (a.lineNumber < 0) b.lineNumber else if (b.lineNumber < 0) a.lineNumber else Math.min(a.lineNumber, 
        b.lineNumber)
      mergedEndLine = Math.max(a.endLineNumber, b.endLineNumber)
    } else {
      var aFull = a.description()
      var bFull = b.description()
      if (aFull.startsWith(MERGE_OF_PREFIX)) aFull = aFull.substring(MERGE_OF_PREFIX.length)
      if (bFull.startsWith(MERGE_OF_PREFIX)) bFull = bFull.substring(MERGE_OF_PREFIX.length)
      mergedDesc = MERGE_OF_PREFIX + aFull + "," + bFull
      mergedStartLine = -1
      mergedEndLine = -1
    }
    var mergedURL: String = null
    mergedURL = if (ConfigImplUtil.equalsHandlingNull(a.urlOrNull, b.urlOrNull)) a.urlOrNull else null
    var mergedResource: String = null
    mergedResource = if (ConfigImplUtil.equalsHandlingNull(a.resourceOrNull, b.resourceOrNull)) a.resourceOrNull else null
    if (ConfigImplUtil.equalsHandlingNull(a.commentsOrNull, b.commentsOrNull)) {
      mergedComments = a.commentsOrNull
    } else {
      mergedComments = new ArrayList[String]()
      if (a.commentsOrNull != null) mergedComments.addAll(a.commentsOrNull)
      if (b.commentsOrNull != null) mergedComments.addAll(b.commentsOrNull)
    }
    new SimpleConfigOrigin(mergedDesc, mergedStartLine, mergedEndLine, mergedType, mergedURL, mergedResource, 
      mergedComments)
  }

  private def similarity(a: SimpleConfigOrigin, b: SimpleConfigOrigin): Int = {
    var count = 0
    if (a.originType == b.originType) count += 1
    if (a.description == b.description) {
      count += 1
      if (a.lineNumber == b.lineNumber) count += 1
      if (a.endLineNumber == b.endLineNumber) count += 1
      if (ConfigImplUtil.equalsHandlingNull(a.urlOrNull, b.urlOrNull)) count += 1
      if (ConfigImplUtil.equalsHandlingNull(a.resourceOrNull, b.resourceOrNull)) count += 1
    }
    count
  }

  private def mergeThree(a: SimpleConfigOrigin, b: SimpleConfigOrigin, c: SimpleConfigOrigin): SimpleConfigOrigin = {
    if (similarity(a, b) >= similarity(b, c)) {
      mergeTwo(mergeTwo(a, b), c)
    } else {
      mergeTwo(a, mergeTwo(b, c))
    }
  }

  def mergeOrigins(a: ConfigOrigin, b: ConfigOrigin): ConfigOrigin = {
    mergeTwo(a.asInstanceOf[SimpleConfigOrigin], b.asInstanceOf[SimpleConfigOrigin])
  }

  def mergeOrigins(stack: List[_ <: AbstractConfigValue]): ConfigOrigin = {
    val origins = new ArrayList[ConfigOrigin](stack.size)
    for (v <- stack) {
      origins.add(v.origin())
    }
    mergeOrigins(origins)
  }

  def mergeOrigins(stack: Collection[_ <: ConfigOrigin]): ConfigOrigin = {
    if (stack.isEmpty) {
      throw new ConfigException.BugOrBroken("can't merge empty list of origins")
    } else if (stack.size == 1) {
      stack.iterator().next()
    } else if (stack.size == 2) {
      val i = stack.iterator()
      mergeTwo(i.next().asInstanceOf[SimpleConfigOrigin], i.next().asInstanceOf[SimpleConfigOrigin])
    } else {
      val remaining = new ArrayList[SimpleConfigOrigin]()
      for (o <- stack) {
        remaining.add(o.asInstanceOf[SimpleConfigOrigin])
      }
      while (remaining.size > 2) {
        val c = remaining.get(remaining.size - 1)
        remaining.remove(remaining.size - 1)
        val b = remaining.get(remaining.size - 1)
        remaining.remove(remaining.size - 1)
        val a = remaining.get(remaining.size - 1)
        remaining.remove(remaining.size - 1)
        val merged = mergeThree(a, b, c)
        remaining.add(merged)
      }
      mergeOrigins(remaining)
    }
  }

  def fieldsDelta(base: Map[SerializedField, Any], child: Map[SerializedField, Any]): Map[SerializedField, Any] = {
    val m = new EnumMap[SerializedField, Any](child)
    for ((key, value) <- base) {
      val f = key
      if (m.containsKey(f) && ConfigImplUtil.equalsHandlingNull(value, m.get(f))) {
        m.remove(f)
      } else if (!m.containsKey(f)) f match {
        case ORIGIN_DESCRIPTION => throw new ConfigException.BugOrBroken("origin missing description field? " + child)
        case ORIGIN_LINE_NUMBER => m.put(SerializedField.ORIGIN_LINE_NUMBER, -1)
        case ORIGIN_END_LINE_NUMBER => m.put(SerializedField.ORIGIN_END_LINE_NUMBER, -1)
        case ORIGIN_TYPE => throw new ConfigException.BugOrBroken("should always be an ORIGIN_TYPE field")
        case ORIGIN_URL => m.put(SerializedField.ORIGIN_NULL_URL, "")
        case ORIGIN_RESOURCE => m.put(SerializedField.ORIGIN_NULL_RESOURCE, "")
        case ORIGIN_COMMENTS => m.put(SerializedField.ORIGIN_NULL_COMMENTS, "")
        case ORIGIN_NULL_URL | ORIGIN_NULL_RESOURCE | ORIGIN_NULL_COMMENTS => throw new ConfigException.BugOrBroken("computing delta, base object should not contain " + f + 
          " " + 
          base)
        case END_MARKER | ROOT_VALUE | ROOT_WAS_CONFIG | UNKNOWN | VALUE_DATA | VALUE_ORIGIN => throw new ConfigException.BugOrBroken("should not appear here: " + f)
      } else {
      }
    }
    m
  }

  def fromFields(m: Map[SerializedField, Any]): SimpleConfigOrigin = {
    if (m.isEmpty) return null
    val description = m.get(SerializedField.ORIGIN_DESCRIPTION).asInstanceOf[String]
    val lineNumber = m.get(SerializedField.ORIGIN_LINE_NUMBER).asInstanceOf[java.lang.Integer]
    val endLineNumber = m.get(SerializedField.ORIGIN_END_LINE_NUMBER).asInstanceOf[java.lang.Integer]
    val originTypeOrdinal = m.get(SerializedField.ORIGIN_TYPE).asInstanceOf[Number]
    if (originTypeOrdinal == null) throw new IOException("Missing ORIGIN_TYPE field")
    val originType = OriginType.values()(originTypeOrdinal.byteValue())
    val urlOrNull = m.get(SerializedField.ORIGIN_URL).asInstanceOf[String]
    var resourceOrNull = m.get(SerializedField.ORIGIN_RESOURCE).asInstanceOf[String]
    val commentsOrNull = m.get(SerializedField.ORIGIN_COMMENTS).asInstanceOf[List[String]]
    if (originType == OriginType.RESOURCE && resourceOrNull == null) {
      resourceOrNull = description
    }
    new SimpleConfigOrigin(description, if (lineNumber != null) lineNumber else -1, if (endLineNumber != null) endLineNumber else -1, 
      originType, urlOrNull, resourceOrNull, commentsOrNull)
  }

  def applyFieldsDelta(base: Map[SerializedField, Any], delta: Map[SerializedField, Any]): Map[SerializedField, Any] = {
    val m = new EnumMap[SerializedField, Any](delta)
    for ((key, value) <- base) {
      val f = key
      if (delta.containsKey(f)) {
      } else f match {
        case ORIGIN_DESCRIPTION => m.put(f, base.get(f))
        case ORIGIN_URL => if (delta.containsKey(SerializedField.ORIGIN_NULL_URL)) {
          m.remove(SerializedField.ORIGIN_NULL_URL)
        } else {
          m.put(f, base.get(f))
        }
        case ORIGIN_RESOURCE => if (delta.containsKey(SerializedField.ORIGIN_NULL_RESOURCE)) {
          m.remove(SerializedField.ORIGIN_NULL_RESOURCE)
        } else {
          m.put(f, base.get(f))
        }
        case ORIGIN_COMMENTS => if (delta.containsKey(SerializedField.ORIGIN_NULL_COMMENTS)) {
          m.remove(SerializedField.ORIGIN_NULL_COMMENTS)
        } else {
          m.put(f, base.get(f))
        }
        case ORIGIN_NULL_URL | ORIGIN_NULL_RESOURCE | ORIGIN_NULL_COMMENTS => throw new ConfigException.BugOrBroken("applying fields, base object should not contain " + f + 
          " " + 
          base)
        case ORIGIN_END_LINE_NUMBER | ORIGIN_LINE_NUMBER | ORIGIN_TYPE => m.put(f, base.get(f))
        case END_MARKER | ROOT_VALUE | ROOT_WAS_CONFIG | UNKNOWN | VALUE_DATA | VALUE_ORIGIN => throw new ConfigException.BugOrBroken("should not appear here: " + f)
      }
    }
    m
  }

  def fromBase(baseOrigin: SimpleConfigOrigin, delta: Map[SerializedField, Any]): SimpleConfigOrigin = {
    var baseFields: Map[SerializedField, Any] = null
    baseFields = if (baseOrigin != null) baseOrigin.toFields() else Collections.emptyMap[SerializedField, Any]()
    val fields = applyFieldsDelta(baseFields, delta)
    fromFields(fields)
  }
}

class SimpleConfigOrigin protected (val description: String, 
    var lineNumber: Int, 
    val endLineNumber: Int, 
    val originType: OriginType, 
    val urlOrNull: String, 
    val resourceOrNull: String, 
    val commentsOrNull: List[String]) extends ConfigOrigin {

  if (description == null) throw new ConfigException.BugOrBroken("description may not be null")

  override def withLineNumber(lineNumber: Int): SimpleConfigOrigin = {
    if (lineNumber == this.lineNumber && lineNumber == this.endLineNumber) {
      this
    } else {
      new SimpleConfigOrigin(this.description, lineNumber, lineNumber, this.originType, this.urlOrNull, 
        this.resourceOrNull, this.commentsOrNull)
    }
  }

  def addURL(url: URL): SimpleConfigOrigin = {
    new SimpleConfigOrigin(this.description, this.lineNumber, this.endLineNumber, this.originType, if (url != null) url.toExternalForm() else null, 
      this.resourceOrNull, this.commentsOrNull)
  }

  override def withComments(comments: List[String]): SimpleConfigOrigin = {
    if (ConfigImplUtil.equalsHandlingNull(comments, this.commentsOrNull)) {
      this
    } else {
      new SimpleConfigOrigin(this.description, this.lineNumber, this.endLineNumber, this.originType, 
        this.urlOrNull, this.resourceOrNull, comments)
    }
  }

  def prependComments(comments: List[String]): SimpleConfigOrigin = {
    if (ConfigImplUtil.equalsHandlingNull(comments, this.commentsOrNull) || 
      comments == null) {
      this
    } else if (this.commentsOrNull == null) {
      withComments(comments)
    } else {
      val merged = new ArrayList[String](comments.size + this.commentsOrNull.size)
      merged.addAll(comments)
      merged.addAll(this.commentsOrNull)
      withComments(merged)
    }
  }

  def appendComments(comments: List[String]): SimpleConfigOrigin = {
    if (ConfigImplUtil.equalsHandlingNull(comments, this.commentsOrNull) || 
      comments == null) {
      this
    } else if (this.commentsOrNull == null) {
      withComments(comments)
    } else {
      val merged = new ArrayList[String](comments.size + this.commentsOrNull.size)
      merged.addAll(this.commentsOrNull)
      merged.addAll(comments)
      withComments(merged)
    }
  }

  override def description(): String = {
    if (lineNumber < 0) {
      description
    } else if (endLineNumber == lineNumber) {
      description + ": " + lineNumber
    } else {
      description + ": " + lineNumber + "-" + endLineNumber
    }
  }

  override def equals(other: Any): Boolean = other match {
    case other: SimpleConfigOrigin => {
      val otherOrigin = other
      this.description == otherOrigin.description && this.lineNumber == otherOrigin.lineNumber && 
        this.endLineNumber == otherOrigin.endLineNumber && 
        this.originType == otherOrigin.originType && 
        ConfigImplUtil.equalsHandlingNull(this.urlOrNull, otherOrigin.urlOrNull) && 
        ConfigImplUtil.equalsHandlingNull(this.resourceOrNull, otherOrigin.resourceOrNull)
    }
    case _ => false
  }

  override def hashCode(): Int = {
    var h = 41 * (41 + description.hashCode)
    h = 41 * (h + lineNumber)
    h = 41 * (h + endLineNumber)
    h = 41 * (h + originType.hashCode)
    if (urlOrNull != null) h = 41 * (h + urlOrNull.hashCode)
    if (resourceOrNull != null) h = 41 * (h + resourceOrNull.hashCode)
    h
  }

  override def toString(): String = "ConfigOrigin(" + description + ")"

  override def filename(): String = {
    if (originType == OriginType.FILE) {
      description
    } else if (urlOrNull != null) {
      var url: URL = null
      try {
        url = new URL(urlOrNull)
      } catch {
        case e: MalformedURLException => return null
      }
      if (url.getProtocol == "file") {
        url.getFile
      } else {
        null
      }
    } else {
      null
    }
  }

  override def url(): URL = {
    if (urlOrNull == null) {
      null
    } else {
      try {
        new URL(urlOrNull)
      } catch {
        case e: MalformedURLException => null
      }
    }
  }

  override def resource(): String = resourceOrNull

  override def comments(): List[String] = {
    if (commentsOrNull != null) {
      Collections.unmodifiableList(commentsOrNull)
    } else {
      Collections.emptyList()
    }
  }

  def toFields(): Map[SerializedField, Any] = {
    val m = new EnumMap[SerializedField, Any](classOf[SerializedField])
    m.put(SerializedField.ORIGIN_DESCRIPTION, description)
    if (lineNumber >= 0) m.put(SerializedField.ORIGIN_LINE_NUMBER, lineNumber)
    if (endLineNumber >= 0) m.put(SerializedField.ORIGIN_END_LINE_NUMBER, endLineNumber)
    m.put(SerializedField.ORIGIN_TYPE, originType.ordinal())
    if (urlOrNull != null) m.put(SerializedField.ORIGIN_URL, urlOrNull)
    if (resourceOrNull != null) m.put(SerializedField.ORIGIN_RESOURCE, resourceOrNull)
    if (commentsOrNull != null) m.put(SerializedField.ORIGIN_COMMENTS, commentsOrNull)
    m
  }

  def toFieldsDelta(baseOrigin: SimpleConfigOrigin): Map[SerializedField, Any] = {
    var baseFields: Map[SerializedField, Any] = null
    baseFields = if (baseOrigin != null) baseOrigin.toFields() else Collections.emptyMap[SerializedField, Any]()
    fieldsDelta(baseFields, toFields())
  }
}
