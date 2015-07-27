package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import ConfigNumber._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigNumber {

  def newNumber(origin: ConfigOrigin, number: Long, originalText: String): ConfigNumber = {
    if (number <= Integer.MAX_VALUE && number >= Integer.MIN_VALUE) new ConfigInt(origin, number.toInt, 
      originalText) else new ConfigLong(origin, number, originalText)
  }

  def newNumber(origin: ConfigOrigin, number: Double, originalText: String): ConfigNumber = {
    val asLong = number.toLong
    if (asLong == number) {
      newNumber(origin, asLong, originalText)
    } else {
      new ConfigDouble(origin, number, originalText)
    }
  }
}

@SerialVersionUID(2L)
abstract class ConfigNumber protected (origin: ConfigOrigin, protected val originalText: String)
    extends AbstractConfigValue(origin) with Serializable {

  override def unwrapped(): Number

  override def transformToString(): String = originalText

  def intValueRangeChecked(path: String): Int = {
    val l = longValue()
    if (l < Integer.MIN_VALUE || l > Integer.MAX_VALUE) {
      throw new ConfigException.WrongType(origin(), path, "32-bit integer", "out-of-range value " + l)
    }
    l.toInt
  }

  protected def longValue(): Long

  protected def doubleValue(): Double

  private def isWhole(): Boolean = {
    val asLong = longValue()
    asLong == doubleValue()
  }

  protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[ConfigNumber]

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[ConfigNumber] && canEqual(other)) {
      val n = other.asInstanceOf[ConfigNumber]
      if (isWhole) {
        n.isWhole && this.longValue() == n.longValue()
      } else {
        (!n.isWhole) && this.doubleValue() == n.doubleValue()
      }
    } else {
      false
    }
  }

  override def hashCode(): Int = {
    var asLong: Long = 0l
    asLong = if (isWhole) longValue() else Double.doubleToLongBits(doubleValue())
    (asLong ^ (asLong >>> 32)).toInt
  }

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)
}
