package com.typesafe.config

import ConfigMemorySize._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigMemorySize {

  /**
   * Constructs a ConfigMemorySize representing the given
   * number of bytes.
   * @since 1.3.0
   * @param bytes a number of bytes
   * @return an instance representing the number of bytes
   */
  def ofBytes(bytes: Long): ConfigMemorySize = new ConfigMemorySize(bytes)
}

/**
 * An immutable class representing an amount of memory.  Use
 * static factory methods such as {@link
 * ConfigMemorySize#ofBytes(long)} to create instances.
 *
 * @since 1.3.0
 */
class ConfigMemorySize private (val bytes: Long) {

  if (bytes < 0) throw new IllegalArgumentException("Attempt to construct ConfigMemorySize with negative number: " + 
    bytes)

  /**
   * Gets the size in bytes.
   * @since 1.3.0
   * @return how many bytes
   */
  def toBytes(): Long = bytes

  override def toString(): String = "ConfigMemorySize(" + bytes + ")"

  override def equals(other: Any): Boolean = other match {
    case other: ConfigMemorySize => other.bytes == this.bytes
    case _ => false
  }

  override def hashCode(): Int = bytes.hashCode
}
