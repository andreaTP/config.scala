package com.typesafe.config

import java.net.URL
import com.typesafe.config.impl.ConfigImpl
//remove if not needed
import scala.collection.JavaConversions._

object ConfigOriginFactory {

  /**
   * Returns the default origin for values when no other information is
   * provided. This is the origin used in {@link ConfigValueFactory
   * #fromAnyRef(Object)}.
   *
   * @since 1.3.0
   *
   * @return the default origin
   */
  def newSimple(): ConfigOrigin = newSimple(null)

  /**
   * Returns an origin with the given description.
   *
   *  @since 1.3.0
   *
   * @param description brief description of what the origin is
   * @return a new origin
   */
  def newSimple(description: String): ConfigOrigin = ConfigImpl.newSimpleOrigin(description)

  /**
   * Creates a file origin with the given filename.
   *
   * @since 1.3.0
   *
   * @param filename the filename of this origin
   * @return a new origin
   */
  def newFile(filename: String): ConfigOrigin = ConfigImpl.newFileOrigin(filename)

  /**
   * Creates a url origin with the given URL object.
   *
   * @since 1.3.0
   *
   * @param url the url of this origin
   * @return a new origin
   */
  def newURL(url: URL): ConfigOrigin = ConfigImpl.newURLOrigin(url)
}
