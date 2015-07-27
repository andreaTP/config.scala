package com.typesafe.config

import java.net.URL
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Implement this <em>in addition to</em> {@link ConfigIncluder} if you want to
 * support inclusion of files with the {@code include url("http://example.com")}
 * syntax. If you do not implement this but do implement {@link ConfigIncluder},
 * attempts to load URLs will use the default includer.
 */
trait ConfigIncluderURL {

  /**
   * Parses another item to be included. The returned object typically would
   * not have substitutions resolved. You can throw a ConfigException here to
   * abort parsing, or return an empty object, but may not return null.
   *
   * @param context
   *            some info about the include context
   * @param what
   *            the include statement's argument
   * @return a non-null ConfigObject
   */
  def includeURL(context: ConfigIncludeContext, what: URL): ConfigObject
}
