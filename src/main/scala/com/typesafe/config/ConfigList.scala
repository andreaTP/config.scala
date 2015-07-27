package com.typesafe.config

import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Subtype of {@link ConfigValue} representing a list value, as in JSON's
 * {@code [1,2,3]} syntax.
 *
 * <p>
 * {@code ConfigList} implements {@code java.util.List<ConfigValue>} so you can
 * use it like a regular Java list. Or call {@link #unwrapped()} to unwrap the
 * list elements into plain Java values.
 *
 * <p>
 * Like all {@link ConfigValue} subtypes, {@code ConfigList} is immutable. This
 * makes it threadsafe and you never have to create "defensive copies." The
 * mutator methods from {@link java.util.List} all throw
 * {@link java.lang.UnsupportedOperationException}.
 *
 * <p>
 * The {@link ConfigValue#valueType} method on a list returns
 * {@link ConfigValueType#LIST}.
 *
 * <p>
 * <em>Do not implement {@code ConfigList}</em>; it should only be implemented
 * by the config library. Arbitrary implementations will not work because the
 * library internals assume a specific concrete implementation. Also, this
 * interface is likely to grow new methods over time, so third-party
 * implementations will break.
 *
 */
trait ConfigList extends List[ConfigValue] with ConfigValue {

  /**
   * Recursively unwraps the list, returning a list of plain Java values such
   * as Integer or String or whatever is in the list.
   */
  override def unwrapped(): List[Any]

  override def withOrigin(origin: ConfigOrigin): ConfigList
}
