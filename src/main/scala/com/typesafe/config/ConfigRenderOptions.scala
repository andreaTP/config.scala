package com.typesafe.config

import ConfigRenderOptions._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object ConfigRenderOptions {

  /**
   * Returns the default render options which are verbose (commented and
   * formatted). See {@link ConfigRenderOptions#concise} for stripped-down
   * options. This rendering will not be valid JSON since it has comments.
   *
   * @return the default render options
   */
  def defaults(): ConfigRenderOptions = {
    new ConfigRenderOptions(true, true, true, true)
  }

  /**
   * Returns concise render options (no whitespace or comments). For a
   * resolved {@link Config}, the concise rendering will be valid JSON.
   *
   * @return the concise render options
   */
  def concise(): ConfigRenderOptions = {
    new ConfigRenderOptions(false, false, false, true)
  }
}

/**
 * <p>
 * A set of options related to rendering a {@link ConfigValue}. Passed to
 * {@link ConfigValue#render(ConfigRenderOptions)}.
 *
 * <p>
 * Here is an example of creating a {@code ConfigRenderOptions}:
 *
 * <pre>
 *     ConfigRenderOptions options =
 *         ConfigRenderOptions.defaults().setComments(false)
 * </pre>
 */
class ConfigRenderOptions private (@BeanProperty val originComments: Boolean, 
    @BeanProperty val comments: Boolean, 
    @BeanProperty val formatted: Boolean, 
    @BeanProperty val json: Boolean) {

  /**
   * Returns options with comments toggled. This controls human-written
   * comments but not the autogenerated "origin of this setting" comments,
   * which are controlled by {@link ConfigRenderOptions#setOriginComments}.
   *
   * @param value
   *            true to include comments in the render
   * @return options with requested setting for comments
   */
  def setComments(value: Boolean): ConfigRenderOptions = {
    if (value == comments) this else new ConfigRenderOptions(originComments, value, formatted, json)
  }

  /**
   * Returns options with origin comments toggled. If this is enabled, the
   * library generates comments for each setting based on the
   * {@link ConfigValue#origin} of that setting's value. For example these
   * comments might tell you which file a setting comes from.
   *
   * <p>
   * {@code setOriginComments()} controls only these autogenerated
   * "origin of this setting" comments, to toggle regular comments use
   * {@link ConfigRenderOptions#setComments}.
   *
   * @param value
   *            true to include autogenerated setting-origin comments in the
   *            render
   * @return options with origin comments toggled
   */
  def setOriginComments(value: Boolean): ConfigRenderOptions = {
    if (value == originComments) this else new ConfigRenderOptions(value, comments, formatted, json)
  }

  /**
   * Returns options with formatting toggled. Formatting means indentation and
   * whitespace, enabling formatting makes things prettier but larger.
   *
   * @param value
   *            true to enable formatting
   * @return options with requested setting for formatting
   */
  def setFormatted(value: Boolean): ConfigRenderOptions = {
    if (value == formatted) this else new ConfigRenderOptions(originComments, comments, value, json)
  }

  /**
   * Returns options with JSON toggled. JSON means that HOCON extensions
   * (omitting commas, quotes for example) won't be used. However, whether to
   * use comments is controlled by the separate {@link #setComments(boolean)}
   * and {@link #setOriginComments(boolean)} options. So if you enable
   * comments you will get invalid JSON despite setting this to true.
   *
   * @param value
   *            true to include non-JSON extensions in the render
   * @return options with requested setting for JSON
   */
  def setJson(value: Boolean): ConfigRenderOptions = {
    if (value == json) this else new ConfigRenderOptions(originComments, comments, formatted, value)
  }

  override def toString(): String = {
    val sb = new StringBuilder("ConfigRenderOptions(")
    if (originComments) sb.append("originComments,")
    if (comments) sb.append("comments,")
    if (formatted) sb.append("formatted,")
    if (json) sb.append("json,")
    if (sb.charAt(sb.length - 1) == ',') sb.setLength(sb.length - 1)
    sb.append(")")
    sb.toString
  }
}
