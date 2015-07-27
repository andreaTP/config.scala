package com.typesafe.config

import ConfigResolveOptions._
import scala.reflect.{BeanProperty, BooleanBeanProperty}
//remove if not needed
import scala.collection.JavaConversions._

object ConfigResolveOptions {

  /**
   * Returns the default resolve options. By default the system environment
   * will be used and unresolved substitutions are not allowed.
   *
   * @return the default resolve options
   */
  def defaults(): ConfigResolveOptions = new ConfigResolveOptions(true, false)

  /**
   * Returns resolve options that disable any reference to "system" data
   * (currently, this means environment variables).
   *
   * @return the resolve options with env variables disabled
   */
  def noSystem(): ConfigResolveOptions = {
    defaults().setUseSystemEnvironment(false)
  }
}

/**
 * A set of options related to resolving substitutions. Substitutions use the
 * <code>${foo.bar}</code> syntax and are documented in the <a
 * href="https://github.com/typesafehub/config/blob/master/HOCON.md">HOCON</a>
 * spec.
 * <p>
 * Typically this class would be used with the method
 * {@link Config#resolve(ConfigResolveOptions)}.
 * <p>
 * This object is immutable, so the "setters" return a new object.
 * <p>
 * Here is an example of creating a custom {@code ConfigResolveOptions}:
 *
 * <pre>
 *     ConfigResolveOptions options = ConfigResolveOptions.defaults()
 *         .setUseSystemEnvironment(false)
 * </pre>
 * <p>
 * In addition to {@link ConfigResolveOptions#defaults}, there's a prebuilt
 * {@link ConfigResolveOptions#noSystem} which avoids looking at any system
 * environment variables or other external system information. (Right now,
 * environment variables are the only example.)
 */
class ConfigResolveOptions private (@BeanProperty val useSystemEnvironment: Boolean, @BeanProperty val allowUnresolved: Boolean)
    {

  /**
   * Returns options with use of environment variables set to the given value.
   *
   * @param value
   *            true to resolve substitutions falling back to environment
   *            variables.
   * @return options with requested setting for use of environment variables
   */
  def setUseSystemEnvironment(value: Boolean): ConfigResolveOptions = {
    new ConfigResolveOptions(value, allowUnresolved)
  }

  /**
   * Returns options with "allow unresolved" set to the given value. By
   * default, unresolved substitutions are an error. If unresolved
   * substitutions are allowed, then a future attempt to use the unresolved
   * value may fail, but {@link Config#resolve(ConfigResolveOptions)} itself
   * will not throw.
   *
   * @param value
   *            true to silently ignore unresolved substitutions.
   * @return options with requested setting for whether to allow substitutions
   * @since 1.2.0
   */
  def setAllowUnresolved(value: Boolean): ConfigResolveOptions = {
    new ConfigResolveOptions(useSystemEnvironment, value)
  }
}
