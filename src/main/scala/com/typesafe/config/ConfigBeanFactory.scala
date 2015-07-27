package com.typesafe.config

import com.typesafe.config.impl.ConfigBeanImpl
//remove if not needed
import scala.collection.JavaConversions._

object ConfigBeanFactory {

  /**
   * Creates an instance of a class, initializing its fields from a {@link Config}.
   *
   * Example usage:
   *
   * <pre>
   * Config configSource = ConfigFactory.load().getConfig("foo");
   * FooConfig config = ConfigBeanFactory.create(configSource, FooConfig.class);
   * </pre>
   *
   * The Java class should follow JavaBean conventions. Field types
   * can be any of the types you can normally get from a {@link
   * Config}, including <code>java.time.Duration</code> or {@link
   * ConfigMemorySize}. Fields may also be another JavaBean-style
   * class.
   *
   * Fields are mapped to config by converting the config key to
   * camel case.  So the key <code>foo-bar</code> becomes JavaBean
   * setter <code>setFooBar</code>.
   *
   * @since 1.3.0
   *
   * @param config source of config information
   * @param clazz class to be instantiated
   * @param <T> the type of the class to be instantiated
   * @return an instance of the class populated with data from the config
   * @throws ConfigException.BadBean
   *     If something is wrong with the JavaBean
   * @throws ConfigException.ValidationFailed
   *     If the config doesn't conform to the bean's implied schema
   * @throws ConfigException
   *     Can throw the same exceptions as the getters on <code>Config</code>
   */
  def create[T](config: Config, clazz: Class[T]): T = {
    ConfigBeanImpl.createInternal(config, clazz)
  }
}