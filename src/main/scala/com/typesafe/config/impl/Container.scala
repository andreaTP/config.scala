package com.typesafe.config.impl

//remove if not needed
import scala.collection.JavaConversions._

/**
 * An AbstractConfigValue which contains other values. Java has no way to
 * express "this has to be an AbstractConfigValue also" other than making
 * AbstractConfigValue an interface which would be aggravating. But we can say
 * we are a ConfigValue.
 */
trait Container extends com.typesafe.config.ConfigValue {

  /**
   * Replace a child of this value. CAUTION if replacement is null, delete the
   * child, which may also delete the parent, or make the parent into a
   * non-container.
   */
  def replaceChild(child: AbstractConfigValue, replacement: AbstractConfigValue): AbstractConfigValue

  /**
   * Super-expensive full traversal to see if descendant is anywhere
   * underneath this container.
   */
  def hasDescendant(descendant: AbstractConfigValue): Boolean
}
