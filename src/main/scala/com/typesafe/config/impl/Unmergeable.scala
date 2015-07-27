package com.typesafe.config.impl

import java.util.Collection
//remove if not needed
import scala.collection.JavaConversions._

/**
 * Interface that tags a ConfigValue that is not mergeable until after
 * substitutions are resolved. Basically these are special ConfigValue that
 * never appear in a resolved tree, like {@link ConfigSubstitution} and
 * {@link ConfigDelayedMerge}.
 */
trait Unmergeable {

  def unmergedValues(): Collection[_ <: AbstractConfigValue]
}
