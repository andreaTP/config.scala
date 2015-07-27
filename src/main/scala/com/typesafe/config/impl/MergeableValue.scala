package com.typesafe.config.impl

import com.typesafe.config.ConfigMergeable
import com.typesafe.config.ConfigValue
//remove if not needed
import scala.collection.JavaConversions._

trait MergeableValue extends ConfigMergeable {

  def toFallbackValue(): ConfigValue
}
