package com.typesafe.config.impl

import java.util.Collection
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodeArray(children: Collection[AbstractConfigNode]) extends ConfigNodeComplexValue(children) {

  protected override def newNode(nodes: Collection[AbstractConfigNode]): ConfigNodeArray = new ConfigNodeArray(nodes)
}
