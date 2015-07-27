package com.typesafe.config.impl

import java.util.Collection
import java.util.Collections
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodeSingleToken(val token: Token) extends AbstractConfigNode {

  protected override def tokens(): Collection[Token] = Collections.singletonList(token)

  protected def token(): Token = token
}
