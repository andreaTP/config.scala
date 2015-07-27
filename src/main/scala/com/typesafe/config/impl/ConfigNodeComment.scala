package com.typesafe.config.impl

import com.typesafe.config.ConfigException
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodeComment(comment: Token) extends ConfigNodeSingleToken(comment) {

  if (!Tokens.isComment(super.token)) {
    throw new ConfigException.BugOrBroken("Tried to create a ConfigNodeComment from a non-comment token")
  }

  protected def commentText(): String = Tokens.getCommentText(super.token)
}
