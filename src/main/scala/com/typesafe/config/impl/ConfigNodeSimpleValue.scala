package com.typesafe.config.impl

import com.typesafe.config.ConfigException
import java.util.Collection
import java.util.Collections
import java.util.List
//remove if not needed
import scala.collection.JavaConversions._

class ConfigNodeSimpleValue(val token: Token) extends AbstractConfigNodeValue {

  protected override def tokens(): Collection[Token] = Collections.singletonList(token)

  protected def token(): Token = token

  protected def value(): AbstractConfigValue = {
    if (Tokens.isValue(token)) return Tokens.getValue(token) else if (Tokens.isUnquotedText(token)) return new ConfigString.Unquoted(token.origin(), 
      Tokens.getUnquotedText(token)) else if (Tokens.isSubstitution(token)) {
      val expression = Tokens.getSubstitutionPathExpression(token)
      val path = PathParser.parsePathExpression(expression.iterator(), token.origin())
      val optional = Tokens.getSubstitutionOptional(token)
      return new ConfigReference(token.origin(), new SubstitutionExpression(path, optional))
    }
    throw new ConfigException.BugOrBroken("ConfigNodeSimpleValue did not contain a valid value token")
  }
}
