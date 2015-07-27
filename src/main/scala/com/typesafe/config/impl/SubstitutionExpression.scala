package com.typesafe.config.impl

//remove if not needed
import scala.collection.JavaConversions._

class SubstitutionExpression(var path: Path, var optional: Boolean) {

  def changePath(newPath: Path): SubstitutionExpression = {
    if (newPath == path) this else new SubstitutionExpression(newPath, optional)
  }

  override def toString(): String = {
    "${" + (if (optional) "?" else "") + path.render() + "}"
  }

  override def equals(other: Any): Boolean = other match {
    case other: SubstitutionExpression => {
      val otherExp = other
      otherExp.path == this.path && otherExp.optional == this.optional
    }
    case _ => false
  }

  override def hashCode(): Int = {
    var h = 41 * (41 + path.hashCode)
    h = 41 * (h + (if (optional) 1 else 0))
    h
  }
}
