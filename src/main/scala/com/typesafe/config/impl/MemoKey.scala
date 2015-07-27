package com.typesafe.config.impl

//remove if not needed
import scala.collection.JavaConversions._

/**
 The key used to memoize already-traversed nodes when resolving substitutions
 */
class MemoKey(val value: AbstractConfigValue, val restrictToChildOrNull: Path)
    {

  override def hashCode(): Int = {
    val h = System.identityHashCode(value)
    if (restrictToChildOrNull != null) {
      h + 41 * (41 + restrictToChildOrNull.hashCode)
    } else {
      h
    }
  }

  override def equals(other: Any): Boolean = other match {
    case other: MemoKey => {
      val o = other
      if (o.value != this.value) false else if (o.restrictToChildOrNull == this.restrictToChildOrNull) true else if (o.restrictToChildOrNull == null || this.restrictToChildOrNull == null) false else o.restrictToChildOrNull == this.restrictToChildOrNull
    }
    case _ => false
  }

  override def toString(): String = {
    "MemoKey(" + value + "@" + System.identityHashCode(value) + 
      "," + 
      restrictToChildOrNull + 
      ")"
  }
}
