package com.typesafe.config.impl

import java.util.HashMap
import java.util.Map
//remove if not needed
import scala.collection.JavaConversions._

/**
 * This exists because we have to memoize resolved substitutions as we go
 * through the config tree; otherwise we could end up creating multiple copies
 * of values or whole trees of values as we follow chains of substitutions.
 */
class ResolveMemos private (val memos: Map[MemoKey, AbstractConfigValue]) {

  def this() {
    this(new HashMap[MemoKey, AbstractConfigValue]())
  }

  def get(key: MemoKey): AbstractConfigValue = memos.get(key)

  def put(key: MemoKey, value: AbstractConfigValue): ResolveMemos = {
    val copy = new HashMap[MemoKey, AbstractConfigValue](memos)
    copy.put(key, value)
    new ResolveMemos(copy)
  }
}
