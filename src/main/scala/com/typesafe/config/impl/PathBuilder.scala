package com.typesafe.config.impl

import java.util.Stack
import com.typesafe.config.ConfigException
//remove if not needed
import scala.collection.JavaConversions._

class PathBuilder() {

  private val keys = new Stack[String]()

  private var result: Path = _

  private def checkCanAppend() {
    if (result != null) throw new ConfigException.BugOrBroken("Adding to PathBuilder after getting result")
  }

  def appendKey(key: String) {
    checkCanAppend()
    keys.push(key)
  }

  def appendPath(path: Path) {
    checkCanAppend()
    var first = path.first()
    var remainder = path.remainder()
    while (true) {
      keys.push(first)
      if (remainder != null) {
        first = remainder.first()
        remainder = remainder.remainder()
      } else {
        //break
      }
    }
  }

  def result(): Path = {
    if (result == null) {
      var remainder: Path = null
      while (!keys.isEmpty) {
        val key = keys.pop()
        remainder = new Path(key, remainder)
      }
      result = remainder
    }
    result
  }
}
