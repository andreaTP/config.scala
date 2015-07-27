package com.typesafe.config.impl

import java.io.IOException
import java.io.Reader
import java.util.ArrayList
import java.util.Collections
import java.util.Comparator
import java.util.HashMap
import java.util.HashSet
import java.util.List
import java.util.Map
import java.util.Properties
import java.util.Set
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
//remove if not needed
import scala.collection.JavaConversions._

object PropertiesParser {

  def parse(reader: Reader, origin: ConfigOrigin): AbstractConfigObject = {
    val props = new Properties()
    props.load(reader)
    fromProperties(origin, props)
  }

  def lastElement(path: String): String = {
    val i = path.lastIndexOf('.')
    if (i < 0) path else path.substring(i + 1)
  }

  def exceptLastElement(path: String): String = {
    val i = path.lastIndexOf('.')
    if (i < 0) null else path.substring(0, i)
  }

  def pathFromPropertyKey(key: String): Path = {
    var last = lastElement(key)
    var exceptLast = exceptLastElement(key)
    var path = new Path(last, null)
    while (exceptLast != null) {
      last = lastElement(exceptLast)
      exceptLast = exceptLastElement(exceptLast)
      path = new Path(last, path)
    }
    path
  }

  def fromProperties(origin: ConfigOrigin, props: Properties): AbstractConfigObject = {
    val pathMap = new HashMap[Path, Any]()
    for ((key, value) <- props) {
      val key = key
      if (key.isInstanceOf[String]) {
        val path = pathFromPropertyKey(key.asInstanceOf[String])
        pathMap.put(path, value)
      }
    }
    fromPathMap(origin, pathMap, true)
  }

  def fromPathMap(origin: ConfigOrigin, pathExpressionMap: Map[_, _]): AbstractConfigObject = {
    val pathMap = new HashMap[Path, Any]()
    for ((key, value) <- pathExpressionMap) {
      val keyObj = key
      if (!(keyObj.isInstanceOf[String])) {
        throw new ConfigException.BugOrBroken("Map has a non-string as a key, expecting a path expression as a String")
      }
      val path = Path.newPath(keyObj.asInstanceOf[String])
      pathMap.put(path, value)
    }
    fromPathMap(origin, pathMap, false)
  }

  private def fromPathMap(origin: ConfigOrigin, pathMap: Map[Path, Any], convertedFromProperties: Boolean): AbstractConfigObject = {
    val scopePaths = new HashSet[Path]()
    val valuePaths = new HashSet[Path]()
    for (path <- pathMap.keySet) {
      valuePaths.add(path)
      var next = path.parent()
      while (next != null) {
        scopePaths.add(next)
        next = next.parent()
      }
    }
    if (convertedFromProperties) {
      valuePaths.removeAll(scopePaths)
    } else {
      for (path <- valuePaths if scopePaths.contains(path)) {
        throw new ConfigException.BugOrBroken("In the map, path '" + path.render() + 
          "' occurs as both the parent object of a value and as a value. " + 
          "Because Map has no defined ordering, this is a broken situation.")
      }
    }
    val root = new HashMap[String, AbstractConfigValue]()
    val scopes = new HashMap[Path, Map[String, AbstractConfigValue]]()
    for (path <- scopePaths) {
      val scope = new HashMap[String, AbstractConfigValue]()
      scopes.put(path, scope)
    }
    for (path <- valuePaths) {
      val parentPath = path.parent()
      val parent = if (parentPath != null) scopes.get(parentPath) else root
      val last = path.last()
      val rawValue = pathMap.get(path)
      var value: AbstractConfigValue = null
      value = if (convertedFromProperties) if (rawValue.isInstanceOf[String]) new ConfigString.Quoted(origin, 
        rawValue.asInstanceOf[String]) else null else ConfigImpl.fromAnyRef(pathMap.get(path), origin, 
        FromMapMode.KEYS_ARE_PATHS)
      if (value != null) parent.put(last, value)
    }
    val sortedScopePaths = new ArrayList[Path]()
    sortedScopePaths.addAll(scopePaths)
    Collections.sort(sortedScopePaths, new Comparator[Path]() {

      override def compare(a: Path, b: Path): Int = return b.length - a.length
    })
    for (scopePath <- sortedScopePaths) {
      val scope = scopes.get(scopePath)
      val parentPath = scopePath.parent()
      val parent = if (parentPath != null) scopes.get(parentPath) else root
      val o = new SimpleConfigObject(origin, scope, ResolveStatus.RESOLVED, false)
      parent.put(scopePath.last(), o)
    }
    new SimpleConfigObject(origin, root, ResolveStatus.RESOLVED, false)
  }
}
