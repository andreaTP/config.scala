package com.typesafe.config.impl

import java.util.ArrayList
import java.util.Collections
import java.util.Comparator
import java.util.HashMap
import java.util.Map
import com.typesafe.config.ConfigValueType
//remove if not needed
import scala.collection.JavaConversions._

object DefaultTransformer {

  def transform(value: AbstractConfigValue, requested: ConfigValueType): AbstractConfigValue = {
    if (value.valueType() == ConfigValueType.STRING) {
      val s = value.unwrapped().asInstanceOf[String]
      requested match {
        case NUMBER =>
          try {
            val v = Long.parseLong(s)
            return new ConfigLong(value.origin(), v, s)
          } catch {
            case e: NumberFormatException =>
          }
          try {
            val v = Double.parseDouble(s)
            return new ConfigDouble(value.origin(), v, s)
          } catch {
            case e: NumberFormatException =>
          }

        case NULL => if (s == "null") return new ConfigNull(value.origin())
        case BOOLEAN => if (s == "true" || s == "yes" || s == "on") {
          return new ConfigBoolean(value.origin(), true)
        } else if (s == "false" || s == "no" || s == "off") {
          return new ConfigBoolean(value.origin(), false)
        }
        case LIST => //break
        case OBJECT => //break
        case STRING => //break
      }
    } else if (requested == ConfigValueType.STRING) value.valueType() match {
      case NUMBER | BOOLEAN => return new ConfigString.Quoted(value.origin(), value.transformToString())
      case NULL => //break
      case OBJECT => //break
      case LIST => //break
      case STRING => //break
    } else if (requested == ConfigValueType.LIST && value.valueType() == ConfigValueType.OBJECT) {
      val o = value.asInstanceOf[AbstractConfigObject]
      val values = new HashMap[Integer, AbstractConfigValue]()
      for (key <- o.keySet) {
        var i: Int = 0
        try {
          i = Integer.parseInt(key, 10)
          if (i < 0) //continue
          values.put(i, o.get(key))
        } catch {
          case e: NumberFormatException => //continue
        }
      }
      if (!values.isEmpty) {
        val entryList = new ArrayList[Map.Entry[Integer, AbstractConfigValue]](values.entrySet())
        Collections.sort(entryList, new Comparator[Map.Entry[Integer, AbstractConfigValue]]() {

          override def compare(a: Map.Entry[Integer, AbstractConfigValue], b: Map.Entry[Integer, AbstractConfigValue]): Int = {
            return Integer.compare(a.getKey, b.getKey)
          }
        })
        val list = new ArrayList[AbstractConfigValue]()
        for (entry <- entryList) {
          list.add(entry.getValue)
        }
        return new SimpleConfigList(value.origin(), list)
      }
    }
    value
  }
}
