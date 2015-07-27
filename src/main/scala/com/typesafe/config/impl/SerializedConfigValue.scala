package com.typesafe.config.impl

import java.io.ByteArrayOutputStream
import java.io.DataInput
import java.io.DataOutput
import java.io.DataOutputStream
import java.io.Externalizable
import java.io.IOException
import java.io.NotSerializableException
import java.io.ObjectInput
import java.io.ObjectOutput
import java.io.ObjectStreamException
import java.util.ArrayList
import java.util.Collections
import java.util.EnumMap
import java.util.HashMap
import java.util.List
import java.util.Map
import com.typesafe.config.Config
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigList
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigValue
import com.typesafe.config.ConfigValueType
import SerializedConfigValue._
//remove if not needed
import scala.collection.JavaConversions._

object SerializedConfigValue {

  object SerializedField extends Enumeration {

    val UNKNOWN = new SerializedField()

    val END_MARKER = new SerializedField()

    val ROOT_VALUE = new SerializedField()

    val ROOT_WAS_CONFIG = new SerializedField()

    val VALUE_DATA = new SerializedField()

    val VALUE_ORIGIN = new SerializedField()

    val ORIGIN_DESCRIPTION = new SerializedField()

    val ORIGIN_LINE_NUMBER = new SerializedField()

    val ORIGIN_END_LINE_NUMBER = new SerializedField()

    val ORIGIN_TYPE = new SerializedField()

    val ORIGIN_URL = new SerializedField()

    val ORIGIN_COMMENTS = new SerializedField()

    val ORIGIN_NULL_URL = new SerializedField()

    val ORIGIN_NULL_COMMENTS = new SerializedField()

    val ORIGIN_RESOURCE = new SerializedField()

    val ORIGIN_NULL_RESOURCE = new SerializedField()

    class SerializedField extends Val

    def forInt(b: Int): SerializedField = {
      if (b < values.length) values()(b) else UNKNOWN
    }

    implicit def convertValue(v: Value): SerializedField = v.asInstanceOf[SerializedField]
  }

  object SerializedValueType extends Enumeration {

    val NULL = new SerializedValueType(ConfigValueType.NULL)

    val BOOLEAN = new SerializedValueType(ConfigValueType.BOOLEAN)

    val INT = new SerializedValueType(ConfigValueType.NUMBER)

    val LONG = new SerializedValueType(ConfigValueType.NUMBER)

    val DOUBLE = new SerializedValueType(ConfigValueType.NUMBER)

    val STRING = new SerializedValueType(ConfigValueType.STRING)

    val LIST = new SerializedValueType(ConfigValueType.LIST)

    val OBJECT = new SerializedValueType(ConfigValueType.OBJECT)

    class SerializedValueType(var configType: ConfigValueType) extends Val

    def forInt(b: Int): SerializedValueType = {
      if (b < values.length) values()(b) else null
    }

    def forValue(value: ConfigValue): SerializedValueType = {
      val t = value.valueType()
      if (t == ConfigValueType.NUMBER) {
        if (value.isInstanceOf[ConfigInt]) return INT else if (value.isInstanceOf[ConfigLong]) return LONG else if (value.isInstanceOf[ConfigDouble]) return DOUBLE
      } else {
        for (st <- values if st.configType == t) return st
      }
      throw new ConfigException.BugOrBroken("don't know how to serialize " + value)
    }

    implicit def convertValue(v: Value): SerializedValueType = v.asInstanceOf[SerializedValueType]
  }

  private class FieldOut(val code: SerializedField) {

    val bytes = new ByteArrayOutputStream()

    val data = new DataOutputStream(bytes)
  }

  private def writeOriginField(out: DataOutput, code: SerializedField, v: AnyRef) = code match {
    case ORIGIN_DESCRIPTION => out.writeUTF(v.asInstanceOf[String])
    case ORIGIN_LINE_NUMBER => out.writeInt(v.asInstanceOf[java.lang.Integer])
    case ORIGIN_END_LINE_NUMBER => out.writeInt(v.asInstanceOf[java.lang.Integer])
    case ORIGIN_TYPE => out.writeByte(v.asInstanceOf[java.lang.Integer])
    case ORIGIN_URL => out.writeUTF(v.asInstanceOf[String])
    case ORIGIN_RESOURCE => out.writeUTF(v.asInstanceOf[String])
    case ORIGIN_COMMENTS =>
      var list = v.asInstanceOf[List[String]]
      var size = list.size
      out.writeInt(size)
      for (s <- list) {
        out.writeUTF(s)
      }

    case ORIGIN_NULL_URL | ORIGIN_NULL_RESOURCE | ORIGIN_NULL_COMMENTS => //break
    case _ => throw new IOException("Unhandled field from origin: " + code)
  }

  def writeOrigin(out: DataOutput, origin: SimpleConfigOrigin, baseOrigin: SimpleConfigOrigin) {
    var m: Map[SerializedField, Any] = null
    m = if (origin != null) origin.toFieldsDelta(baseOrigin) else Collections.emptyMap()
    for ((key, value) <- m) {
      val field = new FieldOut(key)
      val v = value
      writeOriginField(field.data, field.code, v)
      writeField(out, field)
    }
    writeEndMarker(out)
  }

  def readOrigin(in: DataInput, baseOrigin: SimpleConfigOrigin): SimpleConfigOrigin = {
    val m = new EnumMap[SerializedField, Any](classOf[SerializedField])
    while (true) {
      var v: AnyRef = null
      val field = readCode(in)
      field match {
        case END_MARKER => return SimpleConfigOrigin.fromBase(baseOrigin, m)
        case ORIGIN_DESCRIPTION =>
          in.readInt()
          v = in.readUTF()

        case ORIGIN_LINE_NUMBER =>
          in.readInt()
          v = in.readInt()

        case ORIGIN_END_LINE_NUMBER =>
          in.readInt()
          v = in.readInt()

        case ORIGIN_TYPE =>
          in.readInt()
          v = in.readUnsignedByte()

        case ORIGIN_URL =>
          in.readInt()
          v = in.readUTF()

        case ORIGIN_RESOURCE =>
          in.readInt()
          v = in.readUTF()

        case ORIGIN_COMMENTS =>
          in.readInt()
          var size = in.readInt()
          var list = new ArrayList[String](size)
          for (i <- 0 until size) {
            list.add(in.readUTF())
          }
          v = list

        case ORIGIN_NULL_URL | ORIGIN_NULL_RESOURCE | ORIGIN_NULL_COMMENTS =>
          in.readInt()
          v = ""

        case ROOT_VALUE | ROOT_WAS_CONFIG | VALUE_DATA | VALUE_ORIGIN => throw new IOException("Not expecting this field here: " + field)
        case UNKNOWN => skipField(in)
      }
      if (v != null) m.put(field, v)
    }
  }

  private def writeValueData(out: DataOutput, value: ConfigValue) {
    val st = SerializedValueType.forValue(value)
    out.writeByte(st.ordinal())
    st match {
      case BOOLEAN => out.writeBoolean(value.asInstanceOf[ConfigBoolean].unwrapped())
      case NULL => //break
      case INT =>
        out.writeInt(value.asInstanceOf[ConfigInt].unwrapped())
        out.writeUTF(value.asInstanceOf[ConfigNumber].transformToString())

      case LONG =>
        out.writeLong(value.asInstanceOf[ConfigLong].unwrapped())
        out.writeUTF(value.asInstanceOf[ConfigNumber].transformToString())

      case DOUBLE =>
        out.writeDouble(value.asInstanceOf[ConfigDouble].unwrapped())
        out.writeUTF(value.asInstanceOf[ConfigNumber].transformToString())

      case STRING => out.writeUTF(value.asInstanceOf[ConfigString].unwrapped())
      case LIST =>
        var list = value.asInstanceOf[ConfigList]
        out.writeInt(list.size)
        for (v <- list) {
          writeValue(out, v, list.origin().asInstanceOf[SimpleConfigOrigin])
        }

      case OBJECT =>
        var obj = value.asInstanceOf[ConfigObject]
        out.writeInt(obj.size)
        for ((key, value) <- obj) {
          out.writeUTF(key)
          writeValue(out, value, obj.origin().asInstanceOf[SimpleConfigOrigin])
        }

    }
  }

  private def readValueData(in: DataInput, origin: SimpleConfigOrigin): AbstractConfigValue = {
    val stb = in.readUnsignedByte()
    val st = SerializedValueType.forInt(stb)
    if (st == null) throw new IOException("Unknown serialized value type: " + stb)
    st match {
      case BOOLEAN => return new ConfigBoolean(origin, in.readBoolean())
      case NULL => return new ConfigNull(origin)
      case INT =>
        var vi = in.readInt()
        var si = in.readUTF()
        return new ConfigInt(origin, vi, si)

      case LONG =>
        var vl = in.readLong()
        var sl = in.readUTF()
        return new ConfigLong(origin, vl, sl)

      case DOUBLE =>
        var vd = in.readDouble()
        var sd = in.readUTF()
        return new ConfigDouble(origin, vd, sd)

      case STRING => return new ConfigString.Quoted(origin, in.readUTF())
      case LIST =>
        var listSize = in.readInt()
        var list = new ArrayList[AbstractConfigValue](listSize)
        for (i <- 0 until listSize) {
          val v = readValue(in, origin)
          list.add(v)
        }
        return new SimpleConfigList(origin, list)

      case OBJECT =>
        var mapSize = in.readInt()
        var map = new HashMap[String, AbstractConfigValue](mapSize)
        for (i <- 0 until mapSize) {
          val key = in.readUTF()
          val v = readValue(in, origin)
          map.put(key, v)
        }
        return new SimpleConfigObject(origin, map)

    }
    throw new IOException("Unhandled serialized value type: " + st)
  }

  private def writeValue(out: DataOutput, value: ConfigValue, baseOrigin: SimpleConfigOrigin) {
    val origin = new FieldOut(SerializedField.VALUE_ORIGIN)
    writeOrigin(origin.data, value.origin().asInstanceOf[SimpleConfigOrigin], baseOrigin)
    writeField(out, origin)
    val data = new FieldOut(SerializedField.VALUE_DATA)
    writeValueData(data.data, value)
    writeField(out, data)
    writeEndMarker(out)
  }

  private def readValue(in: DataInput, baseOrigin: SimpleConfigOrigin): AbstractConfigValue = {
    var value: AbstractConfigValue = null
    var origin: SimpleConfigOrigin = null
    while (true) {
      val code = readCode(in)
      if (code == SerializedField.END_MARKER) {
        if (value == null) throw new IOException("No value data found in serialization of value")
        value
      } else if (code == SerializedField.VALUE_DATA) {
        if (origin == null) throw new IOException("Origin must be stored before value data")
        in.readInt()
        value = readValueData(in, origin)
      } else if (code == SerializedField.VALUE_ORIGIN) {
        in.readInt()
        origin = readOrigin(in, baseOrigin)
      } else {
        skipField(in)
      }
    }
  }

  private def writeField(out: DataOutput, field: FieldOut) {
    val bytes = field.bytes.toByteArray()
    out.writeByte(field.code.ordinal())
    out.writeInt(bytes.length)
    out.write(bytes)
  }

  private def writeEndMarker(out: DataOutput) {
    out.writeByte(SerializedField.END_MARKER.ordinal())
  }

  private def readCode(in: DataInput): SerializedField = {
    val c = in.readUnsignedByte()
    if (c == SerializedField.UNKNOWN.ordinal()) throw new IOException("field code " + c + " is not supposed to be on the wire")
    SerializedField.forInt(c)
  }

  private def skipField(in: DataInput) {
    val len = in.readInt()
    val skipped = in.skipBytes(len)
    if (skipped < len) {
      val bytes = Array.ofDim[Byte]((len - skipped))
      in.readFully(bytes)
    }
  }

  private def shouldNotBeUsed(): ConfigException = {
    new ConfigException.BugOrBroken(classOf[SerializedConfigValue].getName + " should not exist outside of serialization")
  }
}

/**
 * Deliberately shoving all the serialization code into this class instead of
 * doing it OO-style with each subclass. Seems better to have it all in one
 * place. This class implements a lame serialization format that supports
 * skipping unknown fields, so it's moderately more extensible than the default
 * Java serialization format.
 */
@SerialVersionUID(1L)
class SerializedConfigValue extends AbstractConfigValue with Externalizable {





  private var value: ConfigValue = _

  private var wasConfig: Boolean = _

  def this(value: ConfigValue) {
    this()
    this.value = value
    this.wasConfig = false
  }

  def this(conf: Config) {
    this(conf.root())
    this.wasConfig = true
  }

  private def readResolve(): AnyRef = {
    if (wasConfig) value.asInstanceOf[ConfigObject].toConfig() else value
  }

  override def writeExternal(out: ObjectOutput) {
    if (value.asInstanceOf[AbstractConfigValue].resolveStatus() !=
      ResolveStatus.RESOLVED) throw new NotSerializableException("tried to serialize a value with unresolved substitutions, need to Config#resolve() first, see API docs")
    var field = new FieldOut(SerializedField.ROOT_VALUE)
    writeValue(field.data, value, null)
    writeField(out, field)
    field = new FieldOut(SerializedField.ROOT_WAS_CONFIG)
    field.data.writeBoolean(wasConfig)
    writeField(out, field)
    writeEndMarker(out)
  }

  override def readExternal(in: ObjectInput) {
    while (true) {
      val code = readCode(in)
      if (code == SerializedField.END_MARKER) {
        return
      } else if (code == SerializedField.ROOT_VALUE) {
        in.readInt()
        this.value = readValue(in, null)
      } else if (code == SerializedField.ROOT_WAS_CONFIG) {
        in.readInt()
        this.wasConfig = in.readBoolean()
      } else {
        skipField(in)
      }
    }
  }

  override def valueType(): ConfigValueType = throw shouldNotBeUsed()

  override def unwrapped(): AnyRef = throw shouldNotBeUsed()

  protected override def newCopy(origin: ConfigOrigin): SerializedConfigValue = throw shouldNotBeUsed()

  override def toString(): String = {
    getClass.getSimpleName + "(value=" + value + ",wasConfig=" +
      wasConfig +
      ")"
  }

  override def equals(other: Any): Boolean = other match {
    case other: SerializedConfigValue => canEqual(other) && (this.wasConfig == other.wasConfig) &&
      (this.value == other.value)
    case _ => false
  }

  override def hashCode(): Int = {
    var h = 41 * (41 + value.hashCode)
    h = 41 * (h + (if (wasConfig) 1 else 0))
    h
  }
}
