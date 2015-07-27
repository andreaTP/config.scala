package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import java.util.ArrayList
import java.util.Collection
import java.util.Iterator
import java.util.List
import java.util.ListIterator
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigList
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValue
import com.typesafe.config.ConfigValueType
import SimpleConfigList._
//remove if not needed
import scala.collection.JavaConversions._

object SimpleConfigList {

  private class ResolveModifier(var context: ResolveContext, val source: ResolveSource)
      extends Modifier {

    override def modifyChildMayThrow(key: String, v: AbstractConfigValue): AbstractConfigValue = {
      val result = context.resolve(v, source)
      context = result.context
      result.value
    }
  }

  private def wrapListIterator(i: ListIterator[AbstractConfigValue]): ListIterator[ConfigValue] = {
    new ListIterator[ConfigValue]() {

      override def hasNext(): Boolean = i.hasNext

      override def next(): ConfigValue = i.next()

      override def remove() {
        throw weAreImmutable("listIterator().remove")
      }

      override def add(arg0: ConfigValue) {
        throw weAreImmutable("listIterator().add")
      }

      override def hasPrevious(): Boolean = i.hasPrevious()

      override def nextIndex(): Int = i.nextIndex()

      override def previous(): ConfigValue = i.previous()

      override def previousIndex(): Int = i.previousIndex()

      override def set(arg0: ConfigValue) {
        throw weAreImmutable("listIterator().set")
      }
    }
  }

  private def weAreImmutable(method: String): UnsupportedOperationException = {
    new UnsupportedOperationException("ConfigList is immutable, you can't call List.'" + method + 
      "'")
  }
}

@SerialVersionUID(2L)
class SimpleConfigList(origin: ConfigOrigin, val value: List[AbstractConfigValue], status: ResolveStatus)
    extends AbstractConfigValue(origin) with ConfigList with Container with Serializable {

  private val resolved = status == ResolveStatus.RESOLVED

  if (status != ResolveStatus.fromValues(value)) throw new ConfigException.BugOrBroken("SimpleConfigList created with wrong resolve status: " + 
    this)

  def this(origin: ConfigOrigin, value: List[AbstractConfigValue]) {
    this(origin, value, ResolveStatus.fromValues(value))
  }

  override def valueType(): ConfigValueType = ConfigValueType.LIST

  override def unwrapped(): List[Any] = {
    val list = new ArrayList[Any]()
    for (v <- value) {
      list.add(v.unwrapped())
    }
    list
  }

  override def resolveStatus(): ResolveStatus = ResolveStatus.fromBoolean(resolved)

  override def replaceChild(child: AbstractConfigValue, replacement: AbstractConfigValue): SimpleConfigList = {
    val newList = replaceChildInList(value, child, replacement)
    if (newList == null) {
      null
    } else {
      new SimpleConfigList(origin(), newList)
    }
  }

  override def hasDescendant(descendant: AbstractConfigValue): Boolean = hasDescendantInList(value, descendant)

  private def modify(modifier: NoExceptionsModifier, newResolveStatus: ResolveStatus): SimpleConfigList = {
    modifyMayThrow(modifier, newResolveStatus)
  }

  private def modifyMayThrow(modifier: Modifier, newResolveStatus: ResolveStatus): SimpleConfigList = {
    var changed: List[AbstractConfigValue] = null
    var i = 0
    for (v <- value) {
      val modified = modifier.modifyChildMayThrow(null, v)
      if (changed == null && modified != v) {
        changed = new ArrayList[AbstractConfigValue]()
        for (j <- 0 until i) {
          changed.add(value.get(j))
        }
      }
      if (changed != null && modified != null) {
        changed.add(modified)
      }
      i += 1
    }
    if (changed != null) {
      if (newResolveStatus != null) {
        new SimpleConfigList(origin(), changed, newResolveStatus)
      } else {
        new SimpleConfigList(origin(), changed)
      }
    } else {
      this
    }
  }

  override def resolveSubstitutions(context: ResolveContext, source: ResolveSource): ResolveResult[_ <: SimpleConfigList] = {
    if (resolved) return ResolveResult.make(context, this)
    if (context.isRestrictedToChild) {
      ResolveResult.make(context, this)
    } else {
      val modifier = new ResolveModifier(context, source.pushParent(this))
      val value = modifyMayThrow(modifier, if (context.options().getAllowUnresolved) null else ResolveStatus.RESOLVED)
      ResolveResult.make(modifier.context, value)
    }
  }

  override def relativized(prefix: Path): SimpleConfigList = {
    modify(new NoExceptionsModifier() {

      override def modifyChild(key: String, v: AbstractConfigValue): AbstractConfigValue = v.relativized(prefix)
    }, resolveStatus())
  }

  protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[SimpleConfigList]

  override def equals(other: Any): Boolean = other match {
    case other: SimpleConfigList => canEqual(other) && (value == other.value || value == other.value)
    case _ => false
  }

  override def hashCode(): Int = value.hashCode

  protected override def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      options: ConfigRenderOptions) {
    if (value.isEmpty) {
      sb.append("[]")
    } else {
      sb.append("[")
      if (options.getFormatted) sb.append('\n')
      for (v <- value) {
        if (options.getOriginComments) {
          val lines = v.origin().description().split("\n")
          for (l <- lines) {
            indent(sb, indent + 1, options)
            sb.append('#')
            if (!l.isEmpty) sb.append(' ')
            sb.append(l)
            sb.append("\n")
          }
        }
        if (options.getComments) {
          for (comment <- v.origin().comments()) {
            indent(sb, indent + 1, options)
            sb.append("# ")
            sb.append(comment)
            sb.append("\n")
          }
        }
        indent(sb, indent + 1, options)
        v.render(sb, indent + 1, atRoot, options)
        sb.append(",")
        if (options.getFormatted) sb.append('\n')
      }
      sb.setLength(sb.length - 1)
      if (options.getFormatted) {
        sb.setLength(sb.length - 1)
        sb.append('\n')
        indent(sb, indent, options)
      }
      sb.append("]")
    }
  }

  override def contains(o: AnyRef): Boolean = value.contains(o)

  override def containsAll(c: Collection[_]): Boolean = value.containsAll(c)

  override def get(index: Int): AbstractConfigValue = value.get(index)

  override def indexOf(o: AnyRef): Int = value.indexOf(o)

  override def isEmpty(): Boolean = value.isEmpty

  override def iterator(): Iterator[ConfigValue] = {
    val i = value.iterator()
    new Iterator[ConfigValue]() {

      override def hasNext(): Boolean = i.hasNext

      override def next(): ConfigValue = i.next()

      override def remove() {
        throw weAreImmutable("iterator().remove")
      }
    }
  }

  override def lastIndexOf(o: AnyRef): Int = value.lastIndexOf(o)

  override def listIterator(): ListIterator[ConfigValue] = wrapListIterator(value.listIterator())

  override def listIterator(index: Int): ListIterator[ConfigValue] = {
    wrapListIterator(value.listIterator(index))
  }

  override def size(): Int = value.size

  override def subList(fromIndex: Int, toIndex: Int): List[ConfigValue] = {
    val list = new ArrayList[ConfigValue]()
    for (v <- value.subList(fromIndex, toIndex)) {
      list.add(v)
    }
    list
  }

  override def toArray(): Array[Any] = value.toArray()

  override def toArray[T](a: Array[T]): Array[T] = value.toArray(a)

  override def add(e: ConfigValue): Boolean = throw weAreImmutable("add")

  override def add(index: Int, element: ConfigValue) {
    throw weAreImmutable("add")
  }

  override def addAll(c: Collection[_ <: ConfigValue]): Boolean = throw weAreImmutable("addAll")

  override def addAll(index: Int, c: Collection[_ <: ConfigValue]): Boolean = throw weAreImmutable("addAll")

  override def clear() {
    throw weAreImmutable("clear")
  }

  override def remove(o: AnyRef): Boolean = throw weAreImmutable("remove")

  override def remove(index: Int): ConfigValue = throw weAreImmutable("remove")

  override def removeAll(c: Collection[_]): Boolean = throw weAreImmutable("removeAll")

  override def retainAll(c: Collection[_]): Boolean = throw weAreImmutable("retainAll")

  override def set(index: Int, element: ConfigValue): ConfigValue = throw weAreImmutable("set")

  protected override def newCopy(newOrigin: ConfigOrigin): SimpleConfigList = new SimpleConfigList(newOrigin, value)

  def concatenate(other: SimpleConfigList): SimpleConfigList = {
    val combinedOrigin = SimpleConfigOrigin.mergeOrigins(origin(), other.origin())
    val combined = new ArrayList[AbstractConfigValue](value.size + other.value.size)
    combined.addAll(value)
    combined.addAll(other.value)
    new SimpleConfigList(combinedOrigin, combined)
  }

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)

  override def withOrigin(origin: ConfigOrigin): SimpleConfigList = {
    super.withOrigin(origin).asInstanceOf[SimpleConfigList]
  }
}
