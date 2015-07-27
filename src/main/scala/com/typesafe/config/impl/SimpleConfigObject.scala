package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import java.util.AbstractMap
import java.util.ArrayList
import java.util.Arrays
import java.util.Collection
import java.util.Collections
import java.util.HashMap
import java.util.HashSet
import java.util.List
import java.util.Map
import java.util.Set
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValue
import RenderComparator._
import SimpleConfigObject._
//remove if not needed
import scala.collection.JavaConversions._

object SimpleConfigObject {

  private class ResolveModifier(var context: ResolveContext, val source: ResolveSource)
      extends Modifier {

    val originalRestrict = context.restrictToChild()

    override def modifyChildMayThrow(key: String, v: AbstractConfigValue): AbstractConfigValue = {
      if (context.isRestrictedToChild) {
        if (key == context.restrictToChild().first()) {
          val remainder = context.restrictToChild().remainder()
          if (remainder != null) {
            val result = context.restrict(remainder).resolve(v, source)
            context = result.context.unrestricted().restrict(originalRestrict)
            result.value
          } else {
            v
          }
        } else {
          v
        }
      } else {
        val result = context.unrestricted().resolve(v, source)
        context = result.context.unrestricted().restrict(originalRestrict)
        result.value
      }
    }
  }

  object RenderComparator {

    private def isAllDigits(s: String): Boolean = s.forall(c => c.isDigit)

  }

  @SerialVersionUID(1L)
  private class RenderComparator extends java.util.Comparator[String] with Serializable {

    override def compare(a: String, b: String): Int = {
      val aDigits = isAllDigits(a)
      val bDigits = isAllDigits(b)
      if (aDigits && bDigits) {
        Integer.compare(Integer.parseInt(a), Integer.parseInt(b))
      } else if (aDigits) {
        -1
      } else if (bDigits) {
        1
      } else {
        a.compareTo(b)
      }
    }
  }

  private def mapEquals(a: Map[String, ConfigValue], b: Map[String, ConfigValue]): Boolean = {
    if (a == b) return true
    val aKeys = a.keySet
    val bKeys = b.keySet
    if (aKeys != bKeys) return false
    for (key <- aKeys if a.get(key) != b.get(key)) return false
    true
  }

  private def mapHash(m: Map[String, ConfigValue]): Int = {
    val keys = new ArrayList[String]()
    keys.addAll(m.keySet)
    Collections.sort(keys)
    var valuesHash = 0
    for (k <- keys) {
      valuesHash += m.get(k).hashCode
    }
    41 * (41 + keys.hashCode) + valuesHash
  }

  private val EMPTY_NAME = "empty config"

  private val emptyInstance = empty(SimpleConfigOrigin.newSimple(EMPTY_NAME))

  def empty(): SimpleConfigObject = emptyInstance

  def empty(origin: ConfigOrigin): SimpleConfigObject = {
    if (origin == null) empty() else new SimpleConfigObject(origin, Collections.emptyMap[String, AbstractConfigValue]())
  }

  def emptyMissing(baseOrigin: ConfigOrigin): SimpleConfigObject = {
    new SimpleConfigObject(SimpleConfigOrigin.newSimple(baseOrigin.description() + " (not found)"), Collections.emptyMap[String, AbstractConfigValue]())
  }
}

@SerialVersionUID(2L)
class SimpleConfigObject(origin: ConfigOrigin,
    val value: Map[String, AbstractConfigValue],
    status: ResolveStatus,
    protected var ignoresFallbacks: Boolean) extends AbstractConfigObject(origin) with Serializable {

  private val resolved = status == ResolveStatus.RESOLVED

  if (value == null) throw new ConfigException.BugOrBroken("creating config object with null map")

  if (status != ResolveStatus.fromValues(value.values)) throw new ConfigException.BugOrBroken("Wrong resolved status on " + this)

  def this(origin: ConfigOrigin, value: Map[String, AbstractConfigValue]) {
    this(origin, value, ResolveStatus.fromValues(value.values), false)
  }

  override def withOnlyKey(key: String): SimpleConfigObject = withOnlyPath(Path.newKey(key))

  override def withoutKey(key: String): SimpleConfigObject = withoutPath(Path.newKey(key))

  protected override def withOnlyPathOrNull(path: Path): SimpleConfigObject = {
    val key = path.first()
    val next = path.remainder()
    var v = value.get(key)
    if (next != null) {
      v = if (v != null && (v.isInstanceOf[AbstractConfigObject])) v.asInstanceOf[AbstractConfigObject].withOnlyPathOrNull(next) else null
    }
    if (v == null) {
      null
    } else {
      new SimpleConfigObject(origin(), Collections.singletonMap(key, v), v.resolveStatus(), ignoresFallbacks)
    }
  }

  override def withOnlyPath(path: Path): SimpleConfigObject = {
    val o = withOnlyPathOrNull(path)
    if (o == null) {
      new SimpleConfigObject(origin(), Collections.emptyMap[String, AbstractConfigValue](), ResolveStatus.RESOLVED,
        ignoresFallbacks)
    } else {
      o
    }
  }

  override def withoutPath(path: Path): SimpleConfigObject = {
    val key = path.first()
    val next = path.remainder()
    var v = value.get(key)
    if (v != null && next != null && v.isInstanceOf[AbstractConfigObject]) {
      v = v.asInstanceOf[AbstractConfigObject].withoutPath(next)
      val updated = new HashMap[String, AbstractConfigValue](value)
      updated.put(key, v)
      new SimpleConfigObject(origin(), updated, ResolveStatus.fromValues(updated.values), ignoresFallbacks)
    } else if (next != null || v == null) {
      this
    } else {
      val smaller = new HashMap[String, AbstractConfigValue](value.size - 1)
      for ((key, value) <- value if key != key) smaller.put(key, value)
      new SimpleConfigObject(origin(), smaller, ResolveStatus.fromValues(smaller.values), ignoresFallbacks)
    }
  }

  override def withValue(key: String, v: ConfigValue): SimpleConfigObject = {
    if (v == null) throw new ConfigException.BugOrBroken("Trying to store null ConfigValue in a ConfigObject")
    var newMap: Map[String, AbstractConfigValue] = null
    if (value.isEmpty) {
      newMap = Collections.singletonMap(key, v.asInstanceOf[AbstractConfigValue])
    } else {
      newMap = new HashMap[String, AbstractConfigValue](value)
      newMap.put(key, v.asInstanceOf[AbstractConfigValue])
    }
    new SimpleConfigObject(origin(), newMap, ResolveStatus.fromValues(newMap.values), ignoresFallbacks)
  }

  override def withValue(path: Path, v: ConfigValue): SimpleConfigObject = {
    val key = path.first()
    val next = path.remainder()
    if (next == null) {
      withValue(key, v)
    } else {
      val child = value.get(key)
      if (child != null && child.isInstanceOf[AbstractConfigObject]) {
        withValue(key, child.asInstanceOf[AbstractConfigObject].withValue(next, v))
      } else {
        val subtree = v.asInstanceOf[AbstractConfigValue].atPath(SimpleConfigOrigin.newSimple("withValue(" + next.render() + ")"),
          next)
        withValue(key, subtree.root())
      }
    }
  }

  protected override def attemptPeekWithPartialResolve(key: String): AbstractConfigValue = value.get(key)

  private def newCopy(newStatus: ResolveStatus, newOrigin: ConfigOrigin, newIgnoresFallbacks: Boolean): SimpleConfigObject = {
    new SimpleConfigObject(newOrigin, value, newStatus, newIgnoresFallbacks)
  }

  protected override def newCopy(newStatus: ResolveStatus, newOrigin: ConfigOrigin): SimpleConfigObject = {
    newCopy(newStatus, newOrigin, ignoresFallbacks)
  }

  protected override def withFallbacksIgnored(): SimpleConfigObject = {
    if (ignoresFallbacks) this else newCopy(resolveStatus(), origin(), true)
  }

  override def resolveStatus(): ResolveStatus = ResolveStatus.fromBoolean(resolved)

  override def replaceChild(child: AbstractConfigValue, replacement: AbstractConfigValue): SimpleConfigObject = {
    val newChildren = new HashMap[String, AbstractConfigValue](value)
    for ((key, value) <- newChildren if value == child) {
      if (replacement != null) old.setValue(replacement) else newChildren.remove(key)
      return new SimpleConfigObject(origin(), newChildren, ResolveStatus.fromValues(newChildren.values),
        ignoresFallbacks)
    }
    throw new ConfigException.BugOrBroken("SimpleConfigObject.replaceChild did not find " + child +
      " in " +
      this)
  }

  override def hasDescendant(descendant: AbstractConfigValue): Boolean = {
    for (child <- value.values if child == descendant) return true
    for (child <- value.values if child.isInstanceOf[Container] &&
      child.asInstanceOf[Container].hasDescendant(descendant)) return true
    false
  }

  override def unwrapped(): Map[String, Any] = {
    val m = new HashMap[String, Any]()
    for ((key, value) <- value) {
      m.put(key, value.unwrapped())
    }
    m
  }

  protected override def mergedWithObject(abstractFallback: AbstractConfigObject): SimpleConfigObject = {
    requireNotIgnoringFallbacks()
    if (!(abstractFallback.isInstanceOf[SimpleConfigObject])) {
      throw new ConfigException.BugOrBroken("should not be reached (merging non-SimpleConfigObject)")
    }
    val fallback = abstractFallback.asInstanceOf[SimpleConfigObject]
    var changed = false
    var allResolved = true
    val merged = new HashMap[String, AbstractConfigValue]()
    val allKeys = new HashSet[String]()
    allKeys.addAll(this.keySet)
    allKeys.addAll(fallback.keySet)
    for (key <- allKeys) {
      val first = this.value.get(key)
      val second = fallback.value.get(key)
      var kept: AbstractConfigValue = null
      kept = if (first == null) second else if (second == null) first else first.withFallback(second)
      merged.put(key, kept)
      if (first != kept) changed = true
      if (kept.resolveStatus() == ResolveStatus.UNRESOLVED) allResolved = false
    }
    val newResolveStatus = ResolveStatus.fromBoolean(allResolved)
    val newIgnoresFallbacks = fallback.ignoresFallbacks()
    if (changed) new SimpleConfigObject(mergeOrigins(this, fallback), merged, newResolveStatus, newIgnoresFallbacks) else if (newResolveStatus != resolveStatus() || newIgnoresFallbacks != ignoresFallbacks()) newCopy(newResolveStatus,
      origin(), newIgnoresFallbacks) else this
  }

  private def modify(modifier: NoExceptionsModifier): SimpleConfigObject = modifyMayThrow(modifier)

  private def modifyMayThrow(modifier: Modifier): SimpleConfigObject = {
    var changes: Map[String, AbstractConfigValue] = null
    for (k <- keySet) {
      val v = value.get(k)
      val modified = modifier.modifyChildMayThrow(k, v)
      if (modified != v) {
        if (changes == null) changes = new HashMap[String, AbstractConfigValue]()
        changes.put(k, modified)
      }
    }
    if (changes == null) {
      this
    } else {
      val modified = new HashMap[String, AbstractConfigValue]()
      var sawUnresolved = false
      for (k <- keySet) {
        if (changes.containsKey(k)) {
          val newValue = changes.get(k)
          if (newValue != null) {
            modified.put(k, newValue)
            if (newValue.resolveStatus() == ResolveStatus.UNRESOLVED) sawUnresolved = true
          } else {
          }
        } else {
          val newValue = value.get(k)
          modified.put(k, newValue)
          if (newValue.resolveStatus() == ResolveStatus.UNRESOLVED) sawUnresolved = true
        }
      }
      new SimpleConfigObject(origin(), modified, if (sawUnresolved) ResolveStatus.UNRESOLVED else ResolveStatus.RESOLVED,
        ignoresFallbacks())
    }
  }

  override def resolveSubstitutions(context: ResolveContext, source: ResolveSource): ResolveResult[_ <: AbstractConfigObject] = {
    if (resolveStatus() == ResolveStatus.RESOLVED) return ResolveResult.make(context, this)
    val sourceWithParent = source.pushParent(this)
    val modifier = new ResolveModifier(context, sourceWithParent)
    val value = modifyMayThrow(modifier)
    ResolveResult.make(modifier.context, value).asObjectResult()
  }

  override def relativized(prefix: Path): SimpleConfigObject = {
    modify(new NoExceptionsModifier() {

      override def modifyChild(key: String, v: AbstractConfigValue): AbstractConfigValue = v.relativized(prefix)
    })
  }

  protected override def render(sb: StringBuilder,
      indent: Int,
      atRoot: Boolean,
      options: ConfigRenderOptions) {
    if (isEmpty) {
      sb.append("{}")
    } else {
      val outerBraces = options.getJson || !atRoot
      var innerIndent: Int = 0
      if (outerBraces) {
        innerIndent = indent + 1
        sb.append("{")
        if (options.getFormatted) sb.append('\n')
      } else {
        innerIndent = indent
      }
      var separatorCount = 0
      val keys = keySet.toArray(Array.ofDim[String](size))
      Arrays.sort(keys, new RenderComparator())
      for (k <- keys) {
        var v: AbstractConfigValue = null
        v = value.get(k)
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
            indent(sb, innerIndent, options)
            sb.append("#")
            if (!comment.startsWith(" ")) sb.append(' ')
            sb.append(comment)
            sb.append("\n")
          }
        }
        indent(sb, innerIndent, options)
        v.render(sb, innerIndent, false, k, options)
        if (options.getFormatted) {
          if (options.getJson) {
            sb.append(",")
            separatorCount = 2
          } else {
            separatorCount = 1
          }
          sb.append('\n')
        } else {
          sb.append(",")
          separatorCount = 1
        }
      }
      sb.setLength(sb.length - separatorCount)
      if (outerBraces) {
        if (options.getFormatted) {
          sb.append('\n')
          if (outerBraces) indent(sb, indent, options)
        }
        sb.append("}")
      }
    }
    if (atRoot && options.getFormatted) sb.append('\n')
  }

  override def get(key: AnyRef): AbstractConfigValue = value.get(key)

  protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[ConfigObject]

  override def equals(other: Any): Boolean = other match {
    case other: ConfigObject => canEqual(other) && mapEquals(this, other)
    case _ => false
  }

  override def hashCode(): Int = mapHash(this)

  override def containsKey(key: AnyRef): Boolean = value.containsKey(key)

  override def keySet(): Set[String] = value.keySet

  override def containsValue(v: AnyRef): Boolean = value.containsValue(v)

  override def entrySet(): Set[Map.Entry[String, ConfigValue]] = {
    val entries = new HashSet[Map.Entry[String, ConfigValue]]()
    for ((key, value) <- value) {
      entries.add(new AbstractMap.SimpleImmutableEntry[String, ConfigValue](key, value))
    }
    entries
  }

  override def isEmpty(): Boolean = value.isEmpty

  override def size(): Int = value.size

  override def values(): Collection[ConfigValue] = new HashSet[ConfigValue](value.values)

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)
}
