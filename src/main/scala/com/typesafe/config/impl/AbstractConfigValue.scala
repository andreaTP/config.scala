package com.typesafe.config.impl

import java.util.ArrayList
import java.util.Collection
import java.util.Collections
import java.util.List
import java.util.Map
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigMergeable
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValue
import AbstractConfigValue._
import com.typesafe.config.ConfigValueType._
//remove if not needed
import scala.collection.JavaConversions._

object AbstractConfigValue {

  /**
   * This exception means that a value is inherently not resolveable, at the
   * moment the only known cause is a cycle of substitutions. This is a
   * checked exception since it's internal to the library and we want to be
   * sure we handle it before passing it out to public API. This is only
   * supposed to be thrown by the target of a cyclic reference and it's
   * supposed to be caught by the ConfigReference looking up that reference,
   * so it should be impossible for an outermost resolve() to throw this.
   *
   * Contrast with ConfigException.NotResolved which just means nobody called
   * resolve().
   */
  @SerialVersionUID(1L)
  class NotPossibleToResolve(context: ResolveContext) extends Exception("was not possible to resolve") {

    var traceString: String = context.traceString()
  }

  protected def replaceChildInList(list: List[AbstractConfigValue], child: AbstractConfigValue, replacement: AbstractConfigValue): List[AbstractConfigValue] = {
    var i = 0
    while (i < list.size && list.get(i) != child) i
    if (i == list.size) throw new ConfigException.BugOrBroken("tried to replace " + child + " which is not in " + list)
    val newStack = new ArrayList[AbstractConfigValue](list)
    if (replacement != null) newStack.set(i, replacement) else newStack.remove(i)
    if (newStack.isEmpty) null else newStack
  }

  protected def hasDescendantInList(list: List[AbstractConfigValue], descendant: AbstractConfigValue): Boolean = {
    for (v <- list if v == descendant) return true
    for (v <- list if v.isInstanceOf[Container] && 
      v.asInstanceOf[Container].hasDescendant(descendant)) return true
    false
  }

  protected trait Modifier {

    def modifyChildMayThrow(keyOrNull: String, v: AbstractConfigValue): AbstractConfigValue
  }

  protected def indent(sb: StringBuilder, indent: Int, options: ConfigRenderOptions) {
    if (options.getFormatted) {
      val remaining = indent
      while (remaining > 0) {
        sb.append("    ")
        remaining
      }
    }
  }
}

/**
 *
 * Trying very hard to avoid a parent reference in config values; when you have
 * a tree like this, the availability of parent() tends to result in a lot of
 * improperly-factored and non-modular code. Please don't add parent().
 *
 */
abstract class AbstractConfigValue(origin: ConfigOrigin) extends ConfigValue with MergeableValue {

  var origin: SimpleConfigOrigin = origin.asInstanceOf[SimpleConfigOrigin]

  /**
   * Called only by ResolveContext.resolve().
   *
   * @param context
   *            state of the current resolve
   * @param source
   *            where to look up values
   * @return a new value if there were changes, or this if no changes
   */
  def resolveSubstitutions(context: ResolveContext, source: ResolveSource): ResolveResult[_ <: AbstractConfigValue] = {
    ResolveResult.make(context, this)
  }

  def resolveStatus(): ResolveStatus = ResolveStatus.RESOLVED

  /**
   * This is used when including one file in another; the included file is
   * relativized to the path it's included into in the parent file. The point
   * is that if you include a file at foo.bar in the parent, and the included
   * file as a substitution ${a.b.c}, the included substitution now needs to
   * be ${foo.bar.a.b.c} because we resolve substitutions globally only after
   * parsing everything.
   *
   * @param prefix
   * @return value relativized to the given path or the same value if nothing
   *         to do
   */
  def relativized(prefix: Path): AbstractConfigValue = this

  protected abstract class NoExceptionsModifier extends Modifier {

    override def modifyChildMayThrow(keyOrNull: String, v: AbstractConfigValue): AbstractConfigValue = {
      modifyChild(keyOrNull, v)
    }

    def modifyChild(keyOrNull: String, v: AbstractConfigValue): AbstractConfigValue
  }

  override def toFallbackValue(): AbstractConfigValue = this

  protected def newCopy(origin: ConfigOrigin): AbstractConfigValue

  protected def ignoresFallbacks(): Boolean = {
    resolveStatus() == ResolveStatus.RESOLVED
  }

  protected def withFallbacksIgnored(): AbstractConfigValue = {
    if (ignoresFallbacks()) this else throw new ConfigException.BugOrBroken("value class doesn't implement forced fallback-ignoring " + 
      this)
  }

  protected def requireNotIgnoringFallbacks() {
    if (ignoresFallbacks()) throw new ConfigException.BugOrBroken("method should not have been called with ignoresFallbacks=true " + 
      getClass.getSimpleName)
  }

  protected def constructDelayedMerge(origin: ConfigOrigin, stack: List[AbstractConfigValue]): AbstractConfigValue = {
    new ConfigDelayedMerge(origin, stack)
  }

  protected def mergedWithTheUnmergeable(stack: Collection[AbstractConfigValue], fallback: Unmergeable): AbstractConfigValue = {
    requireNotIgnoringFallbacks()
    val newStack = new ArrayList[AbstractConfigValue]()
    newStack.addAll(stack)
    newStack.addAll(fallback.unmergedValues())
    constructDelayedMerge(AbstractConfigObject.mergeOrigins(newStack), newStack)
  }

  private def delayMerge(stack: Collection[AbstractConfigValue], fallback: AbstractConfigValue): AbstractConfigValue = {
    val newStack = new ArrayList[AbstractConfigValue]()
    newStack.addAll(stack)
    newStack.add(fallback)
    constructDelayedMerge(AbstractConfigObject.mergeOrigins(newStack), newStack)
  }

  protected def mergedWithObject(stack: Collection[AbstractConfigValue], fallback: AbstractConfigObject): AbstractConfigValue = {
    requireNotIgnoringFallbacks()
    if (this.isInstanceOf[AbstractConfigObject]) throw new ConfigException.BugOrBroken("Objects must reimplement mergedWithObject")
    mergedWithNonObject(stack, fallback)
  }

  protected def mergedWithNonObject(stack: Collection[AbstractConfigValue], fallback: AbstractConfigValue): AbstractConfigValue = {
    requireNotIgnoringFallbacks()
    if (resolveStatus() == ResolveStatus.RESOLVED) {
      withFallbacksIgnored()
    } else {
      delayMerge(stack, fallback)
    }
  }

  protected def mergedWithTheUnmergeable(fallback: Unmergeable): AbstractConfigValue = {
    requireNotIgnoringFallbacks()
    mergedWithTheUnmergeable(Collections.singletonList(this), fallback)
  }

  protected def mergedWithObject(fallback: AbstractConfigObject): AbstractConfigValue = {
    requireNotIgnoringFallbacks()
    mergedWithObject(Collections.singletonList(this), fallback)
  }

  protected def mergedWithNonObject(fallback: AbstractConfigValue): AbstractConfigValue = {
    requireNotIgnoringFallbacks()
    mergedWithNonObject(Collections.singletonList(this), fallback)
  }

  override def withOrigin(origin: ConfigOrigin): AbstractConfigValue = {
    if (this.origin == origin) this else newCopy(origin)
  }

  override def withFallback(mergeable: ConfigMergeable): AbstractConfigValue = {
    if (ignoresFallbacks()) {
      this
    } else {
      val other = mergeable.asInstanceOf[MergeableValue].toFallbackValue()
      if (other.isInstanceOf[Unmergeable]) {
        mergedWithTheUnmergeable(other.asInstanceOf[Unmergeable])
      } else if (other.isInstanceOf[AbstractConfigObject]) {
        mergedWithObject(other.asInstanceOf[AbstractConfigObject])
      } else {
        mergedWithNonObject(other.asInstanceOf[AbstractConfigValue])
      }
    }
  }

  protected def canEqual(other: AnyRef): Boolean = other.isInstanceOf[ConfigValue]

  override def equals(other: Any): Boolean = other match {
    case other: ConfigValue => canEqual(other) && (this.valueType() == other.valueType()) && 
      ConfigImplUtil.equalsHandlingNull(this.unwrapped(), other.unwrapped())
    case _ => false
  }

  override def hashCode(): Int = {
    val o = this.unwrapped()
    if (o == null) 0 else o.hashCode
  }

  override def toString(): String = {
    val sb = new StringBuilder()
    render(sb, 0, true, null, ConfigRenderOptions.concise())
    getClass.getSimpleName + "(" + sb.toString + ")"
  }

  protected def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      atKey: String, 
      options: ConfigRenderOptions) {
    if (atKey != null) {
      var renderedKey: String = null
      renderedKey = if (options.getJson) ConfigImplUtil.renderJsonString(atKey) else ConfigImplUtil.renderStringUnquotedIfPossible(atKey)
      sb.append(renderedKey)
      if (options.getJson) {
        if (options.getFormatted) sb.append(" : ") else sb.append(":")
      } else {
        if (this.isInstanceOf[ConfigObject]) {
          if (options.getFormatted) sb.append(' ')
        } else {
          sb.append("=")
        }
      }
    }
    render(sb, indent, atRoot, options)
  }

  protected def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      options: ConfigRenderOptions) {
    val u = unwrapped()
    sb.append(u.toString)
  }

  override def render(): String = render(ConfigRenderOptions.defaults())

  override def render(options: ConfigRenderOptions): String = {
    val sb = new StringBuilder()
    render(sb, 0, true, null, options)
    sb.toString
  }

  def transformToString(): String = null

  def atKey(origin: ConfigOrigin, key: String): SimpleConfig = {
    val m = Collections.singletonMap(key, this)
    (new SimpleConfigObject(origin, m)).toConfig()
  }

  override def atKey(key: String): SimpleConfig = {
    atKey(SimpleConfigOrigin.newSimple("atKey(" + key + ")"), key)
  }

  def atPath(origin: ConfigOrigin, path: Path): SimpleConfig = {
    var parent = path.parent()
    var result = atKey(origin, path.last())
    while (parent != null) {
      val key = parent.last()
      result = result.atKey(origin, key)
      parent = parent.parent()
    }
    result
  }

  override def atPath(pathExpression: String): SimpleConfig = {
    val origin = SimpleConfigOrigin.newSimple("atPath(" + pathExpression + ")")
    atPath(origin, Path.newPath(pathExpression))
  }
}
