package com.typesafe.config.impl

import java.util.ArrayList
import java.util.Collection
import java.util.Collections
import java.util.List
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValueType
import ConfigConcatenation._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigConcatenation {

  private def isIgnoredWhitespace(value: AbstractConfigValue): Boolean = {
    (value.isInstanceOf[ConfigString]) && !value.asInstanceOf[ConfigString].wasQuoted()
  }

  /**
   * Add left and right, or their merger, to builder.
   */
  private def join(builder: ArrayList[AbstractConfigValue], origRight: AbstractConfigValue) {
    var left = builder.get(builder.size - 1)
    var right = origRight
    if (left.isInstanceOf[ConfigObject] && right.isInstanceOf[SimpleConfigList]) {
      left = DefaultTransformer.transform(left, ConfigValueType.LIST)
    } else if (left.isInstanceOf[SimpleConfigList] && right.isInstanceOf[ConfigObject]) {
      right = DefaultTransformer.transform(right, ConfigValueType.LIST)
    }
    var joined: AbstractConfigValue = null
    if (left.isInstanceOf[ConfigObject] && right.isInstanceOf[ConfigObject]) {
      joined = right.withFallback(left)
    } else if (left.isInstanceOf[SimpleConfigList] && right.isInstanceOf[SimpleConfigList]) {
      joined = left.asInstanceOf[SimpleConfigList].concatenate(right.asInstanceOf[SimpleConfigList])
    } else if ((left.isInstanceOf[SimpleConfigList] || left.isInstanceOf[ConfigObject]) && 
      isIgnoredWhitespace(right)) {
      joined = left
    } else if (left.isInstanceOf[ConfigConcatenation] || right.isInstanceOf[ConfigConcatenation]) {
      throw new ConfigException.BugOrBroken("unflattened ConfigConcatenation")
    } else if (left.isInstanceOf[Unmergeable] || right.isInstanceOf[Unmergeable]) {
    } else {
      val s1 = left.transformToString()
      val s2 = right.transformToString()
      if (s1 == null || s2 == null) {
        throw new ConfigException.WrongType(left.origin(), "Cannot concatenate object or list with a non-object-or-list, " + 
          left + 
          " and " + 
          right + 
          " are not compatible")
      } else {
        val joinedOrigin = SimpleConfigOrigin.mergeOrigins(left.origin(), right.origin())
        joined = new ConfigString.Quoted(joinedOrigin, s1 + s2)
      }
    }
    if (joined == null) {
      builder.add(right)
    } else {
      builder.remove(builder.size - 1)
      builder.add(joined)
    }
  }

  def consolidate(pieces: List[AbstractConfigValue]): List[AbstractConfigValue] = {
    if (pieces.size < 2) {
      pieces
    } else {
      val flattened = new ArrayList[AbstractConfigValue](pieces.size)
      for (v <- pieces) {
        if (v.isInstanceOf[ConfigConcatenation]) {
          flattened.addAll(v.asInstanceOf[ConfigConcatenation].pieces)
        } else {
          flattened.add(v)
        }
      }
      val consolidated = new ArrayList[AbstractConfigValue](flattened.size)
      for (v <- flattened) {
        if (consolidated.isEmpty) consolidated.add(v) else join(consolidated, v)
      }
      consolidated
    }
  }

  def concatenate(pieces: List[AbstractConfigValue]): AbstractConfigValue = {
    val consolidated = consolidate(pieces)
    if (consolidated.isEmpty) {
      null
    } else if (consolidated.size == 1) {
      consolidated.get(0)
    } else {
      val mergedOrigin = SimpleConfigOrigin.mergeOrigins(consolidated)
      new ConfigConcatenation(mergedOrigin, consolidated)
    }
  }
}

/**
 * A ConfigConcatenation represents a list of values to be concatenated (see the
 * spec). It only has to exist if at least one value is an unresolved
 * substitution, otherwise we could go ahead and collapse the list into a single
 * value.
 *
 * Right now this is always a list of strings and ${} references, but in the
 * future should support a list of ConfigList. We may also support
 * concatenations of objects, but ConfigDelayedMerge should be used for that
 * since a concat of objects really will merge, not concatenate.
 */
class ConfigConcatenation(origin: ConfigOrigin, val pieces: List[AbstractConfigValue])
    extends AbstractConfigValue(origin) with Unmergeable with Container {

  if (pieces.size < 2) throw new ConfigException.BugOrBroken("Created concatenation with less than 2 items: " + this)

  var hadUnmergeable = false

  for (p <- pieces) {
    if (p.isInstanceOf[ConfigConcatenation]) throw new ConfigException.BugOrBroken("ConfigConcatenation should never be nested: " + this)
    if (p.isInstanceOf[Unmergeable]) hadUnmergeable = true
  }

  if (!hadUnmergeable) throw new ConfigException.BugOrBroken("Created concatenation without an unmergeable in it: " + 
    this)

  private def notResolved(): ConfigException.NotResolved = {
    new ConfigException.NotResolved("need to Config#resolve(), see the API docs for Config#resolve(); substitution not resolved: " + 
      this)
  }

  override def valueType(): ConfigValueType = throw notResolved()

  override def unwrapped(): AnyRef = throw notResolved()

  protected override def newCopy(newOrigin: ConfigOrigin): ConfigConcatenation = {
    new ConfigConcatenation(newOrigin, pieces)
  }

  protected override def ignoresFallbacks(): Boolean = false

  override def unmergedValues(): Collection[ConfigConcatenation] = Collections.singleton(this)

  override def resolveSubstitutions(context: ResolveContext, source: ResolveSource): ResolveResult[_ <: AbstractConfigValue] = {
    if (ConfigImpl.traceSubstitutionsEnabled()) {
      val indent = context.depth() + 2
      ConfigImpl.trace(indent - 1, "concatenation has " + pieces.size + " pieces:")
      var count = 0
      for (v <- pieces) {
        ConfigImpl.trace(indent, count + ": " + v)
        count += 1
      }
    }
    val sourceWithParent = source
    var newContext = context
    val resolved = new ArrayList[AbstractConfigValue](pieces.size)
    for (p <- pieces) {
      val restriction = newContext.restrictToChild()
      val result = newContext.unrestricted().resolve(p, sourceWithParent)
      val r = result.value
      newContext = result.context.restrict(restriction)
      if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(context.depth(), "resolved concat piece to " + r)
      if (r == null) {
      } else {
        resolved.add(r)
      }
    }
    val joined = consolidate(resolved)
    if (joined.size > 1 && context.options().getAllowUnresolved) ResolveResult.make(newContext, new ConfigConcatenation(this.origin(), 
      joined)) else if (joined.isEmpty) ResolveResult.make(newContext, null) else if (joined.size == 1) ResolveResult.make(newContext, 
      joined.get(0)) else throw new ConfigException.BugOrBroken("Bug in the library; resolved list was joined to too many values: " + 
      joined)
  }

  override def resolveStatus(): ResolveStatus = ResolveStatus.UNRESOLVED

  override def replaceChild(child: AbstractConfigValue, replacement: AbstractConfigValue): ConfigConcatenation = {
    val newPieces = replaceChildInList(pieces, child, replacement)
    if (newPieces == null) null else new ConfigConcatenation(origin(), newPieces)
  }

  override def hasDescendant(descendant: AbstractConfigValue): Boolean = hasDescendantInList(pieces, descendant)

  override def relativized(prefix: Path): ConfigConcatenation = {
    val newPieces = new ArrayList[AbstractConfigValue]()
    for (p <- pieces) {
      newPieces.add(p.relativized(prefix))
    }
    new ConfigConcatenation(origin(), newPieces)
  }

  protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[ConfigConcatenation]

  override def equals(other: Any): Boolean = other match {
    case other: ConfigConcatenation => canEqual(other) && this.pieces == other.pieces
    case _ => false
  }

  override def hashCode(): Int = pieces.hashCode

  protected override def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      options: ConfigRenderOptions) {
    for (p <- pieces) {
      p.render(sb, indent, atRoot, options)
    }
  }
}
