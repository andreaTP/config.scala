package com.typesafe.config.impl

import java.util.ArrayList
import java.util.Collection
import java.util.List
import java.util.Map
import java.util.Set
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigList
import com.typesafe.config.ConfigMergeable
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValue
import ConfigDelayedMergeObject._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigDelayedMergeObject {

  private def notResolved(): ConfigException = {
    new ConfigException.NotResolved("need to Config#resolve() before using this object, see the API docs for Config#resolve()")
  }
}

class ConfigDelayedMergeObject(origin: ConfigOrigin, val stack: List[AbstractConfigValue])
    extends AbstractConfigObject(origin) with Unmergeable with ReplaceableMergeStack {

  if (stack.isEmpty) throw new ConfigException.BugOrBroken("creating empty delayed merge object")

  if (!(stack.get(0).isInstanceOf[AbstractConfigObject])) throw new ConfigException.BugOrBroken("created a delayed merge object not guaranteed to be an object")

  for (v <- stack if v.isInstanceOf[ConfigDelayedMerge] || v.isInstanceOf[ConfigDelayedMergeObject]) throw new ConfigException.BugOrBroken("placed nested DelayedMerge in a ConfigDelayedMergeObject, should have consolidated stack")

  protected override def newCopy(status: ResolveStatus, origin: ConfigOrigin): ConfigDelayedMergeObject = {
    if (status != resolveStatus()) throw new ConfigException.BugOrBroken("attempt to create resolved ConfigDelayedMergeObject")
    new ConfigDelayedMergeObject(origin, stack)
  }

  override def resolveSubstitutions(context: ResolveContext, source: ResolveSource): ResolveResult[_ <: AbstractConfigObject] = {
    val merged = ConfigDelayedMerge.resolveSubstitutions(this, stack, context, source)
    merged.asObjectResult()
  }

  override def makeReplacement(context: ResolveContext, skipping: Int): AbstractConfigValue = {
    ConfigDelayedMerge.makeReplacement(context, stack, skipping)
  }

  override def resolveStatus(): ResolveStatus = ResolveStatus.UNRESOLVED

  override def replaceChild(child: AbstractConfigValue, replacement: AbstractConfigValue): AbstractConfigValue = {
    val newStack = replaceChildInList(stack, child, replacement)
    if (newStack == null) null else new ConfigDelayedMergeObject(origin(), newStack)
  }

  override def hasDescendant(descendant: AbstractConfigValue): Boolean = hasDescendantInList(stack, descendant)

  override def relativized(prefix: Path): ConfigDelayedMergeObject = {
    val newStack = new ArrayList[AbstractConfigValue]()
    for (o <- stack) {
      newStack.add(o.relativized(prefix))
    }
    new ConfigDelayedMergeObject(origin(), newStack)
  }

  protected override def ignoresFallbacks(): Boolean = {
    ConfigDelayedMerge.stackIgnoresFallbacks(stack)
  }

  protected override def mergedWithTheUnmergeable(fallback: Unmergeable): ConfigDelayedMergeObject = {
    requireNotIgnoringFallbacks()
    mergedWithTheUnmergeable(stack, fallback).asInstanceOf[ConfigDelayedMergeObject]
  }

  protected override def mergedWithObject(fallback: AbstractConfigObject): ConfigDelayedMergeObject = {
    mergedWithNonObject(fallback)
  }

  protected override def mergedWithNonObject(fallback: AbstractConfigValue): ConfigDelayedMergeObject = {
    requireNotIgnoringFallbacks()
    mergedWithNonObject(stack, fallback).asInstanceOf[ConfigDelayedMergeObject]
  }

  override def withFallback(mergeable: ConfigMergeable): ConfigDelayedMergeObject = {
    super.withFallback(mergeable).asInstanceOf[ConfigDelayedMergeObject]
  }

  override def withOnlyKey(key: String): ConfigDelayedMergeObject = throw notResolved()

  override def withoutKey(key: String): ConfigDelayedMergeObject = throw notResolved()

  protected override def withOnlyPathOrNull(path: Path): AbstractConfigObject = throw notResolved()

  override def withOnlyPath(path: Path): AbstractConfigObject = throw notResolved()

  override def withoutPath(path: Path): AbstractConfigObject = throw notResolved()

  override def withValue(key: String, value: ConfigValue): ConfigDelayedMergeObject = throw notResolved()

  override def withValue(path: Path, value: ConfigValue): ConfigDelayedMergeObject = throw notResolved()

  override def unmergedValues(): Collection[AbstractConfigValue] = stack

  protected override def canEqual(other: AnyRef): Boolean = {
    other.isInstanceOf[ConfigDelayedMergeObject]
  }

  override def equals(other: Any): Boolean = other match {
    case other: ConfigDelayedMergeObject => canEqual(other) &&
      (this.stack == other.stack || this.stack == other.stack)
    case _ => false
  }

  override def hashCode(): Int = stack.hashCode

  protected override def render(sb: StringBuilder,
      indent: Int,
      atRoot: Boolean,
      atKey: String,
      options: ConfigRenderOptions) {
    ConfigDelayedMerge.render(stack, sb, indent, atRoot, atKey, options)
  }

  protected override def render(sb: StringBuilder,
      indent: Int,
      atRoot: Boolean,
      options: ConfigRenderOptions) {
    render(sb, indent, atRoot, null, options)
  }

  override def unwrapped(): Map[String, Any] = throw notResolved()

  override def get(key: AnyRef): AbstractConfigValue = throw notResolved()

  override def containsKey(key: AnyRef): Boolean = throw notResolved()

  override def containsValue(value: AnyRef): Boolean = throw notResolved()

  override def entrySet(): Set[java.util.Map.Entry[String, ConfigValue]] = throw notResolved()

  override def isEmpty(): Boolean = throw notResolved()

  override def keySet(): Set[String] = throw notResolved()

  override def size(): Int = throw notResolved()

  override def values(): Collection[ConfigValue] = throw notResolved()

  protected override def attemptPeekWithPartialResolve(key: String): AbstractConfigValue = {
    for (layer <- stack) {
      if (layer.isInstanceOf[AbstractConfigObject]) {
        val objectLayer = layer.asInstanceOf[AbstractConfigObject]
        val v = objectLayer.attemptPeekWithPartialResolve(key)
        if (v != null) {
          if (v.ignoresFallbacks()) {
            return v
          } else {
            //continue
          }
        } else if (layer.isInstanceOf[Unmergeable]) {
          throw new ConfigException.BugOrBroken("should not be reached: unmergeable object returned null value")
        } else {
          //continue
        }
      } else if (layer.isInstanceOf[Unmergeable]) {
        throw new ConfigException.NotResolved("Key '" + key + "' is not available at '" + origin().description() +
          "' because value at '" +
          layer.origin().description() +
          "' has not been resolved and may turn out to contain or hide '" +
          key +
          "'." +
          " Be sure to Config#resolve() before using a config object.")
      } else if (layer.resolveStatus() == ResolveStatus.UNRESOLVED) {
        if (!(layer.isInstanceOf[ConfigList])) throw new ConfigException.BugOrBroken("Expecting a list here, not " + layer)
        return null
      } else {
        if (!layer.ignoresFallbacks()) {
          throw new ConfigException.BugOrBroken("resolved non-object should ignore fallbacks")
        }
        return null
      }
    }
    throw new ConfigException.BugOrBroken("Delayed merge stack does not contain any unmergeable values")
  }
}
