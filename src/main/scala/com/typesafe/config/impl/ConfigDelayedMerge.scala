package com.typesafe.config.impl

import java.util.ArrayList
import java.util.Collection
import java.util.Collections
import java.util.List
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValueType
import ConfigDelayedMerge._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigDelayedMerge {

  def resolveSubstitutions(replaceable: ReplaceableMergeStack, 
      stack: List[AbstractConfigValue], 
      context: ResolveContext, 
      source: ResolveSource): ResolveResult[_ <: AbstractConfigValue] = {
    if (ConfigImpl.traceSubstitutionsEnabled()) {
      ConfigImpl.trace(context.depth(), "delayed merge stack has " + stack.size + " items:")
      var count = 0
      for (v <- stack) {
        ConfigImpl.trace(context.depth() + 1, count + ": " + v)
        count += 1
      }
    }
    var newContext = context
    var count = 0
    var merged: AbstractConfigValue = null
    for (end <- stack) {
      var sourceForEnd: ResolveSource = null
      if (end.isInstanceOf[ReplaceableMergeStack]) throw new ConfigException.BugOrBroken("A delayed merge should not contain another one: " + replaceable) else if (end.isInstanceOf[Unmergeable]) {
        val remainder = replaceable.makeReplacement(context, count + 1)
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "remainder portion: " + remainder)
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "building sourceForEnd")
        sourceForEnd = source.replaceWithinCurrentParent(replaceable.asInstanceOf[AbstractConfigValue], 
          remainder)
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "  sourceForEnd before reset parents but after replace: " + 
          sourceForEnd)
        sourceForEnd = sourceForEnd.resetParents()
      } else {
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "will resolve end against the original source with parent pushed")
        sourceForEnd = source.pushParent(replaceable)
      }
      if (ConfigImpl.traceSubstitutionsEnabled()) {
        ConfigImpl.trace(newContext.depth(), "sourceForEnd      =" + sourceForEnd)
      }
      if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "Resolving highest-priority item in delayed merge " + 
        end + 
        " against " + 
        sourceForEnd + 
        " endWasRemoved=" + 
        (source != sourceForEnd))
      val result = newContext.resolve(end, sourceForEnd)
      val resolvedEnd = result.value
      newContext = result.context
      if (resolvedEnd != null) {
        if (merged == null) {
          merged = resolvedEnd
        } else {
          if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth() + 1, "merging " + merged + " with fallback " + resolvedEnd)
          merged = merged.withFallback(resolvedEnd)
        }
      }
      count += 1
      if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "stack merged, yielding: " + merged)
    }
    ResolveResult.make(newContext, merged)
  }

  def makeReplacement(context: ResolveContext, stack: List[AbstractConfigValue], skipping: Int): AbstractConfigValue = {
    val subStack = stack.subList(skipping, stack.size)
    if (subStack.isEmpty) {
      if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(context.depth(), "Nothing else in the merge stack, replacing with null")
      null
    } else {
      var merged: AbstractConfigValue = null
      for (v <- subStack) {
        merged = if (merged == null) v else merged.withFallback(v)
      }
      merged
    }
  }

  def stackIgnoresFallbacks(stack: List[AbstractConfigValue]): Boolean = {
    val last = stack.get(stack.size - 1)
    last.ignoresFallbacks()
  }

  def render(stack: List[AbstractConfigValue], 
      sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      atKey: String, 
      options: ConfigRenderOptions) {
    val commentMerge = options.getComments
    if (commentMerge) {
      sb.append("# unresolved merge of " + stack.size + " values follows (\n")
      if (atKey == null) {
        indent(sb, indent, options)
        sb.append("# this unresolved merge will not be parseable because it's at the root of the object\n")
        indent(sb, indent, options)
        sb.append("# the HOCON format has no way to list multiple root objects in a single file\n")
      }
    }
    val reversed = new ArrayList[AbstractConfigValue]()
    reversed.addAll(stack)
    Collections.reverse(reversed)
    var i = 0
    for (v <- reversed) {
      if (commentMerge) {
        indent(sb, indent, options)
        if (atKey != null) {
          sb.append("#     unmerged value " + i + " for key " + ConfigImplUtil.renderJsonString(atKey) + 
            " from ")
        } else {
          sb.append("#     unmerged value " + i + " from ")
        }
        i += 1
        sb.append(v.origin().description())
        sb.append("\n")
        for (comment <- v.origin().comments()) {
          indent(sb, indent, options)
          sb.append("# ")
          sb.append(comment)
          sb.append("\n")
        }
      }
      indent(sb, indent, options)
      if (atKey != null) {
        sb.append(ConfigImplUtil.renderJsonString(atKey))
        if (options.getFormatted) sb.append(" : ") else sb.append(":")
      }
      v.render(sb, indent, atRoot, options)
      sb.append(",")
      if (options.getFormatted) sb.append('\n')
    }
    sb.setLength(sb.length - 1)
    if (options.getFormatted) {
      sb.setLength(sb.length - 1)
      sb.append("\n")
    }
    if (commentMerge) {
      indent(sb, indent, options)
      sb.append("# ) end of unresolved merge\n")
    }
  }
}

/**
 * The issue here is that we want to first merge our stack of config files, and
 * then we want to evaluate substitutions. But if two substitutions both expand
 * to an object, we might need to merge those two objects. Thus, we can't ever
 * "override" a substitution when we do a merge; instead we have to save the
 * stack of values that should be merged, and resolve the merge when we evaluate
 * substitutions.
 */
class ConfigDelayedMerge(origin: ConfigOrigin, val stack: List[AbstractConfigValue])
    extends AbstractConfigValue(origin) with Unmergeable with ReplaceableMergeStack {

  if (stack.isEmpty) throw new ConfigException.BugOrBroken("creating empty delayed merge value")

  for (v <- stack if v.isInstanceOf[ConfigDelayedMerge] || v.isInstanceOf[ConfigDelayedMergeObject]) throw new ConfigException.BugOrBroken("placed nested DelayedMerge in a ConfigDelayedMerge, should have consolidated stack")

  override def valueType(): ConfigValueType = {
    throw new ConfigException.NotResolved("called valueType() on value with unresolved substitutions, need to Config#resolve() first, see API docs")
  }

  override def unwrapped(): AnyRef = {
    throw new ConfigException.NotResolved("called unwrapped() on value with unresolved substitutions, need to Config#resolve() first, see API docs")
  }

  override def resolveSubstitutions(context: ResolveContext, source: ResolveSource): ResolveResult[_ <: AbstractConfigValue] = {
    resolveSubstitutions(this, stack, context, source)
  }

  override def makeReplacement(context: ResolveContext, skipping: Int): AbstractConfigValue = {
    ConfigDelayedMerge.makeReplacement(context, stack, skipping)
  }

  override def resolveStatus(): ResolveStatus = ResolveStatus.UNRESOLVED

  override def replaceChild(child: AbstractConfigValue, replacement: AbstractConfigValue): AbstractConfigValue = {
    val newStack = replaceChildInList(stack, child, replacement)
    if (newStack == null) null else new ConfigDelayedMerge(origin(), newStack)
  }

  override def hasDescendant(descendant: AbstractConfigValue): Boolean = hasDescendantInList(stack, descendant)

  override def relativized(prefix: Path): ConfigDelayedMerge = {
    val newStack = new ArrayList[AbstractConfigValue]()
    for (o <- stack) {
      newStack.add(o.relativized(prefix))
    }
    new ConfigDelayedMerge(origin(), newStack)
  }

  protected override def ignoresFallbacks(): Boolean = stackIgnoresFallbacks(stack)

  protected override def newCopy(newOrigin: ConfigOrigin): AbstractConfigValue = {
    new ConfigDelayedMerge(newOrigin, stack)
  }

  protected override def mergedWithTheUnmergeable(fallback: Unmergeable): ConfigDelayedMerge = {
    mergedWithTheUnmergeable(stack, fallback).asInstanceOf[ConfigDelayedMerge]
  }

  protected override def mergedWithObject(fallback: AbstractConfigObject): ConfigDelayedMerge = {
    mergedWithObject(stack, fallback).asInstanceOf[ConfigDelayedMerge]
  }

  protected override def mergedWithNonObject(fallback: AbstractConfigValue): ConfigDelayedMerge = {
    mergedWithNonObject(stack, fallback).asInstanceOf[ConfigDelayedMerge]
  }

  override def unmergedValues(): Collection[AbstractConfigValue] = stack

  protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[ConfigDelayedMerge]

  override def equals(other: Any): Boolean = other match {
    case other: ConfigDelayedMerge => canEqual(other) && 
      (this.stack == other.stack || this.stack == other.stack)
    case _ => false
  }

  override def hashCode(): Int = stack.hashCode

  protected override def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      atKey: String, 
      options: ConfigRenderOptions) {
    render(stack, sb, indent, atRoot, atKey, options)
  }
}
