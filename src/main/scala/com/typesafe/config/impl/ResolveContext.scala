package com.typesafe.config.impl

import java.util.Collections
import java.util.IdentityHashMap
import java.util.List
import java.util.ArrayList
import java.util.Set
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigResolveOptions
import com.typesafe.config.impl.AbstractConfigValue.NotPossibleToResolve
import ResolveContext._
//remove if not needed
import scala.collection.JavaConversions._

object ResolveContext {

  private def newCycleMarkers(): Set[AbstractConfigValue] = {
    Collections.newSetFromMap(new IdentityHashMap[AbstractConfigValue, Boolean]())
  }

  def resolve(value: AbstractConfigValue, root: AbstractConfigObject, options: ConfigResolveOptions): AbstractConfigValue = {
    val source = new ResolveSource(root)
    val context = new ResolveContext(options, null)
    context.resolve(value, source).value
  }
}

class ResolveContext(val memos: ResolveMemos, 
    var options: ConfigResolveOptions, 
    var restrictToChild: Path, 
    resolveStack: List[AbstractConfigValue], 
    cycleMarkers: Set[AbstractConfigValue]) {

  private val resolveStack = Collections.unmodifiableList(resolveStack)

  private val cycleMarkers = Collections.unmodifiableSet(cycleMarkers)

  def this(options: ConfigResolveOptions, restrictToChild: Path) {
    this(new ResolveMemos(), options, restrictToChild, new ArrayList[AbstractConfigValue](), newCycleMarkers())
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "ResolveContext restrict to child " + restrictToChild)
  }

  def addCycleMarker(value: AbstractConfigValue): ResolveContext = {
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "++ Cycle marker " + value + "@" + System.identityHashCode(value))
    if (cycleMarkers.contains(value)) throw new ConfigException.BugOrBroken("Added cycle marker twice " + value)
    val copy = newCycleMarkers()
    copy.addAll(cycleMarkers)
    copy.add(value)
    new ResolveContext(memos, options, restrictToChild, resolveStack, copy)
  }

  def removeCycleMarker(value: AbstractConfigValue): ResolveContext = {
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "-- Cycle marker " + value + "@" + System.identityHashCode(value))
    val copy = newCycleMarkers()
    copy.addAll(cycleMarkers)
    copy.remove(value)
    new ResolveContext(memos, options, restrictToChild, resolveStack, copy)
  }

  private def memoize(key: MemoKey, value: AbstractConfigValue): ResolveContext = {
    val changed = memos.put(key, value)
    new ResolveContext(changed, options, restrictToChild, resolveStack, cycleMarkers)
  }

  def isRestrictedToChild(): Boolean = restrictToChild != null

  def restrict(restrictTo: Path): ResolveContext = {
    if (restrictTo == restrictToChild) this else new ResolveContext(memos, options, restrictTo, resolveStack, 
      cycleMarkers)
  }

  def unrestricted(): ResolveContext = restrict(null)

  def traceString(): String = {
    val separator = ", "
    val sb = new StringBuilder()
    for (value <- resolveStack if value.isInstanceOf[ConfigReference]) {
      sb.append(value.asInstanceOf[ConfigReference].expression().toString)
      sb.append(separator)
    }
    if (sb.length > 0) sb.setLength(sb.length - separator.length)
    sb.toString
  }

  private def pushTrace(value: AbstractConfigValue): ResolveContext = {
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "pushing trace " + value)
    val copy = new ArrayList[AbstractConfigValue](resolveStack)
    copy.add(value)
    new ResolveContext(memos, options, restrictToChild, copy, cycleMarkers)
  }

  def popTrace(): ResolveContext = {
    val copy = new ArrayList[AbstractConfigValue](resolveStack)
    val old = copy.remove(resolveStack.size - 1)
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth() - 1, "popped trace " + old)
    new ResolveContext(memos, options, restrictToChild, copy, cycleMarkers)
  }

  def depth(): Int = {
    if (resolveStack.size > 30) throw new ConfigException.BugOrBroken("resolve getting too deep")
    resolveStack.size
  }

  def resolve(original: AbstractConfigValue, source: ResolveSource): ResolveResult[_ <: AbstractConfigValue] = {
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "resolving " + original + " restrictToChild=" + restrictToChild + 
      " in " + 
      source)
    pushTrace(original).realResolve(original, source).popTrace()
  }

  private def realResolve(original: AbstractConfigValue, source: ResolveSource): ResolveResult[_ <: AbstractConfigValue] = {
    val fullKey = new MemoKey(original, null)
    var restrictedKey: MemoKey = null
    var cached = memos.get(fullKey)
    if (cached == null && isRestrictedToChild) {
      restrictedKey = new MemoKey(original, restrictToChild())
      cached = memos.get(restrictedKey)
    }
    if (cached != null) {
      if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "using cached resolution " + cached + " for " + original + 
        " restrictToChild " + 
        restrictToChild())
      ResolveResult.make(this, cached)
    } else {
      if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "not found in cache, resolving " + original + "@" + System.identityHashCode(original))
      if (cycleMarkers.contains(original)) {
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "Cycle detected, can't resolve; " + original + "@" + System.identityHashCode(original))
        throw new NotPossibleToResolve(this)
      }
      val result = original.resolveSubstitutions(this, source)
      val resolved = result.value
      if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "resolved to " + resolved + "@" + System.identityHashCode(resolved) + 
        " from " + 
        original + 
        "@" + 
        System.identityHashCode(resolved))
      var withMemo = result.context
      if (resolved == null || resolved.resolveStatus() == ResolveStatus.RESOLVED) {
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "caching " + fullKey + " result " + resolved)
        withMemo = withMemo.memoize(fullKey, resolved)
      } else {
        if (isRestrictedToChild) {
          if (restrictedKey == null) {
            throw new ConfigException.BugOrBroken("restrictedKey should not be null here")
          }
          if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "caching " + restrictedKey + " result " + resolved)
          withMemo = withMemo.memoize(restrictedKey, resolved)
        } else if (options().getAllowUnresolved) {
          if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(depth(), "caching " + fullKey + " result " + resolved)
          withMemo = withMemo.memoize(fullKey, resolved)
        } else {
          throw new ConfigException.BugOrBroken("resolveSubstitutions() did not give us a resolved object")
        }
      }
      ResolveResult.make(withMemo, resolved)
    }
  }
}
