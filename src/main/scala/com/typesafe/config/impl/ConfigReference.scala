package com.typesafe.config.impl

import java.util.Collection
import java.util.Collections
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValueType
//remove if not needed
import scala.collection.JavaConversions._

/**
 * ConfigReference replaces ConfigReference (the older class kept for back
 * compat) and represents the ${} substitution syntax. It can resolve to any
 * kind of value.
 */
class ConfigReference private (origin: ConfigOrigin, val expr: SubstitutionExpression, val prefixLength: Int)
    extends AbstractConfigValue(origin) with Unmergeable {

  def this(origin: ConfigOrigin, expr: SubstitutionExpression) {
    this(origin, expr, 0)
  }

  private def notResolved(): ConfigException.NotResolved = {
    new ConfigException.NotResolved("need to Config#resolve(), see the API docs for Config#resolve(); substitution not resolved: " + 
      this)
  }

  override def valueType(): ConfigValueType = throw notResolved()

  override def unwrapped(): AnyRef = throw notResolved()

  protected override def newCopy(newOrigin: ConfigOrigin): ConfigReference = {
    new ConfigReference(newOrigin, expr, prefixLength)
  }

  protected override def ignoresFallbacks(): Boolean = false

  override def unmergedValues(): Collection[ConfigReference] = Collections.singleton(this)

  override def resolveSubstitutions(context: ResolveContext, source: ResolveSource): ResolveResult[_ <: AbstractConfigValue] = {
    var newContext = context.addCycleMarker(this)
    var v: AbstractConfigValue = null
    try {
      val resultWithPath = source.lookupSubst(newContext, expr, prefixLength)
      newContext = resultWithPath.result.context
      if (resultWithPath.result.value != null) {
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "recursively resolving " + resultWithPath + " which was the resolution of " + 
          expr + 
          " against " + 
          source)
        val recursiveResolveSource = (new ResolveSource(resultWithPath.pathFromRoot.last().asInstanceOf[AbstractConfigObject], 
          resultWithPath.pathFromRoot))
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "will recursively resolve against " + recursiveResolveSource)
        val result = newContext.resolve(resultWithPath.result.value, recursiveResolveSource)
        v = result.value
        newContext = result.context
      } else {
        v = null
      }
    } catch {
      case e: NotPossibleToResolve => {
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(newContext.depth(), "not possible to resolve " + expr + ", cycle involved: " + 
          e.traceString())
        if (expr.optional()) v = null else throw new ConfigException.UnresolvedSubstitution(origin(), 
          expr + " was part of a cycle of substitutions involving " + 
          e.traceString(), e)
      }
    }
    if (v == null && !expr.optional()) {
      if (newContext.options().getAllowUnresolved) ResolveResult.make(newContext.removeCycleMarker(this), 
        this) else throw new ConfigException.UnresolvedSubstitution(origin(), expr.toString)
    } else {
      ResolveResult.make(newContext.removeCycleMarker(this), v)
    }
  }

  override def resolveStatus(): ResolveStatus = ResolveStatus.UNRESOLVED

  override def relativized(prefix: Path): ConfigReference = {
    val newExpr = expr.changePath(expr.path().prepend(prefix))
    new ConfigReference(origin(), newExpr, prefixLength + prefix.length)
  }

  protected override def canEqual(other: AnyRef): Boolean = other.isInstanceOf[ConfigReference]

  override def equals(other: Any): Boolean = other match {
    case other: ConfigReference => canEqual(other) && this.expr == other.expr
    case _ => false
  }

  override def hashCode(): Int = expr.hashCode

  protected override def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      options: ConfigRenderOptions) {
    sb.append(expr.toString)
  }

  def expression(): SubstitutionExpression = expr
}
