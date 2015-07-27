package com.typesafe.config.impl

import com.typesafe.config.ConfigException
import com.typesafe.config.impl.AbstractConfigValue.NotPossibleToResolve
import ResolveSource._
//remove if not needed
import scala.collection.JavaConversions._

object ResolveSource {

  private def findInObject(obj: AbstractConfigObject, context: ResolveContext, path: Path): ResultWithPath = {
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace("*** finding '" + path + "' in " + obj)
    val restriction = context.restrictToChild()
    val partiallyResolved = context.restrict(path).resolve(obj, new ResolveSource(obj))
    val newContext = partiallyResolved.context.restrict(restriction)
    if (partiallyResolved.value.isInstanceOf[AbstractConfigObject]) {
      val pair = findInObject(partiallyResolved.value.asInstanceOf[AbstractConfigObject], path)
      new ResultWithPath(ResolveResult.make(newContext, pair.value), pair.pathFromRoot)
    } else {
      throw new ConfigException.BugOrBroken("resolved object to non-object " + obj + " to " + partiallyResolved)
    }
  }

  private def findInObject(obj: AbstractConfigObject, path: Path): ValueWithPath = findInObject(obj, path, null)

  private def findInObject(obj: AbstractConfigObject, path: Path, parents: Node[Container]): ValueWithPath = {
    val key = path.first()
    val next = path.remainder()
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace("*** looking up '" + key + "' in " + obj)
    val v = obj.attemptPeekWithPartialResolve(key)
    val newParents = if (parents == null) new Node[Container](obj) else parents.prepend(obj)
    if (next == null) {
      new ValueWithPath(v, newParents)
    } else {
      if (v.isInstanceOf[AbstractConfigObject]) {
        findInObject(v.asInstanceOf[AbstractConfigObject], next, newParents)
      } else {
        new ValueWithPath(null, newParents)
      }
    }
  }

  private def replace(list: Node[Container], old: Container, replacement: AbstractConfigValue): Node[Container] = {
    val child = list.head()
    if (child != old) throw new ConfigException.BugOrBroken("Can only replace() the top node we're resolving; had " + 
      child + 
      " on top and tried to replace " + 
      old + 
      " overall list was " + 
      list)
    val parent = if (list.tail() == null) null else list.tail().head()
    if (replacement == null || !(replacement.isInstanceOf[Container])) {
      if (parent == null) {
        null
      } else {
        val newParent = parent.replaceChild(old.asInstanceOf[AbstractConfigValue], null)
        replace(list.tail(), parent, newParent)
      }
    } else {
      if (parent == null) {
        new Node[Container](replacement.asInstanceOf[Container])
      } else {
        val newParent = parent.replaceChild(old.asInstanceOf[AbstractConfigValue], replacement)
        val newTail = replace(list.tail(), parent, newParent)
        if (newTail != null) newTail.prepend(replacement.asInstanceOf[Container]) else new Node[Container](replacement.asInstanceOf[Container])
      }
    }
  }

  class Node[T](val value: T, val next: Node[T]) {

    def this(value: T) {
      this(value, null)
    }

    def prepend(value: T): Node[T] = new Node[T](value, this)

    def head(): T = value

    def tail(): Node[T] = next

    def last(): T = {
      var i = this
      while (i.next != null) i = i.next
      i.value
    }

    def reverse(): Node[T] = {
      if (next == null) {
        this
      } else {
        var reversed = new Node[T](value)
        var i = next
        while (i != null) {
          reversed = reversed.prepend(i.value)
          i = i.next
        }
        reversed
      }
    }

    override def toString(): String = {
      val sb = new StringBuffer()
      sb.append("[")
      var toAppendValue = this.reverse()
      while (toAppendValue != null) {
        sb.append(toAppendValue.value.toString)
        if (toAppendValue.next != null) sb.append(" <= ")
        toAppendValue = toAppendValue.next
      }
      sb.append("]")
      sb.toString
    }
  }

  class ValueWithPath(val value: AbstractConfigValue, val pathFromRoot: Node[Container])
      {

    override def toString(): String = {
      "ValueWithPath(value=" + value + ", pathFromRoot=" + pathFromRoot + 
        ")"
    }
  }

  class ResultWithPath(val result: ResolveResult[_ <: AbstractConfigValue], val pathFromRoot: Node[Container])
      {

    override def toString(): String = {
      "ResultWithPath(result=" + result + ", pathFromRoot=" + 
        pathFromRoot + 
        ")"
    }
  }
}

/**
 * This class is the source for values for a substitution like ${foo}.
 */
class ResolveSource(val root: AbstractConfigObject, val pathFromRoot: Node[Container])
    {

  def this(root: AbstractConfigObject) {
    this()
    this.root = root
    this.pathFromRoot = null
  }

  private def rootMustBeObj(value: Container): AbstractConfigObject = {
    if (value.isInstanceOf[AbstractConfigObject]) {
      value.asInstanceOf[AbstractConfigObject]
    } else {
      SimpleConfigObject.empty()
    }
  }

  def lookupSubst(context: ResolveContext, subst: SubstitutionExpression, prefixLength: Int): ResultWithPath = {
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(context.depth(), "searching for " + subst)
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(context.depth(), subst + " - looking up relative to file it occurred in")
    var result = findInObject(root, context, subst.path())
    if (result.result.value == null) {
      val unprefixed = subst.path().subPath(prefixLength)
      if (prefixLength > 0) {
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(result.result.context.depth(), unprefixed + " - looking up relative to parent file")
        result = findInObject(root, result.result.context, unprefixed)
      }
      if (result.result.value == null && 
        result.result.context.options().getUseSystemEnvironment) {
        if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(result.result.context.depth(), unprefixed + " - looking up in system environment")
        result = findInObject(ConfigImpl.envVariablesAsConfigObject(), context, unprefixed)
      }
    }
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace(result.result.context.depth(), "resolved to " + result)
    result
  }

  def pushParent(parent: Container): ResolveSource = {
    if (parent == null) throw new ConfigException.BugOrBroken("can't push null parent")
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace("pushing parent " + parent + " ==root " + (parent == root) + 
      " onto " + 
      this)
    if (pathFromRoot == null) {
      if (parent == root) {
        new ResolveSource(root, new Node[Container](parent))
      } else {
        if (ConfigImpl.traceSubstitutionsEnabled()) {
          if (root.hasDescendant(parent.asInstanceOf[AbstractConfigValue])) ConfigImpl.trace("***** BUG ***** tried to push parent " + parent + " without having a path to it in " + 
            this)
        }
        this
      }
    } else {
      val parentParent = pathFromRoot.head()
      if (ConfigImpl.traceSubstitutionsEnabled()) {
        if (parentParent != null && 
          !parentParent.hasDescendant(parent.asInstanceOf[AbstractConfigValue])) ConfigImpl.trace("***** BUG ***** trying to push non-child of " + parentParent + 
          ", non-child was " + 
          parent)
      }
      new ResolveSource(root, pathFromRoot.prepend(parent))
    }
  }

  def resetParents(): ResolveSource = {
    if (pathFromRoot == null) this else new ResolveSource(root)
  }

  def replaceCurrentParent(old: Container, replacement: Container): ResolveSource = {
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace("replaceCurrentParent old " + old + "@" + System.identityHashCode(old) + 
      " replacement " + 
      replacement + 
      "@" + 
      System.identityHashCode(old) + 
      " in " + 
      this)
    if (old == replacement) {
      this
    } else if (pathFromRoot != null) {
      val newPath = replace(pathFromRoot, old, replacement.asInstanceOf[AbstractConfigValue])
      if (ConfigImpl.traceSubstitutionsEnabled()) {
        ConfigImpl.trace("replaced " + old + " with " + replacement + " in " + 
          this)
        ConfigImpl.trace("path was: " + pathFromRoot + " is now " + newPath)
      }
      if (newPath != null) new ResolveSource(newPath.last().asInstanceOf[AbstractConfigObject], newPath) else new ResolveSource(SimpleConfigObject.empty())
    } else {
      if (old == root) {
        new ResolveSource(rootMustBeObj(replacement))
      } else {
        throw new ConfigException.BugOrBroken("attempt to replace root " + root + " with " + replacement)
      }
    }
  }

  def replaceWithinCurrentParent(old: AbstractConfigValue, replacement: AbstractConfigValue): ResolveSource = {
    if (ConfigImpl.traceSubstitutionsEnabled()) ConfigImpl.trace("replaceWithinCurrentParent old " + old + "@" + System.identityHashCode(old) + 
      " replacement " + 
      replacement + 
      "@" + 
      System.identityHashCode(old) + 
      " in " + 
      this)
    if (old == replacement) {
      this
    } else if (pathFromRoot != null) {
      val parent = pathFromRoot.head()
      val newParent = parent.replaceChild(old, replacement)
      replaceCurrentParent(parent, if ((newParent.isInstanceOf[Container])) newParent.asInstanceOf[Container] else null)
    } else {
      if (old == root && replacement.isInstanceOf[Container]) {
        new ResolveSource(rootMustBeObj(replacement.asInstanceOf[Container]))
      } else {
        throw new ConfigException.BugOrBroken("replace in parent not possible " + old + " with " + replacement + 
          " in " + 
          this)
      }
    }
  }

  override def toString(): String = {
    "ResolveSource(root=" + root + ", pathFromRoot=" + pathFromRoot + 
      ")"
  }
}
