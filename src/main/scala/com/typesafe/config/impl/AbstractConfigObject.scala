package com.typesafe.config.impl

import java.util.ArrayList
import java.util.Arrays
import java.util.Collection
import java.util.List
import java.util.Map
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigMergeable
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValue
import com.typesafe.config.ConfigValueType
import AbstractConfigObject._
//remove if not needed
import scala.collection.JavaConversions._

object AbstractConfigObject {

  private def peekPath(self: AbstractConfigObject, path: Path): AbstractConfigValue = {
    val next = path.remainder()
    val v = self.attemptPeekWithPartialResolve(path.first())
    if (next == null) {
      v
    } else {
      if (v.isInstanceOf[AbstractConfigObject]) {
        peekPath(v.asInstanceOf[AbstractConfigObject], next)
      } else {
        null
      }
    }
  }

  def mergeOrigins(stack: Collection[_ <: AbstractConfigValue]): ConfigOrigin = {
    if (stack.isEmpty) throw new ConfigException.BugOrBroken("can't merge origins on empty list")
    val origins = new ArrayList[ConfigOrigin]()
    var firstOrigin: ConfigOrigin = null
    var numMerged = 0
    for (v <- stack) {
      if (firstOrigin == null) firstOrigin = v.origin()
      if (v.isInstanceOf[AbstractConfigObject] && 
        v.asInstanceOf[AbstractConfigObject].resolveStatus() == 
        ResolveStatus.RESOLVED && 
        v.asInstanceOf[ConfigObject].isEmpty) {
      } else {
        origins.add(v.origin())
        numMerged += 1
      }
    }
    if (numMerged == 0) {
      origins.add(firstOrigin)
    }
    SimpleConfigOrigin.mergeOrigins(origins)
  }

  def mergeOrigins(stack: AbstractConfigObject*): ConfigOrigin = mergeOrigins(Arrays.asList(stack:_*))

  private def weAreImmutable(method: String): UnsupportedOperationException = {
    new UnsupportedOperationException("ConfigObject is immutable, you can't call Map." + method)
  }
}

abstract class AbstractConfigObject protected (origin: ConfigOrigin) extends AbstractConfigValue(origin) with ConfigObject with Container {

  private val config = new SimpleConfig(this)

  override def toConfig(): SimpleConfig = config

  override def toFallbackValue(): AbstractConfigObject = this

  override def withOnlyKey(key: String): AbstractConfigObject

  override def withoutKey(key: String): AbstractConfigObject

  override def withValue(key: String, value: ConfigValue): AbstractConfigObject

  protected def withOnlyPathOrNull(path: Path): AbstractConfigObject

  def withOnlyPath(path: Path): AbstractConfigObject

  def withoutPath(path: Path): AbstractConfigObject

  def withValue(path: Path, value: ConfigValue): AbstractConfigObject

  /**
   * This looks up the key with no transformation or type conversion of any
   * kind, and returns null if the key is not present. The object must be
   * resolved along the nodes needed to get the key or
   * ConfigException.NotResolved will be thrown.
   *
   * @param key
   * @return the unmodified raw value or null
   */
  protected def peekAssumingResolved(key: String, originalPath: Path): AbstractConfigValue = attemptPeekWithPartialResolve(key)

  /**
   * Look up the key on an only-partially-resolved object, with no
   * transformation or type conversion of any kind; if 'this' is not resolved
   * then try to look up the key anyway if possible.
   *
   * @param key
   *            key to look up
   * @return the value of the key, or null if known not to exist
   * @throws ConfigException.NotResolved
   *             if can't figure out key's value (or existence) without more
   *             resolving
   */
  def attemptPeekWithPartialResolve(key: String): AbstractConfigValue

  /**
   * Looks up the path with no transformation or type conversion. Returns null
   * if the path is not found; throws ConfigException.NotResolved if we need
   * to go through an unresolved node to look up the path.
   */
  protected def peekPath(path: Path): AbstractConfigValue = peekPath(this, path)

  override def valueType(): ConfigValueType = ConfigValueType.OBJECT

  protected def newCopy(status: ResolveStatus, origin: ConfigOrigin): AbstractConfigObject

  protected override def newCopy(origin: ConfigOrigin): AbstractConfigObject = newCopy(resolveStatus(), origin)

  protected override def constructDelayedMerge(origin: ConfigOrigin, stack: List[AbstractConfigValue]): AbstractConfigObject = {
    new ConfigDelayedMergeObject(origin, stack)
  }

  protected override def mergedWithObject(fallback: AbstractConfigObject): AbstractConfigObject

  override def withFallback(mergeable: ConfigMergeable): AbstractConfigObject = {
    super.withFallback(mergeable).asInstanceOf[AbstractConfigObject]
  }

  override def resolveSubstitutions(context: ResolveContext, source: ResolveSource): ResolveResult[_ <: AbstractConfigObject]

  override def relativized(prefix: Path): AbstractConfigObject

  override def get(key: AnyRef): AbstractConfigValue

  protected override def render(sb: StringBuilder, 
      indent: Int, 
      atRoot: Boolean, 
      options: ConfigRenderOptions): Unit

  override def clear() {
    throw weAreImmutable("clear")
  }

  override def put(arg0: String, arg1: ConfigValue): ConfigValue = throw weAreImmutable("put")

  override def putAll(arg0: Map[_ <: String, _ <: ConfigValue]) {
    throw weAreImmutable("putAll")
  }

  override def remove(arg0: AnyRef): ConfigValue = throw weAreImmutable("remove")

  override def withOrigin(origin: ConfigOrigin): AbstractConfigObject = {
    super.withOrigin(origin).asInstanceOf[AbstractConfigObject]
  }
}
