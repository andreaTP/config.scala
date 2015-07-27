package com.typesafe.config

import java.io.IOException
import java.io.Serializable
import java.lang.reflect.Field
import com.typesafe.config.impl.ConfigImplUtil
import ConfigException.Null._
import ConfigException.ValidationFailed._
import ConfigException._
//remove if not needed
import scala.collection.JavaConversions._

object ConfigException {

  /**
   * Exception indicating that the type of a value does not match the type you
   * requested.
   *
   */
  @SerialVersionUID(1L)
  class WrongType(origin: ConfigOrigin,
      path: String,
      expected: String,
      actual: String,
      cause: Throwable) extends ConfigException(origin, path + " has type " + actual + " rather than " + expected,
    cause) {

    def this(origin: ConfigOrigin,
        path: String,
        expected: String,
        actual: String) {
      this(origin, path, expected, actual, null)
    }

    def this(origin: ConfigOrigin, message: String, cause: Throwable) {
      this(origin, message, null, null, cause)
    }

    def this(origin: ConfigOrigin, message: String) {
      this(origin, message, null, null, null)
    }
  }

  /**
   * Exception indicates that the setting was never set to anything, not even
   * null.
   */
  @SerialVersionUID(1L)
  class Missing(path: String, cause: Throwable) extends ConfigException("No configuration setting found for key '" + path + "'",
    cause) {

    def this(path: String) {
      this(path, null)
    }

    protected def this(origin: ConfigOrigin, message: String, cause: Throwable) {
      this(message, cause)
    }

    protected def this(origin: ConfigOrigin, message: String) {
      this(origin, message, null)
    }
  }

  object Null {

    private def makeMessage(path: String, expected: String): String = {
      if (expected != null) {
        "Configuration key '" + path + "' is set to null but expected " +
          expected
      } else {
        "Configuration key '" + path + "' is null"
      }
    }
  }

  /**
   * Exception indicates that the setting was treated as missing because it
   * was set to null.
   */
  @SerialVersionUID(1L)
  class Null(origin: ConfigOrigin,
      path: String,
      expected: String,
      cause: Throwable) extends Missing(origin, makeMessage(path, expected), cause) {

    def this(origin: ConfigOrigin, path: String, expected: String) {
      this(origin, path, expected, null)
    }
  }

  /**
   * Exception indicating that a value was messed up, for example you may have
   * asked for a duration and the value can't be sensibly parsed as a
   * duration.
   *
   */
  @SerialVersionUID(1L)
  class BadValue(origin: ConfigOrigin,
      path: String,
      message: String,
      cause: Throwable) extends ConfigException(origin, "Invalid value at '" + path + "': " + message,
    cause) {

    def this(origin: ConfigOrigin, path: String, message: String) {
      this(origin, path, message, null)
    }

    def this(path: String, message: String, cause: Throwable) {
      this(null, null, "Invalid value at '" + path + "': " + message, cause)
    }

    def this(path: String, message: String) {
      this(path, message, null)
    }
  }

  /**
   * Exception indicating that a path expression was invalid. Try putting
   * double quotes around path elements that contain "special" characters.
   *
   */
  @SerialVersionUID(1L)
  class BadPath(origin: ConfigOrigin,
      path: String,
      message: String,
      cause: Throwable) extends ConfigException(origin, if (path != null) ("Invalid path '" + path + "': " + message) else message,
    cause) {

    def this(origin: ConfigOrigin, path: String, message: String) {
      this(origin, path, message, null)
    }

    def this(path: String, message: String, cause: Throwable) {
      this(null, null, if (path != null) ("Invalid path '" + path + "': " + message) else message, cause)
    }

    def this(path: String, message: String) {
      this(path, message, null)
    }

    def this(origin: ConfigOrigin, message: String) {
      this(origin, null, message)
    }
  }

  /**
   * Exception indicating that there's a bug in something (possibly the
   * library itself) or the runtime environment is broken. This exception
   * should never be handled; instead, something should be fixed to keep the
   * exception from occurring. This exception can be thrown by any method in
   * the library.
   */
  @SerialVersionUID(1L)
  class BugOrBroken(message: String, cause: Throwable) extends ConfigException(message, cause) {

    def this(message: String) {
      this(message, null)
    }
  }

  /**
   * Exception indicating that there was an IO error.
   *
   */
  @SerialVersionUID(1L)
  class IO(origin: ConfigOrigin, message: String, cause: Throwable) extends ConfigException(origin, message,
    cause) {

    def this(origin: ConfigOrigin, message: String) {
      this(origin, message, null)
    }
  }

  /**
   * Exception indicating that there was a parse error.
   *
   */
  @SerialVersionUID(1L)
  class Parse(origin: ConfigOrigin, message: String, cause: Throwable) extends ConfigException(origin,
    message, cause) {

    def this(origin: ConfigOrigin, message: String) {
      this(origin, message, null)
    }
  }

  /**
   * Exception indicating that a substitution did not resolve to anything.
   * Thrown by {@link Config#resolve}.
   */
  @SerialVersionUID(1L)
  class UnresolvedSubstitution(origin: ConfigOrigin, detail: String, cause: Throwable)
      extends Parse(origin, "Could not resolve substitution to a value: " + detail, cause) {

    def this(origin: ConfigOrigin, detail: String) {
      this(origin, detail, null)
    }
  }

  /**
   * Exception indicating that you tried to use a function that requires
   * substitutions to be resolved, but substitutions have not been resolved
   * (that is, {@link Config#resolve} was not called). This is always a bug in
   * either application code or the library; it's wrong to write a handler for
   * this exception because you should be able to fix the code to avoid it by
   * adding calls to {@link Config#resolve}.
   */
  @SerialVersionUID(1L)
  class NotResolved(message: String, cause: Throwable) extends BugOrBroken(message, cause) {

    def this(message: String) {
      this(message, null)
    }
  }

  /**
   * Information about a problem that occurred in {@link Config#checkValid}. A
   * {@link ConfigException.ValidationFailed} exception thrown from
   * <code>checkValid()</code> includes a list of problems encountered.
   */
  class ValidationProblem(var path: String, var origin: ConfigOrigin, var problem: String)
      {

    override def toString(): String = {
      "ValidationProblem(" + path + "," + origin + "," + problem +
        ")"
    }
  }

  object ValidationFailed {

    private def makeMessage(problems: java.lang.Iterable[ValidationProblem]): String = {
      val sb = new StringBuilder()
      for (p <- problems) {
        sb.append(p.origin.description)
        sb.append(": ")
        sb.append(p.path)
        sb.append(": ")
        sb.append(p.problem)
        sb.append(", ")
      }
      if (sb.length == 0) throw new ConfigException.BugOrBroken("ValidationFailed must have a non-empty list of problems")
      sb.setLength(sb.length - 2)
      sb.toString
    }
  }

  /**
   * Exception indicating that {@link Config#checkValid} found validity
   * problems. The problems are available via the {@link #problems()} method.
   * The <code>getMessage()</code> of this exception is a potentially very
   * long string listing all the problems found.
   */
  @SerialVersionUID(1L)
  class ValidationFailed(var problems: java.lang.Iterable[ValidationProblem])
      extends ConfigException(makeMessage(problems), null)

  /**
   * Some problem with a JavaBean we are trying to initialize.
   * @since 1.3.0
   */
  @SerialVersionUID(1L)
  class BadBean(message: String, cause: Throwable) extends BugOrBroken(message, cause) {

    def this(message: String) {
      this(message, null)
    }
  }

  /**
   * Exception that doesn't fall into any other category.
   */
  @SerialVersionUID(1L)
  class Generic(message: String, cause: Throwable) extends ConfigException(message, cause) {

    def this(message: String) {
      this(message, null)
    }
  }
}

/**
 * All exceptions thrown by the library are subclasses of
 * <code>ConfigException</code>.
 */
@SerialVersionUID(1L)
abstract class ConfigException protected (var origin: ConfigOrigin, message: String, cause: Throwable)
    extends RuntimeException(origin.description() + ": " + message, cause) with Serializable {

  protected def this(origin: ConfigOrigin, message: String) {
    this(null, origin.description + ": " + message, null)
  }

  protected def this(message: String, cause: Throwable) {
    this(null, message, cause)
    this.origin = null
  }

  protected def this(message: String) {
    this(message, null)
  }

  private def writeObject(out: java.io.ObjectOutputStream) {
    out.defaultWriteObject()
    ConfigImplUtil.writeOrigin(out, origin)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    in.defaultReadObject()
    val origin = ConfigImplUtil.readOrigin(in)
    var f: Field = null
    f = classOf[ConfigException].getDeclaredField("origin")
    f.setAccessible(true)
    f.set(this, origin)
  }
}
