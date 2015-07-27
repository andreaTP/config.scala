package com.typesafe.config.impl

import java.io.ObjectStreamException
import java.io.Serializable
import java.math.BigDecimal
import java.math.BigInteger
import java.time.Duration
import java.util.AbstractMap
import java.util.ArrayList
import java.util.HashMap
import java.util.HashSet
import java.util.List
import java.util.Map
import java.util.Set
import java.util.concurrent.TimeUnit
import com.typesafe.config.Config
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigList
import com.typesafe.config.ConfigMemorySize
import com.typesafe.config.ConfigMergeable
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigOrigin
import com.typesafe.config.ConfigResolveOptions
import com.typesafe.config.ConfigValue
import com.typesafe.config.ConfigValueType
import SimpleConfig._
//remove if not needed
import scala.collection.JavaConversions._

object SimpleConfig {

  private def findPaths(entries: Set[Map.Entry[String, ConfigValue]], parent: Path, obj: AbstractConfigObject) {
    for ((key, value) <- obj) {
      val elem = key
      val v = value
      var path = Path.newKey(elem)
      if (parent != null) path = path.prepend(parent)
      if (v.isInstanceOf[AbstractConfigObject]) {
        findPaths(entries, path, v.asInstanceOf[AbstractConfigObject])
      } else if (v.isInstanceOf[ConfigNull]) {
      } else {
        entries.add(new AbstractMap.SimpleImmutableEntry[String, ConfigValue](path.render(), v))
      }
    }
  }

  private def throwIfNull(v: AbstractConfigValue, expected: ConfigValueType, originalPath: Path): AbstractConfigValue = {
    if (v.valueType() == ConfigValueType.NULL) throw new ConfigException.Null(v.origin(), originalPath.render(),
      if (expected != null) expected.name() else null) else v
  }

  private def findKey(self: AbstractConfigObject,
      key: String,
      expected: ConfigValueType,
      originalPath: Path): AbstractConfigValue = {
    throwIfNull(findKeyOrNull(self, key, expected, originalPath), expected, originalPath)
  }

  private def findKeyOrNull(self: AbstractConfigObject,
      key: String,
      expected: ConfigValueType,
      originalPath: Path): AbstractConfigValue = {
    var v = self.peekAssumingResolved(key, originalPath)
    if (v == null) throw new ConfigException.Missing(originalPath.render())
    if (expected != null) v = DefaultTransformer.transform(v, expected)
    if (expected != null &&
      (v.valueType() != expected && v.valueType() != ConfigValueType.NULL)) throw new ConfigException.WrongType(v.origin(),
      originalPath.render(), expected.name(), v.valueType().name()) else v
  }

  private def findOrNull(self: AbstractConfigObject,
      path: Path,
      expected: ConfigValueType,
      originalPath: Path): AbstractConfigValue = {
    val key = path.first()
    val next = path.remainder()
    if (next == null) {
      findKeyOrNull(self, key, expected, originalPath)
    } else {
      val o = findKey(self, key, ConfigValueType.OBJECT, originalPath.subPath(0, originalPath.length - next.length)).asInstanceOf[AbstractConfigObject]
      assert((o != null))
      findOrNull(o, next, expected, originalPath)
    }
  }

  private def getUnits(s: String): String = {
    var i = s.length - 1
    while (i >= 0) {
      val c = s.charAt(i)
      if (!Character.isLetter(c)) //break
      i -= 1
    }
    s.substring(i + 1)
  }

  /**
   * Parses a duration string. If no units are specified in the string, it is
   * assumed to be in milliseconds. The returned duration is in nanoseconds.
   * The purpose of this function is to implement the duration-related methods
   * in the ConfigObject interface.
   *
   * @param input
   *            the string to parse
   * @param originForException
   *            origin of the value being parsed
   * @param pathForException
   *            path to include in exceptions
   * @return duration in nanoseconds
   * @throws ConfigException
   *             if string is invalid
   */
  def parseDuration(input: String, originForException: ConfigOrigin, pathForException: String): Long = {
    val s = ConfigImplUtil.unicodeTrim(input)
    val originalUnitString = getUnits(s)
    var unitString = originalUnitString
    val numberString = ConfigImplUtil.unicodeTrim(s.substring(0, s.length - unitString.length))
    var units: TimeUnit = null
    if (numberString.length == 0) throw new ConfigException.BadValue(originForException, pathForException,
      "No number in duration value '" + input + "'")
    if (unitString.length > 2 && !unitString.endsWith("s")) unitString = unitString + "s"
    if (unitString == "" || unitString == "ms" || unitString == "millis" ||
      unitString == "milliseconds") {
      units = TimeUnit.MILLISECONDS
    } else if (unitString == "us" || unitString == "micros" || unitString == "microseconds") {
      units = TimeUnit.MICROSECONDS
    } else if (unitString == "ns" || unitString == "nanos" || unitString == "nanoseconds") {
      units = TimeUnit.NANOSECONDS
    } else if (unitString == "d" || unitString == "days") {
      units = TimeUnit.DAYS
    } else if (unitString == "h" || unitString == "hours") {
      units = TimeUnit.HOURS
    } else if (unitString == "s" || unitString == "seconds") {
      units = TimeUnit.SECONDS
    } else if (unitString == "m" || unitString == "minutes") {
      units = TimeUnit.MINUTES
    } else {
      throw new ConfigException.BadValue(originForException, pathForException, "Could not parse time unit '" + originalUnitString + "' (try ns, us, ms, s, m, h, d)")
    }
    if (numberString.matches("[0-9]+")) {
      units.toNanos(Long.parseLong(numberString))
    } else {
      val nanosInUnit = units.toNanos(1)
      (Double.parseDouble(numberString) * nanosInUnit).toLong
    }
  }

  object MemoryUnit extends Enumeration {

    val BYTES = new MemoryUnit("", 1024, 0)

    val KILOBYTES = new MemoryUnit("kilo", 1000, 1)

    val MEGABYTES = new MemoryUnit("mega", 1000, 2)

    val GIGABYTES = new MemoryUnit("giga", 1000, 3)

    val TERABYTES = new MemoryUnit("tera", 1000, 4)

    val PETABYTES = new MemoryUnit("peta", 1000, 5)

    val EXABYTES = new MemoryUnit("exa", 1000, 6)

    val ZETTABYTES = new MemoryUnit("zetta", 1000, 7)

    val YOTTABYTES = new MemoryUnit("yotta", 1000, 8)

    val KIBIBYTES = new MemoryUnit("kibi", 1024, 1)

    val MEBIBYTES = new MemoryUnit("mebi", 1024, 2)

    val GIBIBYTES = new MemoryUnit("gibi", 1024, 3)

    val TEBIBYTES = new MemoryUnit("tebi", 1024, 4)

    val PEBIBYTES = new MemoryUnit("pebi", 1024, 5)

    val EXBIBYTES = new MemoryUnit("exbi", 1024, 6)

    val ZEBIBYTES = new MemoryUnit("zebi", 1024, 7)

    val YOBIBYTES = new MemoryUnit("yobi", 1024, 8)

    class MemoryUnit(val prefix: String, val powerOf: Int, val power: Int) extends Val {

      val bytes = BigInteger.valueOf(powerOf).pow(power)
    }

    private def makeUnitsMap(): Map[String, MemoryUnit] = {
      val map = new HashMap[String, MemoryUnit]()
      for (unit <- MemoryUnit.values) {
        map.put(unit.prefix + "byte", unit)
        map.put(unit.prefix + "bytes", unit)
        if (unit.prefix.length == 0) {
          map.put("b", unit)
          map.put("B", unit)
          map.put("", unit)
        } else {
          val first = unit.prefix.substring(0, 1)
          val firstUpper = first.toUpperCase()
          if (unit.powerOf == 1024) {
            map.put(first, unit)
            map.put(firstUpper, unit)
            map.put(firstUpper + "i", unit)
            map.put(firstUpper + "iB", unit)
          } else if (unit.powerOf == 1000) {
            if (unit.power == 1) {
              map.put(first + "B", unit)
            } else {
              map.put(firstUpper + "B", unit)
            }
          } else {
            throw new RuntimeException("broken MemoryUnit enum")
          }
        }
      }
      map
    }

    private var unitsMap: Map[String, MemoryUnit] = makeUnitsMap()

    def parseUnit(unit: String): MemoryUnit = unitsMap.get(unit)

    implicit def convertValue(v: Value): MemoryUnit = v.asInstanceOf[MemoryUnit]
  }

  /**
   * Parses a size-in-bytes string. If no units are specified in the string,
   * it is assumed to be in bytes. The returned value is in bytes. The purpose
   * of this function is to implement the size-in-bytes-related methods in the
   * Config interface.
   *
   * @param input
   *            the string to parse
   * @param originForException
   *            origin of the value being parsed
   * @param pathForException
   *            path to include in exceptions
   * @return size in bytes
   * @throws ConfigException
   *             if string is invalid
   */
  def parseBytes(input: String, originForException: ConfigOrigin, pathForException: String): Long = {
    val s = ConfigImplUtil.unicodeTrim(input)
    val unitString = getUnits(s)
    val numberString = ConfigImplUtil.unicodeTrim(s.substring(0, s.length - unitString.length))
    if (numberString.length == 0) throw new ConfigException.BadValue(originForException, pathForException,
      "No number in size-in-bytes value '" + input + "'")
    val units = MemoryUnit.parseUnit(unitString)
    if (units == null) {
      throw new ConfigException.BadValue(originForException, pathForException, "Could not parse size-in-bytes unit '" + unitString +
        "' (try k, K, kB, KiB, kilobytes, kibibytes)")
    }
    var result: BigInteger = null
    if (numberString.matches("[0-9]+")) {
      result = units.bytes.multiply(new BigInteger(numberString))
    } else {
      val resultDecimal = (new BigDecimal(units.bytes)).multiply(new BigDecimal(numberString))
      result = resultDecimal.toBigInteger()
    }
    if (result.bitLength() < 64) result.longValue() else throw new ConfigException.BadValue(originForException,
      pathForException, "size-in-bytes value is out of range for a 64-bit long: '" +
      input +
      "'")
  }

  private def addProblem(accumulator: List[ConfigException.ValidationProblem],
      path: Path,
      origin: ConfigOrigin,
      problem: String) {
    accumulator.add(new ConfigException.ValidationProblem(path.render(), origin, problem))
  }

  private def getDesc(`type`: ConfigValueType): String = `type`.name().toLowerCase()

  private def getDesc(refValue: ConfigValue): String = {
    if (refValue.isInstanceOf[AbstractConfigObject]) {
      val obj = refValue.asInstanceOf[AbstractConfigObject]
      if (!obj.isEmpty) "object with keys " + obj.keySet else getDesc(refValue.valueType())
    } else {
      getDesc(refValue.valueType())
    }
  }

  private def addMissing(accumulator: List[ConfigException.ValidationProblem],
      refDesc: String,
      path: Path,
      origin: ConfigOrigin) {
    addProblem(accumulator, path, origin, "No setting at '" + path.render() + "', expecting: " +
      refDesc)
  }

  private def addMissing(accumulator: List[ConfigException.ValidationProblem],
      refValue: ConfigValue,
      path: Path,
      origin: ConfigOrigin) {
    addMissing(accumulator, getDesc(refValue), path, origin)
  }

  def addMissing(accumulator: List[ConfigException.ValidationProblem],
      refType: ConfigValueType,
      path: Path,
      origin: ConfigOrigin) {
    addMissing(accumulator, getDesc(refType), path, origin)
  }

  private def addWrongType(accumulator: List[ConfigException.ValidationProblem],
      refDesc: String,
      actual: AbstractConfigValue,
      path: Path) {
    addProblem(accumulator, path, actual.origin(), "Wrong value type at '" + path.render() + "', expecting: " +
      refDesc +
      " but got: " +
      getDesc(actual))
  }

  private def addWrongType(accumulator: List[ConfigException.ValidationProblem],
      refValue: ConfigValue,
      actual: AbstractConfigValue,
      path: Path) {
    addWrongType(accumulator, getDesc(refValue), actual, path)
  }

  private def addWrongType(accumulator: List[ConfigException.ValidationProblem],
      refType: ConfigValueType,
      actual: AbstractConfigValue,
      path: Path) {
    addWrongType(accumulator, getDesc(refType), actual, path)
  }

  private def couldBeNull(v: AbstractConfigValue): Boolean = {
    DefaultTransformer.transform(v, ConfigValueType.NULL)
      .valueType() ==
      ConfigValueType.NULL
  }

  private def haveCompatibleTypes(reference: ConfigValue, value: AbstractConfigValue): Boolean = {
    if (couldBeNull(reference.asInstanceOf[AbstractConfigValue])) {
      true
    } else {
      haveCompatibleTypes(reference.valueType(), value)
    }
  }

  private def haveCompatibleTypes(referenceType: ConfigValueType, value: AbstractConfigValue): Boolean = {
    if (referenceType == ConfigValueType.NULL || couldBeNull(value)) {
      true
    } else if (referenceType == ConfigValueType.OBJECT) {
      if (value.isInstanceOf[AbstractConfigObject]) {
        true
      } else {
        false
      }
    } else if (referenceType == ConfigValueType.LIST) {
      if (value.isInstanceOf[SimpleConfigList] || value.isInstanceOf[SimpleConfigObject]) {
        true
      } else {
        false
      }
    } else if (referenceType == ConfigValueType.STRING) {
      true
    } else if (value.isInstanceOf[ConfigString]) {
      true
    } else {
      if (referenceType == value.valueType()) {
        true
      } else {
        false
      }
    }
  }

  private def checkValidObject(path: Path,
      reference: AbstractConfigObject,
      value: AbstractConfigObject,
      accumulator: List[ConfigException.ValidationProblem]) {
    for ((key, value) <- reference) {
      val key = key
      var childPath: Path = null
      childPath = if (path != null) Path.newKey(key).prepend(path) else Path.newKey(key)
      val v = value.get(key)
      if (v == null) {
        addMissing(accumulator, value, childPath, value.origin())
      } else {
        checkValid(childPath, value, v, accumulator)
      }
    }
  }

  private def checkListCompatibility(path: Path,
      listRef: SimpleConfigList,
      listValue: SimpleConfigList,
      accumulator: List[ConfigException.ValidationProblem]) {
    if (listRef.isEmpty || listValue.isEmpty) {
    } else {
      val refElement = listRef.get(0)
      for (elem <- listValue) {
        val e = elem.asInstanceOf[AbstractConfigValue]
        if (!haveCompatibleTypes(refElement, e)) {
          addProblem(accumulator, path, e.origin(), "List at '" + path.render() + "' contains wrong value type, expecting list of " +
            getDesc(refElement) +
            " but got element of type " +
            getDesc(e))
          //break
        }
      }
    }
  }

  def checkValid(path: Path,
      referenceType: ConfigValueType,
      value: AbstractConfigValue,
      accumulator: List[ConfigException.ValidationProblem]) {
    if (haveCompatibleTypes(referenceType, value)) {
      if (referenceType == ConfigValueType.LIST && value.isInstanceOf[SimpleConfigObject]) {
        val listValue = DefaultTransformer.transform(value, ConfigValueType.LIST)
        if (!(listValue.isInstanceOf[SimpleConfigList])) addWrongType(accumulator, referenceType, value,
          path)
      }
    } else {
      addWrongType(accumulator, referenceType, value, path)
    }
  }

  private def checkValid(path: Path,
      reference: ConfigValue,
      value: AbstractConfigValue,
      accumulator: List[ConfigException.ValidationProblem]) {
    if (haveCompatibleTypes(reference, value)) {
      if (reference.isInstanceOf[AbstractConfigObject] && value.isInstanceOf[AbstractConfigObject]) {
        checkValidObject(path, reference.asInstanceOf[AbstractConfigObject], value.asInstanceOf[AbstractConfigObject],
          accumulator)
      } else if (reference.isInstanceOf[SimpleConfigList] && value.isInstanceOf[SimpleConfigList]) {
        val listRef = reference.asInstanceOf[SimpleConfigList]
        val listValue = value.asInstanceOf[SimpleConfigList]
        checkListCompatibility(path, listRef, listValue, accumulator)
      } else if (reference.isInstanceOf[SimpleConfigList] && value.isInstanceOf[SimpleConfigObject]) {
        val listRef = reference.asInstanceOf[SimpleConfigList]
        val listValue = DefaultTransformer.transform(value, ConfigValueType.LIST)
        if (listValue.isInstanceOf[SimpleConfigList]) checkListCompatibility(path, listRef, listValue.asInstanceOf[SimpleConfigList],
          accumulator) else addWrongType(accumulator, reference, value, path)
      }
    } else {
      addWrongType(accumulator, reference, value, path)
    }
  }
}

/**
 * One thing to keep in mind in the future: as Collection-like APIs are added
 * here, including iterators or size() or anything, they should be consistent
 * with a one-level java.util.Map from paths to non-null values. Null values are
 * not "in" the map.
 */
@SerialVersionUID(1L)
class SimpleConfig(val `object`: AbstractConfigObject) extends Config with MergeableValue with Serializable {

  override def root(): AbstractConfigObject = `object`

  override def origin(): ConfigOrigin = `object`.origin()

  override def resolve(): SimpleConfig = {
    resolve(ConfigResolveOptions.defaults())
  }

  override def resolve(options: ConfigResolveOptions): SimpleConfig = resolveWith(this, options)

  override def resolveWith(source: Config): SimpleConfig = {
    resolveWith(source, ConfigResolveOptions.defaults())
  }

  override def resolveWith(source: Config, options: ConfigResolveOptions): SimpleConfig = {
    val resolved = ResolveContext.resolve(`object`, source.asInstanceOf[SimpleConfig].`object`, options)
    if (resolved == `object`) this else new SimpleConfig(resolved.asInstanceOf[AbstractConfigObject])
  }

  private def hasPathPeek(pathExpression: String): ConfigValue = {
    val path = Path.newPath(pathExpression)
    var peeked: ConfigValue = null
    peeked = `object`.peekPath(path)
    peeked
  }

  override def hasPath(pathExpression: String): Boolean = {
    val peeked = hasPathPeek(pathExpression)
    peeked != null && peeked.valueType() != ConfigValueType.NULL
  }

  override def hasPathOrNull(path: String): Boolean = {
    val peeked = hasPathPeek(path)
    peeked != null
  }

  override def isEmpty(): Boolean = `object`.isEmpty

  override def entrySet(): Set[Map.Entry[String, ConfigValue]] = {
    val entries = new HashSet[Map.Entry[String, ConfigValue]]()
    findPaths(entries, null, `object`)
    entries
  }

  def find(pathExpression: Path, expected: ConfigValueType, originalPath: Path): AbstractConfigValue = {
    throwIfNull(findOrNull(`object`, pathExpression, expected, originalPath), expected, originalPath)
  }

  def find(pathExpression: String, expected: ConfigValueType): AbstractConfigValue = {
    val path = Path.newPath(pathExpression)
    find(path, expected, path)
  }

  private def findOrNull(pathExpression: Path, expected: ConfigValueType, originalPath: Path): AbstractConfigValue = {
    findOrNull(`object`, pathExpression, expected, originalPath)
  }

  private def findOrNull(pathExpression: String, expected: ConfigValueType): AbstractConfigValue = {
    val path = Path.newPath(pathExpression)
    findOrNull(path, expected, path)
  }

  override def getValue(path: String): AbstractConfigValue = find(path, null)

  override def getIsNull(path: String): Boolean = {
    val v = findOrNull(path, null)
    (v.valueType() == ConfigValueType.NULL)
  }

  override def getBoolean(path: String): Boolean = {
    val v = find(path, ConfigValueType.BOOLEAN)
    v.unwrapped().asInstanceOf[java.lang.Boolean]
  }

  private def getConfigNumber(path: String): ConfigNumber = {
    val v = find(path, ConfigValueType.NUMBER)
    v.asInstanceOf[ConfigNumber]
  }

  override def getNumber(path: String): Number = getConfigNumber(path).unwrapped()

  override def getInt(path: String): Int = {
    val n = getConfigNumber(path)
    n.intValueRangeChecked(path)
  }

  override def getLong(path: String): Long = getNumber(path).longValue()

  override def getDouble(path: String): Double = getNumber(path).doubleValue()

  override def getString(path: String): String = {
    val v = find(path, ConfigValueType.STRING)
    v.unwrapped().asInstanceOf[String]
  }

  override def getList(path: String): ConfigList = {
    val v = find(path, ConfigValueType.LIST)
    v.asInstanceOf[ConfigList]
  }

  override def getObject(path: String): AbstractConfigObject = {
    val obj = find(path, ConfigValueType.OBJECT).asInstanceOf[AbstractConfigObject]
    obj
  }

  override def getConfig(path: String): SimpleConfig = getObject(path).toConfig()

  override def getAnyRef(path: String): AnyRef = {
    val v = find(path, null)
    v.unwrapped()
  }

  override def getBytes(path: String): java.lang.Long = {
    var size: java.lang.Long = null
    try {
      size = getLong(path)
    } catch {
      case e: ConfigException.WrongType => {
        val v = find(path, ConfigValueType.STRING)
        size = parseBytes(v.unwrapped().asInstanceOf[String], v.origin(), path)
      }
    }
    size
  }

  override def getMemorySize(path: String): ConfigMemorySize = {
    ConfigMemorySize.ofBytes(getBytes(path))
  }

  @Deprecated
  override def getMilliseconds(path: String): java.lang.Long = {
    getDuration(path, TimeUnit.MILLISECONDS)
  }

  @Deprecated
  override def getNanoseconds(path: String): java.lang.Long = getDuration(path, TimeUnit.NANOSECONDS)

  override def getDuration(path: String, unit: TimeUnit): Long = {
    val v = find(path, ConfigValueType.STRING)
    val result = unit.convert(parseDuration(v.unwrapped().asInstanceOf[String], v.origin(), path), TimeUnit.NANOSECONDS)
    result
  }

  override def getDuration(path: String): Duration = {
    val v = find(path, ConfigValueType.STRING)
    val nanos = parseDuration(v.unwrapped().asInstanceOf[String], v.origin(), path)
    Duration.ofNanos(nanos)
  }

  private def getHomogeneousUnwrappedList[T](path: String, expected: ConfigValueType): List[T] = {
    val l = new ArrayList[T]()
    val list = getList(path)
    for (cv <- list) {
      var v = cv.asInstanceOf[AbstractConfigValue]
      if (expected != null) {
        v = DefaultTransformer.transform(v, expected)
      }
      if (v.valueType() != expected) throw new ConfigException.WrongType(v.origin(), path, "list of " + expected.name(),
        "list of " + v.valueType().name())
      l.add(v.unwrapped().asInstanceOf[T])
    }
    l
  }

  override def getBooleanList(path: String): List[Boolean] = {
    getHomogeneousUnwrappedList(path, ConfigValueType.BOOLEAN)
  }

  override def getNumberList(path: String): List[Number] = {
    getHomogeneousUnwrappedList(path, ConfigValueType.NUMBER)
  }

  override def getIntList(path: String): List[Integer] = {
    val l = new ArrayList[Integer]()
    val numbers = getHomogeneousWrappedList(path, ConfigValueType.NUMBER)
    for (v <- numbers) {
      l.add(v.asInstanceOf[ConfigNumber].intValueRangeChecked(path))
    }
    l
  }

  override def getLongList(path: String): List[Long] = {
    val l = new ArrayList[Long]()
    val numbers = getNumberList(path)
    for (n <- numbers) {
      l.add(n.longValue())
    }
    l
  }

  override def getDoubleList(path: String): List[Double] = {
    val l = new ArrayList[Double]()
    val numbers = getNumberList(path)
    for (n <- numbers) {
      l.add(n.doubleValue())
    }
    l
  }

  override def getStringList(path: String): List[String] = {
    getHomogeneousUnwrappedList(path, ConfigValueType.STRING)
  }

  private def getHomogeneousWrappedList[T <: ConfigValue](path: String, expected: ConfigValueType): List[T] = {
    val l = new ArrayList[T]()
    val list = getList(path)
    for (cv <- list) {
      var v = cv.asInstanceOf[AbstractConfigValue]
      if (expected != null) {
        v = DefaultTransformer.transform(v, expected)
      }
      if (v.valueType() != expected) throw new ConfigException.WrongType(v.origin(), path, "list of " + expected.name(),
        "list of " + v.valueType().name())
      l.add(v.asInstanceOf[T])
    }
    l
  }

  override def getObjectList(path: String): List[ConfigObject] = {
    getHomogeneousWrappedList(path, ConfigValueType.OBJECT)
  }

  override def getConfigList(path: String): List[_ <: Config] = {
    val objects = getObjectList(path)
    val l = new ArrayList[Config]()
    for (o <- objects) {
      l.add(o.toConfig())
    }
    l
  }

  override def getAnyRefList(path: String): List[_ <: Any] = {
    val l = new ArrayList[Any]()
    val list = getList(path)
    for (v <- list) {
      l.add(v.unwrapped())
    }
    l
  }

  override def getBytesList(path: String): List[Long] = {
    val l = new ArrayList[Long]()
    val list = getList(path)
    for (v <- list) {
      if (v.valueType() == ConfigValueType.NUMBER) {
        l.add(v.unwrapped().asInstanceOf[Number].longValue())
      } else if (v.valueType() == ConfigValueType.STRING) {
        val s = v.unwrapped().asInstanceOf[String]
        val n = parseBytes(s, v.origin(), path)
        l.add(n)
      } else {
        throw new ConfigException.WrongType(v.origin(), path, "memory size string or number of bytes",
          v.valueType().name())
      }
    }
    l
  }

  override def getMemorySizeList(path: String): List[ConfigMemorySize] = {
    val list = getBytesList(path)
    val builder = new ArrayList[ConfigMemorySize]()
    for (v <- list) {
      builder.add(ConfigMemorySize.ofBytes(v))
    }
    builder
  }

  override def getDurationList(path: String, unit: TimeUnit): List[Long] = {
    val l = new ArrayList[Long]()
    val list = getList(path)
    for (v <- list) {
      if (v.valueType() == ConfigValueType.NUMBER) {
        val n = unit.convert(v.unwrapped().asInstanceOf[Number].longValue(), TimeUnit.MILLISECONDS)
        l.add(n)
      } else if (v.valueType() == ConfigValueType.STRING) {
        val s = v.unwrapped().asInstanceOf[String]
        val n = unit.convert(parseDuration(s, v.origin(), path), TimeUnit.NANOSECONDS)
        l.add(n)
      } else {
        throw new ConfigException.WrongType(v.origin(), path, "duration string or number of milliseconds",
          v.valueType().name())
      }
    }
    l
  }

  override def getDurationList(path: String): List[Duration] = {
    val l = getDurationList(path, TimeUnit.NANOSECONDS)
    val builder = new ArrayList[Duration](l.size)
    for (value <- l) {
      builder.add(Duration.ofNanos(value))
    }
    builder
  }

  @Deprecated
  override def getMillisecondsList(path: String): List[Long] = {
    getDurationList(path, TimeUnit.MILLISECONDS)
  }

  @Deprecated
  override def getNanosecondsList(path: String): List[Long] = {
    getDurationList(path, TimeUnit.NANOSECONDS)
  }

  override def toFallbackValue(): AbstractConfigObject = `object`

  override def withFallback(other: ConfigMergeable): SimpleConfig = `object`.withFallback(other).toConfig()

  override def equals(other: Any): Boolean = other match {
    case other: SimpleConfig => `object` == other.`object`
    case _ => false
  }

  override def hashCode(): Int = 41 * `object`.hashCode

  override def toString(): String = "Config(" + `object`.toString + ")"

  private def peekPath(path: Path): AbstractConfigValue = root().peekPath(path)

  override def isResolved(): Boolean = {
    root().resolveStatus() == ResolveStatus.RESOLVED
  }

  override def checkValid(reference: Config, restrictToPaths: String*) {
    val ref = reference.asInstanceOf[SimpleConfig]
    if (ref.root().resolveStatus() != ResolveStatus.RESOLVED) throw new ConfigException.BugOrBroken("do not call checkValid() with an unresolved reference config, call Config#resolve(), see Config#resolve() API docs")
    if (root().resolveStatus() != ResolveStatus.RESOLVED) throw new ConfigException.NotResolved("need to Config#resolve() each config before using it, see the API docs for Config#resolve()")
    val problems = new ArrayList[ConfigException.ValidationProblem]()
    if (restrictToPaths.length == 0) {
      checkValidObject(null, ref.root(), root(), problems)
    } else {
      for (p <- restrictToPaths) {
        val path = Path.newPath(p)
        val refValue = ref.peekPath(path)
        if (refValue != null) {
          val child = peekPath(path)
          if (child != null) {
            checkValid(path, refValue, child, problems)
          } else {
            addMissing(problems, refValue, path, origin())
          }
        }
      }
    }
    if (!problems.isEmpty) {
      throw new ConfigException.ValidationFailed(problems)
    }
  }

  override def withOnlyPath(pathExpression: String): SimpleConfig = {
    val path = Path.newPath(pathExpression)
    new SimpleConfig(root().withOnlyPath(path))
  }

  override def withoutPath(pathExpression: String): SimpleConfig = {
    val path = Path.newPath(pathExpression)
    new SimpleConfig(root().withoutPath(path))
  }

  override def withValue(pathExpression: String, v: ConfigValue): SimpleConfig = {
    val path = Path.newPath(pathExpression)
    new SimpleConfig(root().withValue(path, v))
  }

  def atKey(origin: ConfigOrigin, key: String): SimpleConfig = root().atKey(origin, key)

  override def atKey(key: String): SimpleConfig = root().atKey(key)

  override def atPath(path: String): Config = root().atPath(path)

  private def writeReplace(): AnyRef = new SerializedConfigValue(this)
}
