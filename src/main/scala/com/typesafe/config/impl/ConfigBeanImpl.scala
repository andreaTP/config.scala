package com.typesafe.config.impl

import java.beans.BeanInfo
import java.beans.IntrospectionException
import java.beans.Introspector
import java.beans.PropertyDescriptor
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Type
import java.util.ArrayList
import java.util.HashMap
import java.util.List
import java.util.Map
import java.time.Duration
import com.typesafe.config.Config
import com.typesafe.config.ConfigObject
import com.typesafe.config.ConfigList
import com.typesafe.config.ConfigException
import com.typesafe.config.ConfigMemorySize
import com.typesafe.config.ConfigValue
import com.typesafe.config.ConfigValueType
//remove if not needed
import scala.collection.JavaConversions._

object ConfigBeanImpl {

  /**
   * This is public ONLY for use by the "config" package, DO NOT USE this ABI
   * may change.
   * @param <T> type of the bean
   * @param config config to use
   * @param clazz class of the bean
   * @return the bean instance
   */
  def createInternal[T](config: Config, clazz: Class[T]): T = {
    if (config.asInstanceOf[SimpleConfig].root().resolveStatus() != 
      ResolveStatus.RESOLVED) throw new ConfigException.NotResolved("need to Config#resolve() a config before using it to initialize a bean, see the API docs for Config#resolve()")
    val configProps = new HashMap[String, AbstractConfigValue]()
    val originalNames = new HashMap[String, String]()
    for ((key, value) <- config.root()) {
      val originalName = key
      val camelName = ConfigImplUtil.toCamelCase(originalName)
      if (originalNames.containsKey(camelName) && originalName != camelName) {
      } else {
        configProps.put(camelName, value.asInstanceOf[AbstractConfigValue])
        originalNames.put(camelName, originalName)
      }
    }
    var beanInfo: BeanInfo = null
    beanInfo = Introspector.getBeanInfo(clazz)
    val beanProps = new ArrayList[PropertyDescriptor]()
    for (beanProp <- beanInfo.getPropertyDescriptors) {
      if (beanProp.getReadMethod == null || beanProp.getWriteMethod == null) {
        //continue
      }
      beanProps.add(beanProp)
    }
    val problems = new ArrayList[ConfigException.ValidationProblem]()
    for (beanProp <- beanProps) {
      val setter = beanProp.getWriteMethod
      val parameterClass = setter.getParameterTypes()(0)
      val expectedType = getValueTypeOrNull(parameterClass)
      if (expectedType != null) {
        var name = originalNames.get(beanProp.getName)
        if (name == null) name = beanProp.getName
        val path = Path.newKey(name)
        val configValue = configProps.get(beanProp.getName)
        if (configValue != null) {
          SimpleConfig.checkValid(path, expectedType, configValue, problems)
        } else {
          SimpleConfig.addMissing(problems, expectedType, path, config.origin())
        }
      }
    }
    if (!problems.isEmpty) {
      throw new ConfigException.ValidationFailed(problems)
    }
    val bean = clazz.newInstance()
    for (beanProp <- beanProps) {
      val setter = beanProp.getWriteMethod
      val parameterType = setter.getGenericParameterTypes()(0)
      val parameterClass = setter.getParameterTypes()(0)
      val unwrapped = getValue(clazz, parameterType, parameterClass, config, originalNames.get(beanProp.getName))
      setter.invoke(bean, unwrapped)
    }
    bean
  }

  private def getValue(beanClass: Class[_], 
      parameterType: Type, 
      parameterClass: Class[_], 
      config: Config, 
      configPropName: String): AnyRef = {
    if (parameterClass == classOf[Boolean] || parameterClass == classOf[Boolean]) {
      config.getBoolean(configPropName)
    } else if (parameterClass == classOf[Integer] || parameterClass == classOf[Int]) {
      config.getInt(configPropName)
    } else if (parameterClass == classOf[Double] || parameterClass == classOf[Double]) {
      config.getDouble(configPropName)
    } else if (parameterClass == classOf[Long] || parameterClass == classOf[Long]) {
      config.getLong(configPropName)
    } else if (parameterClass == classOf[String]) {
      config.getString(configPropName)
    } else if (parameterClass == classOf[Duration]) {
      config.getDuration(configPropName)
    } else if (parameterClass == classOf[ConfigMemorySize]) {
      config.getMemorySize(configPropName)
    } else if (parameterClass == classOf[AnyRef]) {
      config.getAnyRef(configPropName)
    } else if (parameterClass == classOf[List[_]]) {
      getListValue(beanClass, parameterType, parameterClass, config, configPropName)
    } else if (parameterClass == classOf[Map[_,_]]) {
      val typeArgs = parameterType.asInstanceOf[ParameterizedType].getActualTypeArguments
      if (typeArgs(0) != classOf[String] || typeArgs(1) != classOf[AnyRef]) {
        throw new ConfigException.BadBean("Bean property '" + configPropName + "' of class " + beanClass.getName + 
          " has unsupported Map<" + 
          typeArgs(0) + 
          "," + 
          typeArgs(1) + 
          ">, only Map<String,Object> is supported right now")
      }
      config.getObject(configPropName).unwrapped()
    } else if (parameterClass == classOf[Config]) {
      config.getConfig(configPropName)
    } else if (parameterClass == classOf[ConfigObject]) {
      config.getObject(configPropName)
    } else if (parameterClass == classOf[ConfigValue]) {
      config.getValue(configPropName)
    } else if (parameterClass == classOf[ConfigList]) {
      config.getList(configPropName)
    } else if (hasAtLeastOneBeanProperty(parameterClass)) {
      createInternal(config.getConfig(configPropName), parameterClass)
    } else {
      throw new ConfigException.BadBean("Bean property " + configPropName + " of class " + beanClass.getName + 
        " has unsupported type " + 
        parameterType)
    }
  }

  private def getListValue(beanClass: Class[_], 
      parameterType: Type, 
      parameterClass: Class[_], 
      config: Config, 
      configPropName: String): AnyRef = {
    val elementType = parameterType.asInstanceOf[ParameterizedType].getActualTypeArguments()(0)
    if (elementType == classOf[Boolean]) {
      config.getBooleanList(configPropName)
    } else if (elementType == classOf[Integer]) {
      config.getIntList(configPropName)
    } else if (elementType == classOf[Double]) {
      config.getDoubleList(configPropName)
    } else if (elementType == classOf[Long]) {
      config.getLongList(configPropName)
    } else if (elementType == classOf[String]) {
      config.getStringList(configPropName)
    } else if (elementType == classOf[Duration]) {
      config.getDurationList(configPropName)
    } else if (elementType == classOf[ConfigMemorySize]) {
      config.getMemorySizeList(configPropName)
    } else if (elementType == classOf[AnyRef]) {
      config.getAnyRefList(configPropName)
    } else if (elementType == classOf[Config]) {
      config.getConfigList(configPropName)
    } else if (elementType == classOf[ConfigObject]) {
      config.getObjectList(configPropName)
    } else if (elementType == classOf[ConfigValue]) {
      config.getList(configPropName)
    } else if (hasAtLeastOneBeanProperty(elementType.asInstanceOf[Class[_]])) {
      val beanList = new ArrayList[Any]()
      val configList = config.getConfigList(configPropName)
      for (listMember <- configList) {
        beanList.add(createInternal(listMember, elementType.asInstanceOf[Class[_]]))
      }
      beanList
    } else {
      throw new ConfigException.BadBean("Bean property '" + configPropName + "' of class " + beanClass.getName + 
        " has unsupported list element type " + 
        elementType)
    }
  }

  private def getValueTypeOrNull(parameterClass: Class[_]): ConfigValueType = {
    if (parameterClass == classOf[Boolean] || parameterClass == classOf[Boolean]) {
      ConfigValueType.BOOLEAN
    } else if (parameterClass == classOf[Integer] || parameterClass == classOf[Int]) {
      ConfigValueType.NUMBER
    } else if (parameterClass == classOf[Double] || parameterClass == classOf[Double]) {
      ConfigValueType.NUMBER
    } else if (parameterClass == classOf[Long] || parameterClass == classOf[Long]) {
      ConfigValueType.NUMBER
    } else if (parameterClass == classOf[String]) {
      ConfigValueType.STRING
    } else if (parameterClass == classOf[Duration]) {
      null
    } else if (parameterClass == classOf[ConfigMemorySize]) {
      null
    } else if (parameterClass == classOf[List[_]]) {
      ConfigValueType.LIST
    } else if (parameterClass == classOf[Map[_,_]]) {
      ConfigValueType.OBJECT
    } else if (parameterClass == classOf[Config]) {
      ConfigValueType.OBJECT
    } else if (parameterClass == classOf[ConfigObject]) {
      ConfigValueType.OBJECT
    } else if (parameterClass == classOf[ConfigList]) {
      ConfigValueType.LIST
    } else {
      null
    }
  }

  private def hasAtLeastOneBeanProperty(clazz: Class[_]): Boolean = {
    var beanInfo: BeanInfo = null
    try {
      beanInfo = Introspector.getBeanInfo(clazz)
    } catch {
      case e: IntrospectionException => return false
    }
    for (beanProp <- beanInfo.getPropertyDescriptors if beanProp.getReadMethod != null && beanProp.getWriteMethod != null) {
      return true
    }
    false
  }
}
