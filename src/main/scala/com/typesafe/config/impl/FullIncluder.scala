package com.typesafe.config.impl

import com.typesafe.config.ConfigIncluder
import com.typesafe.config.ConfigIncluderClasspath
import com.typesafe.config.ConfigIncluderFile
import com.typesafe.config.ConfigIncluderURL
//remove if not needed
import scala.collection.JavaConversions._

trait FullIncluder extends ConfigIncluder with ConfigIncluderFile with ConfigIncluderURL with ConfigIncluderClasspath
