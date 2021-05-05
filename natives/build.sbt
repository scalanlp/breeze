Common.commonSettings

name := "breeze-natives"

// see https://github.com/typesafehub/scalalogging/issues/23
testOptions in Test += Tests.Setup(
  classLoader =>
    classLoader
      .loadClass("org.slf4j.LoggerFactory")
      .getMethod("getLogger", classLoader.loadClass("java.lang.String"))
      .invoke(null, "ROOT"))
