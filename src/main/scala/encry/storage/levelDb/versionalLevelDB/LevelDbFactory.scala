package encry.storage.levelDb.versionalLevelDB

import com.typesafe.scalalogging.StrictLogging
import org.iq80.leveldb.DBFactory

import scala.util.Try

object LevelDbFactory extends StrictLogging {
  private val nativeFactory = "org.fusesource.leveldbjni.JniDBFactory"
  private val javaFactory   = "org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = {
    val pairs = for {
      loader      <- List(ClassLoader.getSystemClassLoader, this.getClass.getClassLoader).view
      factoryName <- List(nativeFactory, javaFactory)
      factory     <- Try(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory]).toOption
    } yield (factoryName, factory)

    val (fName, f) = pairs.headOption.getOrElse(throw new Exception(s"Could not load any of the factory classes: $nativeFactory, $javaFactory"))
    if (fName == javaFactory) logger.warn("Using the pure java LevelDB implementation which is still experimental")
    else logger.trace(s"Loaded $fName with $f")
    f
  }
}
