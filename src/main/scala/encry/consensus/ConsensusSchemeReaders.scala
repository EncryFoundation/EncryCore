package encry.consensus

import com.typesafe.config.{Config, ConfigException}
import net.ceedubs.ficus.readers.ValueReader

trait ConsensusSchemeReaders {

  val readers: Seq[ConsensusSchemeReader[_ <: ConsensusScheme]] = Seq.empty

  implicit val powSchemeReader: ValueReader[ConsensusScheme] =  { (cfg, path) =>
    val schemeNameKey = s"$path.powType"
    val schemeName = cfg.getString(schemeNameKey)
    val schemeReader = readers.find(_.schemeName == schemeName)
      .getOrElse(throw new ConfigException.BadValue(schemeNameKey, schemeName))
    schemeReader.read(cfg, path)
  }

}

sealed trait ConsensusSchemeReader[T <: ConsensusScheme] {
  def schemeName: String
  def read(config: Config, path: String): T
}
