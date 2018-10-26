package encry.api.http.utils

import encry.api.http.utils
import io.circe.{Decoder, HCursor}

case class DataForTx(data: Seq[Array[Byte]], token: String)

object DataForTx {

  implicit val jsonDecoder: Decoder[DataForTx] = (c: HCursor) => {
    for {
      data <- c.downField("data").as[Seq[String]]
      token <- c.downField("token").as[String]
    } yield utils.DataForTx(
      data.map(_.getBytes),
      token
    )
  }
}
