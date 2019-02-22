package encry.serialization.protobuf

import com.google.protobuf.ByteString
import scalapb.TypeMapper

object Utils {

//  implicit val optionMapper: (Option[_] => ByteString) => TypeMapper[ByteString, Option[_]] =
//    TypeMapper[ByteString, Option[_]] { bs =>
//      if (bs.isEmpty) Option.empty else Option(bs)
//    } { bs =>
//      if(bs.isEmpty) ByteString.EMPTY else ByteString.copyFrom(bs.)
//    }
//
//  implicit val tokenIdMapper = TypeMapper[]
}
