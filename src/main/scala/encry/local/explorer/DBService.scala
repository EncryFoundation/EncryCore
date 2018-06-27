package encry.local.explorer

import doobie.free.connection.ConnectionIO
import doobie.util.fragment.Fragment

object DBService {

  def insert(table: String, fieldsString: String, dataString: String): ConnectionIO[Int] = {
    if (dataString.length > 2) {
      Fragment.const(s"INSERT INTO $table $fieldsString VALUES $dataString;").update.run
    } else {
      doobie.free.connection.unit.map(_ => 0)
    }
  }
}
