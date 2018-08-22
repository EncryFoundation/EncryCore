package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Longs
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.utils.ScorexEncoding

import scala.util.{Failure, Success, Try}

sealed trait Operation {
  val key: ADKey
}

case class Lookup(override val key: ADKey) extends Operation

trait Modification extends Operation with ScorexEncoding {
  val key: ADKey
  type OldValue = Option[ADValue]

  type NewValue = ADValue

  type UpdateFunction = OldValue => Try[Option[NewValue]]

  def updateFn: UpdateFunction
}

case class Insert(key: ADKey, value: ADValue) extends Modification {
  override def updateFn: UpdateFunction = {
    case None => Success(Some(value))
    case Some(_) => Failure(new Exception(s"Key ${encoder.encode(key)} already exists"))
  }: UpdateFunction

  override def toString: String = s"""Insert(\"${encoder.encode(key)}\",\"${encoder.encode(value)}\")"""
}

case class InsertOrUpdate(key: ADKey, value: ADValue) extends Modification {
  override def updateFn: UpdateFunction = (_ => Success(Some(value))): UpdateFunction

  override def toString: String = s"""InsertOrUpdate(\"${encoder.encode(key)}\",\"${encoder.encode(value)}\")"""
}


case class Remove(key: ADKey) extends Modification {
  override def updateFn: UpdateFunction = {
    case None => Failure(new Exception(s"Key ${encoder.encode(key)} does not exist"))
    case Some(_) => Success(None)
  }: UpdateFunction

  override def toString: String = s"""Remove(\"${encoder.encode(key)}\")"""
}

case class UpdateLongBy(key: ADKey, delta: Long) extends Modification {
  override def updateFn: UpdateFunction = {
    case m if delta == 0 => Success(m)
    case None if delta > 0 => Success(Some(ADValue @@ Longs.toByteArray(delta)))
    case None if delta < 0 => Failure(new Exception("Trying to decrease non-existing value"))
    case Some(oldV) =>
      val newVal = Math.addExact(Longs.fromByteArray(oldV), delta)
      if (newVal == 0) {
        Success(None)
      } else if (newVal > 0) {
        Success(Some(ADValue @@ Longs.toByteArray(newVal)))
      } else {
        Failure(new Exception("New value is negative"))
      }
  }: UpdateFunction
}