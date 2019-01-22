package encry.storage.levelDb.versionalLevelDB

import encry.utils.CoreTaggedTypes.ModifierId
import org.encryfoundation.common.Algos

case class Version[D <: Diff](modifierId: ModifierId,
                              diffs: Seq[D]) {

  def printTree: String = s"Version: (${Algos.encode(modifierId)})"
}

object Version {

  def apply[D <: Diff](modifierId: ModifierId,
                       diffs: Seq[D]): Version[D] =
    new Version[D](modifierId, diffs)

  def empty[D <: Diff]: Version[D] = Version(ModifierId @@ Array.emptyByteArray, Seq.empty[D])
}
