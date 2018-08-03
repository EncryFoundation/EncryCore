package encry.modifiers.state.box

import encry.Address
import encry.modifiers.mempool.{EncryAddress, Pay2ContractHashAddress, Pay2PubKeyAddress, Proof}
import encry.modifiers.mempool.regcontract.{HeightLockedContract, OpenContract, PubKeyLockedContract}
import encry.modifiers.serialization.Serializer
import encry.settings.{Algos, Constants}
import encry.view.history.Height
import encry.view.state.Proposition
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import org.encryfoundation.prismlang.core.wrapped.PValue
import org.encryfoundation.prismlang.evaluator.Evaluator
import scorex.crypto.signatures.PublicKey
import scala.util.{Failure, Success, Try}

case class EncryProposition(contractHash: ContractHash) extends Proposition {

  override type M = EncryProposition

  override def serializer: Serializer[EncryProposition] = EncryPropositionSerializer

  def canUnlock(ctx: Context, contract: CompiledContract, proofs: Seq[Proof]): Boolean =
    if (sameHash(contractHash, contract.hash)) {
      val env: List[(Option[String], PValue)] =
        if (contract.args.isEmpty) List.empty
        else List((None, ctx.transaction.asVal), (None, ctx.state.asVal), (None, ctx.box.asVal)) ++
          proofs.map(proof => (proof.tagOpt, proof.value))
      val args: List[(String, PValue)] = contract.args.map { case (name, tpe) =>
        env.find(_._1.contains(name))
          .orElse(env.find(e => e._2.tpe == tpe || tpe.isSubtypeOf(e._2.tpe)))
          .map(elt => name -> elt._2)
          .getOrElse(throw new Exception("Not enough arguments for contact")) }
      Evaluator.initializedWith(args).eval[Boolean](contract.script)
    } else false

  def sameHash(h1: Array[Byte], h2: Array[Byte]): Boolean = ByteArrayWrapper(h1) == ByteArrayWrapper(h2)
}

object EncryProposition {

  case object UnlockFailedException extends Exception("Unlock failed")

  implicit val jsonEncoder: Encoder[EncryProposition] = (p: EncryProposition) => Map(
    "contractHash" -> Algos.encode(p.contractHash).asJson
  ).asJson

  def open: EncryProposition = EncryProposition(OpenContract.contract.hash)
  def heightLocked(height: Height): EncryProposition = EncryProposition(HeightLockedContract(height).contract.hash)
  def pubKeyLocked(pubKey: PublicKey): EncryProposition = EncryProposition(PubKeyLockedContract(pubKey).contract.hash)
  def addressLocked(address: Address): EncryProposition = EncryAddress.resolveAddress(address).map {
    case p2pk: Pay2PubKeyAddress => EncryProposition(PubKeyLockedContract(p2pk.pubKey).contract.hash)
    case p2sh: Pay2ContractHashAddress => EncryProposition(p2sh.contractHash)
  }.getOrElse(throw EncryAddress.InvalidAddressException)
}

object EncryPropositionSerializer extends Serializer[EncryProposition] {

  override def toBytes(obj: EncryProposition): Array[Byte] = obj.contractHash

  override def parseBytes(bytes: Array[Byte]): Try[EncryProposition] =
    if (bytes.lengthCompare(Constants.DigestLength) == 0) Success(EncryProposition(bytes))
    else Failure(new Exception("Invalid contract hash length"))
}
