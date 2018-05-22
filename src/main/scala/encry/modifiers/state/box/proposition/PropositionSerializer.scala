package encry.modifiers.state.box.proposition

import scorex.core.serialization.Serializer

import scala.util.{Failure, Try}

object PropositionSerializer extends Serializer[EncryProposition] {

  override def toBytes(obj: EncryProposition): Array[Byte] = obj match {
    case op: OpenProposition.type =>
      OpenProposition.typeId +: OpenPropositionSerializer.toBytes(op)
    case ap: AccountProposition =>
      AccountProposition.TypeId +: AccountPropositionSerializer.toBytes(ap)
    case hp: HeightProposition =>
      HeightProposition.TypeId +: HeightPropositionSerializer.toBytes(hp)
    case cp: ContractProposition =>
      ContractProposition.TypeId +: ContractPropositionSerializer.toBytes(cp)
    case m => throw new Exception(s"Serialization of unknown proposition type: $m")
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryProposition] = Try(bytes.head).flatMap {
    case OpenProposition.`typeId` => OpenPropositionSerializer.parseBytes(bytes.tail)
    case AccountProposition.`TypeId` => AccountPropositionSerializer.parseBytes(bytes.tail)
    case HeightProposition.`TypeId` => HeightPropositionSerializer.parseBytes(bytes.tail)
    case ContractProposition.`TypeId` => ContractPropositionSerializer.parseBytes(bytes.tail)
    case t => Failure(new Exception(s"Got unknown typeId: $t"))
  }
}
