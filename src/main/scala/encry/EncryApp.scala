package encry

import com.google.common.primitives._
import encry.modifiers.history.block.header.EncryBlockHeader
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.hash.{Digest32, Sha256}
import scorex.core.block.Block._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.app.Application
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.{Base16, Base58}
import java.io.File
import java.util.concurrent.TimeUnit.SECONDS

import encry.consensus.{Difficulty, PowLinearController}
import encry.crypto.Address
import encry.mining.PowMiner
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryPaymentTransaction
import encry.modifiers.state.box.{EncryPaymentBox, EncryPaymentBoxSerializer}
import encry.modifiers.state.box.body.PaymentBoxBody
import encry.modifiers.state.box.proposition.AddressProposition
import encry.settings.Algos
import encry.view.history.Height
import encry.view.state.UtxoState
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}


//class EncryApp(args: Seq[String]) extends Application {
//  override type P = PublicKey25519Proposition
//  override type TX = this.type
//  override type PMOD = this.type
//  override type NVHT = this.type
//}

object EncryApp extends App {
  //  new EncryApp(args).run()
  //-----

  val keyPair = Curve25519.createKeyPair(Base58.decode("Bars").get)
  val pubKey : PublicKey = keyPair._2
  val priKey : PrivateKey = keyPair._1
  val recipientProp = PublicKey25519Proposition(pubKey)
  val senderProp = PublicKey25519Proposition(pubKey)

  val block = new EncryBlockHeader(
    99.toByte, ModifierId @@ Longs.toByteArray(999L), Digest32 @@ Array[Byte](32), 9999L, 0, 16, Difficulty @@ BigInt(20), senderProp)

  println("Block Hash > " + Base16.encode(block.id))
  println("     Nonce > " + block.nonce)

  // Miner
//  var foundBlock: Option[EncryBlockHeader] = None
//  while (foundBlock.isEmpty) {
//    foundBlock = PowMiner.powIteration(
//      99.toByte, ModifierId @@ Longs.toByteArray(999L), Digest32 @@ Array[Byte](32), 16, Difficulty @@ BigInt(500), senderProp)
//  }
//
//  println("Found valid blok hash: " + Base16.encode(foundBlock.get.id))



  // Difficulty retargeting test

  val dir = new File(System.getProperty("user.dir"))

  println("Initializing UTXO state storage dir ... --> " + dir.getAbsoluteFile)

  val store = new LSMStore(dir, keepVersions = 20) // todo: magic number, move to settings

  //nb init



  //2 trx init

  // UTXO Test


//  val keyPair = Curve25519.createKeyPair(Base58.decode("Bars").get)
//  val pubKey : PublicKey = keyPair._2
//  val priKey : PrivateKey = keyPair._1
//  val recepientProp = PublicKey25519Proposition(pubKey)
//  val senderProp = PublicKey25519Proposition(pubKey)


  val nb1 = new EncryPaymentBox(new AddressProposition(Address @@ recipientProp.address),25L,PaymentBoxBody(12L))
  println("Addr is: " + new AddressProposition(Address @@ recipientProp.address).address)
  val nb2 = new EncryPaymentBox(new AddressProposition(Address @@ recipientProp.address),32L,PaymentBoxBody(24L))

  val InputNullTX1 = IndexedSeq(ADKey @@ EncryPaymentBox.idFromBox(nb1.proposition, nb1.nonce))

  //println(EncryPaymentBox.idFromBox(nb1.proposition,nb1.nonce))

  val InputNullTX2 = IndexedSeq(ADKey @@ EncryPaymentBox.idFromBox(nb2.proposition, nb2.nonce))

  val sigTX1 = Signature25519(Curve25519.sign(priKey,"firstTransaction".getBytes))
  val sigTX2 = Signature25519(Curve25519.sign(priKey,"secondTransaction".getBytes))

  val OutputNullTX1 = IndexedSeq((Address @@ recipientProp.address,12L))
  val OutputNullTX2 = IndexedSeq((Address @@ recipientProp.address,15L))
  val BaseTX1 = EncryPaymentTransaction(senderProp, 12L, 123L, sigTX1, InputNullTX1, OutputNullTX1)
  val BaseTX2 = EncryPaymentTransaction(senderProp, 28L, 124L, sigTX2, InputNullTX2, OutputNullTX2)



  BaseTX1.signature = Signature25519(Curve25519.sign(priKey,BaseTX1.messageToSign))
  BaseTX2.signature = Signature25519(Curve25519.sign(priKey,BaseTX2.messageToSign))

  //Block init

  val testPayload = new EncryBlockPayload(ModifierId @@ "ModId".getBytes(),IndexedSeq(BaseTX1,BaseTX2))
  val testHeader = EncryBlockHeader(99.toByte, ModifierId @@ Longs.toByteArray(999L), Digest32 @@ Array[Byte](32), 9999L, 0, 16, Difficulty @@ BigInt(20000), senderProp)
  val testBlock = new EncryBlock(testHeader,testPayload)

  //UTXO init

  //val nb1 = EncryPaymentBox(AddressProposition(senderProp.address),)

  val utxo = UtxoState.create(dir)
  val ver = new ByteArrayWrapper(1)
  val toRem = List()
  val toUp = List((new ByteArrayWrapper(EncryPaymentBox.idFromBox(nb1.proposition,nb1.nonce)),new ByteArrayWrapper(EncryPaymentBoxSerializer.toBytes(nb1))),
    (new ByteArrayWrapper(EncryPaymentBox.idFromBox(nb2.proposition,nb2.nonce)),new ByteArrayWrapper(EncryPaymentBoxSerializer.toBytes(nb2)))
  )
  utxo.store.update(ver,toRem,toUp)
  utxo.applyModifier(testBlock)


  def forceStopApplication(code: Int = 1): Unit =
    new Thread(() => System.exit(code), "encry-shutdown-thread").start()
}