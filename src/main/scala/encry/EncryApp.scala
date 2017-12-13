package encry

import com.google.common.primitives._
import encry.modifiers.history.block.header.EncryBlockHeader
import scorex.crypto.hash.Digest32
import scorex.core.block.Block._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.app.Application
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base16

//class EncryApp(args: Seq[String]) extends Application {
//  override type P = PublicKey25519Proposition
//  override type TX = this.type
//  override type PMOD = this.type
//  override type NVHT = this.type
//}

object EncryApp extends App {
//  new EncryApp(args).run()
  val block = new EncryBlockHeader(
    99.toByte, ModifierId @@ Longs.toByteArray(999L), Digest32 @@ Array[Byte](32), 9999L, 0, targetedDiff = 4)

  println("Block Hash > " + Base16.encode(block.powHash))
  println("     Nonce > " + block.nonce)

  def forceStopApplication(code: Int = 1): Unit =
    new Thread(() => System.exit(code), "encry-shutdown-thread").start()
}
