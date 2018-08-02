package encry.modifiers

import encry.modifiers.mempool._
import encry.modifiers.state.Keys
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.utils.{EncryGenerator, TestHelper}
import encry.view.history.Height
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.{Ast, Types}
import scorex.crypto.authds.ADKey
import scorex.utils.Random
import scala.util.{Random => Scarand}

trait InstanceFactory extends Keys with EncryGenerator {

  private val genHelper = TestHelper

  lazy val fakeTransaction: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address.address),
      genHelper.genAssetBox(publicKey.address.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address.address, 12345678L)
  }

  lazy val paymentTransactionValid: EncryTransaction = {
    val fee: Amount = genHelper.Props.txFee
    val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genHelper.genAssetBox(publicKey.address.address),
      genHelper.genAssetBox(publicKey.address.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address.address, genHelper.Props.txAmount)
  }

  def paymentTransactionDynamic: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = (0 to 5).map(_ => {
      AssetBox(
        EncryProposition.pubKeyLocked(secret.publicImage.pubKeyBytes),
        Scarand.nextLong(),
        999L
      )
    })

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, Scarand.nextLong(), useBoxes,
      publicKey.address.address, genHelper.Props.txAmount)
  }

  lazy val paymentTransactionInvalid: EncryTransaction = {
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, -100, timestamp, useBoxes,
      randomAddress, genHelper.Props.txAmount)
  }

  lazy val coinbaseTransaction: EncryTransaction = {
    TransactionFactory.coinbaseTransactionScratch(secret.publicImage, timestamp, 10L, 0, Height @@ 100)
  }

  lazy val AssetBoxI: AssetBox =
    AssetBox(
      EncryProposition.pubKeyLocked(secret.publicImage.pubKeyBytes),
      999L,
      100000L
    )

  lazy val OpenAssetBoxI: AssetBox =
    AssetBox(
      EncryProposition.open,
      999L,
      100000L
    )

  lazy val Contract: CompiledContract = CompiledContract(
    List("state" -> Types.EncryState),
    Expr.If(
      Expr.Compare(
        Expr.Attribute(
          Expr.Name(
            Ast.Ident("state"),
            Types.EncryState
          ),
          Ast.Ident("height"),
          Types.PInt
        ),
        List(Ast.CompOp.GtE),
        List(Expr.IntConst(1000L))
      ),
      Expr.True,
      Expr.False,
      Types.PBoolean
    )
  )

  lazy val UnsignedInput: Input = Input(ADKey @@ Random.randomBytes(), Left(Contract), List.empty)
}