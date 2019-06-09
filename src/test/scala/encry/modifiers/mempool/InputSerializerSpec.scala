package encry.modifiers.mempool

import org.encryfoundation.common.modifiers.mempool.transaction._
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.{Ast, Types}
import org.encryfoundation.prismlang.core.wrapped.BoxedValue._
import org.scalatest.{Matchers, PropSpec}
import scorex.utils.Random
import scorex.crypto.signatures.PublicKey

class InputSerializerSpec extends PropSpec with Matchers {

  private val boxId: ADKey = ADKey @@ Random.randomBytes()

  private val pubKey: PublicKey = PublicKey @@ Random.randomBytes()

  private val proofList: List[Proof] = List(
    Proof(IntValue(1000), Some("IntValue")),
    Proof(ByteValue(7), Some("ByteValue")),
    Proof(BoolValue(true), Some("BoolValue")),
    Proof(StringValue("somerandomstringwithstrangesymbols!@#$%^&*()\n_+~/"), Some("StringValue")),
    Proof(ByteCollectionValue(Random.randomBytes().toList), Some("ByteCollectionValue")),
    Proof(Signature25519Value(Random.randomBytes().toList), Some("Signature25519Value")),
    Proof(MultiSignatureValue((1 to 5).map(_ => Random.randomBytes().toList).toList), Some("MultiSignatureValue"))
  )

  private val compiledContract: CompiledContract =
    CompiledContract(
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
          List(Expr.IntConst(100L))
        ),
        Expr.True,
        Expr.False,
        Types.PBoolean
      )
    )



  property("Serialize and parse input with compiled contract and no proofs") {
    val input = Input(boxId, Left(compiledContract), List.empty)

    val bytes = input.bytes

    val parsed = InputSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    val parsedInput = parsed.get
    parsedInput.boxId.sameElements(input.boxId) shouldBe true
    parsedInput.proofs shouldEqual List.empty
    parsedInput.contract shouldEqual Left(compiledContract)
  }

  property("Serialize and parse input with compiled contract and with proofs") {
    val input = Input(boxId, Left(compiledContract), proofList)

    val bytes = input.bytes

    val parsed = InputSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    val parsedInput = parsed.get
    parsedInput.boxId.sameElements(input.boxId) shouldBe true
    parsedInput.proofs shouldEqual proofList
    parsedInput.contract shouldEqual Left(compiledContract)
  }

  property("Serialize and parse input with HeightLockedContract and no proofs") {
    val input = Input(boxId, Right(HeightLockedContract(10)), List.empty)

    val bytes = input.bytes

    val parsed = InputSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    val parsedInput = parsed.get
    parsedInput.boxId.sameElements(input.boxId) shouldBe true
    parsedInput.proofs shouldEqual List.empty
    parsedInput.contract shouldEqual Right(HeightLockedContract(10))
  }

  property("Serialize and parse input with HeightLockedContract and with proofs") {
    val input = Input(boxId, Right(HeightLockedContract(10)), proofList)

    val bytes = input.bytes

    val parsed = InputSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    val parsedInput = parsed.get
    parsedInput.boxId.sameElements(input.boxId) shouldBe true
    parsedInput.proofs shouldEqual proofList
    parsedInput.contract shouldEqual Right(HeightLockedContract(10))
  }

  property("Serialize and parse input with OpenContract and no proofs") {
    val input = Input(boxId, Right(OpenContract), List.empty)

    val bytes = input.bytes

    val parsed = InputSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    val parsedInput = parsed.get
    parsedInput.boxId.sameElements(input.boxId) shouldBe true
    parsedInput.proofs shouldEqual List.empty
    parsedInput.contract shouldEqual Right(OpenContract)
  }

  property("Serialize and parse input with OpenContract and with proofs") {
    val input = Input(boxId, Right(OpenContract), proofList)

    val bytes = input.bytes

    val parsed = InputSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    val parsedInput = parsed.get
    parsedInput.boxId.sameElements(input.boxId) shouldBe true
    parsedInput.proofs shouldEqual proofList
    parsedInput.contract shouldEqual Right(OpenContract)
  }

  property("Serialize and parse input with PubKeyLockedContract and no proofs") {
    val input = Input(boxId, Right(PubKeyLockedContract(pubKey)), List.empty)

    val bytes = input.bytes

    val parsed = InputSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    val parsedInput = parsed.get
    parsedInput.boxId.sameElements(input.boxId) shouldBe true
    parsedInput.proofs shouldEqual List.empty
    parsedInput.contract.toSeq.collect {
      case contr: PubKeyLockedContract => contr.pubKey.sameElements(pubKey) shouldEqual true
      case _ => fail
    }
  }

  property("Serialize and parse input with PubKeyLockedContract and with proofs") {
    val input = Input(boxId, Right(PubKeyLockedContract(pubKey)), proofList)

    val bytes = input.bytes

    val parsed = InputSerializer.parseBytes(bytes)
    parsed.isSuccess shouldBe true
    val parsedInput = parsed.get
    parsedInput.boxId.sameElements(input.boxId) shouldBe true
    parsedInput.proofs shouldEqual proofList
    parsedInput.contract.toSeq.collect {
      case contr: PubKeyLockedContract => contr.pubKey.sameElements(pubKey) shouldEqual true
      case _ => fail
    }
  }

}
