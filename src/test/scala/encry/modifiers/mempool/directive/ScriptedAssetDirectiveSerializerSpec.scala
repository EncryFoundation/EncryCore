package encry.modifiers.mempool.directive

import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.{Ast, Types}
import org.scalatest.{Matchers, PropSpec}

class ScriptedAssetDirectiveSerializerSpec extends PropSpec with Matchers {

  property("toBytes/fromBytes") {

    val directive: ScriptedAssetDirective = ScriptedAssetDirective(
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
      ), 1000
    )

    val directiveSerialized = directive.bytes

    val directiveDeserialized = ScriptedAssetDirectiveSerializer.parseBytes(directiveSerialized)

    directiveDeserialized.isSuccess shouldBe true
  }
}
