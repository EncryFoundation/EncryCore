package encry.settings

case class TestingSettings(transactionGeneration: Boolean,
                           minimalFee: Int,
                           amount: Int,
                           defaultRecipientAddress: String,
                           limitPerEpoch: Int)
