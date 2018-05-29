package encry.settings

case class TestingSettings(transactionGeneration: Boolean,
                           defaultRecipientAddress: String,
                           limitPerEpoch: Int)
