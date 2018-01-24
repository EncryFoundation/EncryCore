package encry.settings

case class KeyManagerSettings(path: String,
                              lock : Boolean,
                              cypherAlgorithm : String,
                              hashAttempts : Int,
                              keyUnlockAttempts : Int)
