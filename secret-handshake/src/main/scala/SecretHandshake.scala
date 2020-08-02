object SecretHandshake {
  val Signals = Map(1 -> "wink", 2 -> "double blink",
                    4 -> "close your eyes", 8 -> "jump")

  def commands(n: Int)  = {
    val cmds = for ((k, v) <- Signals; if ((n & k) > 0)) yield v
    if ((n & 16) > 0) cmds.toList.reverse else cmds
  }
}
