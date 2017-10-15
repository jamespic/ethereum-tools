package io.github.jamespic.ethereum_tools.static_analysis

trait HashMemo {
  private[this] var hash = 0
  override def hashCode = {
    if (hash != 0) hash
    else {
      hash = super.hashCode
      hash
    }
  }
}
