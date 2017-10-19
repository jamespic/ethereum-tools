package io.github.jamespic.ethereum_tools.static_analysis

import scala.runtime.ScalaRunTime

trait HashMemo {this: Product =>
  private[this] var hash = 0
  override def hashCode = {
    if (hash != 0) hash
    else {
      hash = ScalaRunTime._hashCode(this)
      hash
    }
  }
}
