package io.github.jamespic.ethereum_tools.static_analysis.constraints

object ArbitraryOrdering {
  private implicit class IntOr(val x: Int) extends AnyVal {
    def ||(y: => Int) = if (x != y) x else y
  }
  def apply[T] = new ArbitraryOrdering[T]
}

class ArbitraryOrdering[T] extends Ordering[T] {
  import ArbitraryOrdering._
  override def compare(x: T, y: T): Int = {
    if (x == y) 0
    else {
      (x.hashCode compare y.hashCode) ||
      (x.toString compare y.toString) ||
      (System.identityHashCode(x) compare System.identityHashCode(y))
    }
  }
}
