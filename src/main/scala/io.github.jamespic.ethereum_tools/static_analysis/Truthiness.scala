package io.github.jamespic.ethereum_tools.static_analysis

sealed trait Truthiness
case object Truthy extends Truthiness with HashMemo
case object Falsey extends Truthiness with HashMemo
case object Maybey extends Truthiness with HashMemo

object Truthiness {
  def truthiness(predicate: EVMData): Truthiness = predicate match {
    case Constant(x) => if (x == 0) Falsey else Truthy
    case Equals(AttackerControlledAddress, DefenderControlled()|Constant(_)) => Falsey
    case Equals(DefenderControlled()|Constant(_), AttackerControlledAddress) => Falsey
    case Equals(DefenderControlled(), Constant(_)) => Falsey
    case Equals(Constant(_), DefenderControlled()) => Falsey
    case Equals(x: Hash, y: Hash) =>
      if (x.data.length != y.data.length) Falsey
      else truthiness(
        ((True: EVMData) /: (x.data zip y.data)){
          case (oldExpr, (x1, y1)) => AndExpr(oldExpr, Equals(x1, y1))
        })
    case Not(x) => truthiness(x) match {
      case Truthy => Falsey
      case Falsey => Truthy
      case Maybey => Maybey
    }
    case AndExpr(a: Predicate, b: Predicate) => (truthiness(a), truthiness(b)) match {
      case (Falsey, _)|(_, Falsey) => Falsey
      case (Maybey, Truthy|Maybey)|(Truthy|Maybey, Maybey) => Maybey
      case (Truthy, Truthy) => Truthy
    }
    case OrExpr(a: Predicate, b: Predicate) => (truthiness(a), truthiness(b)) match {
      case (Truthy, _)|(_, Truthy) => Truthy
      case (Maybey, Falsey|Maybey)|(Falsey|Maybey, Maybey) => Maybey
      case (Falsey, Falsey) => Falsey
    }
    case _ => Maybey
  }
}

