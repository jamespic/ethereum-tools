package io.github.jamespic.ethereum_tools.static_analysis

import io.github.jamespic.ethereum_tools.static_analysis.constraints._

object EVMConstraints extends Ander[EVMConstraints] {
  override def and(a: EVMConstraints, b: EVMConstraints): Option[EVMConstraints] = {
    val newLinearOpt =  a.linearConstraints & b.linearConstraints
    // Check if either any elements in a are negations of elements of b
    val otherConstraintsCompatible = !a.otherConstraints.map(!_).exists(b.otherConstraints)
    for (newLinear <- newLinearOpt if otherConstraintsCompatible) yield {
      EVMConstraints(newLinear, a.otherConstraints ++ b.otherConstraints)
    }
  }
  implicit def ander = this
}

case class EVMConstraints(linearConstraints: NotEqualsConstraintWrapper[AttackerControlled]
                          = NotEqualsConstraintWrapper[AttackerControlled](),
                          otherConstraints: Set[Predicate] = Set.empty) extends HashMemo {
  def implies(predicate: EVMData): When[EVMConstraints] = predicate match {
    case Constant(n) => if (n == 0) Never else Always
    case Equals(AttackerControlledAddress, DefenderControlled()|Constant(_)) => Never
    case Equals(DefenderControlled()|Constant(_), AttackerControlledAddress) => Never
    case Equals(DefenderControlled(), Constant(_)) => Never
    case Equals(Constant(_), DefenderControlled()) => Never
    case Equals(x: Hash, y: Hash) =>
      if (x.data.length != y.data.length) Never
      else
        ((Always: When[EVMConstraints]) /: (x.data zip y.data)){
          case (oldExpr, (x1, y1)) => oldExpr & implies(Equals(x1, y1))
        }
    case Not(x) => !implies(x)
    case AndExpr(a: Predicate, b: Predicate) => implies(a) & implies(b)
    case OrExpr(a: Predicate, b: Predicate) => implies(a) | implies(b)
    case LinearNonEquality(clause, constant) =>
      // FIXME: You should get a performance boost from replacing this with a single solver
      linearConstraints.impliesNot(clause, constant).map {
         newConstraint => copy(linearConstraints = newConstraint)
      }
    case LinearInequality(clause, range) =>
      linearConstraints.implies(clause, range) map (newConstraint => copy(linearConstraints = newConstraint))
    // FIXME: Do something better with otherConstraints
    case x: Predicate if otherConstraints contains x => Always
    case x: Predicate if otherConstraints contains !x => Never
    case x: Predicate =>
      Sometimes(
        Set(this.copy(otherConstraints = otherConstraints + x)),
        Set(this.copy(otherConstraints = otherConstraints + !x))
      )
    case x =>
      Sometimes(
        Set(this.copy(otherConstraints = otherConstraints + !(x === 0))),
        Set(this.copy(otherConstraints = otherConstraints + (x === 0)))
      )
  }

  object LinearClausePlusConstant {
    def unapply(x: EVMData): Option[(LinearClause[AttackerControlled], Rational)] = x match {
      case AddExpr(LinearClausePlusConstant(clause1, const1), LinearClausePlusConstant(clause2, const2)) =>
        Some((clause1 + clause2, const1 + const2))
      case SubExpr(LinearClausePlusConstant(clause1, const1), LinearClausePlusConstant(clause2, const2)) =>
        Some((clause1 - clause2, const1 - const2))
      case MulExpr(LinearClausePlusConstant(clause, const), Constant(n)) =>
        Some((clause * n, const * n))
      case MulExpr(Constant(n), LinearClausePlusConstant(clause, const)) =>
        Some((clause * n, const * n))
      case DivExpr(LinearClausePlusConstant(clause, const), Constant(n)) =>
        Some((clause / n, const / n))
      case Constant(n) => Some(LinearClause[AttackerControlled](), n)
      case x: AttackerControlled => Some(LinearClause[AttackerControlled](x -> Rational.One), 0)
      case _ => None
    }
  }

  object LinearInequality {
    def unapply(x: EVMData): Option[(LinearClause[AttackerControlled], Range)] = normaliseNots(x) match {
      case LessThan(LinearClausePlusConstant(clause1, const1), LinearClausePlusConstant(clause2, const2)) =>
        Some((clause1 - clause2, Range(NoBound, OpenBound(const2 - const1))))
      case LessOrEqual(LinearClausePlusConstant(clause1, const1), LinearClausePlusConstant(clause2, const2)) =>
        Some((clause1 - clause2, Range(NoBound, ClosedBound(const2 - const1))))
      case GreaterThan(LinearClausePlusConstant(clause1, const1), LinearClausePlusConstant(clause2, const2)) =>
        Some((clause1 - clause2, Range(OpenBound(const2 - const1), NoBound)))
      case GreaterOrEqual(LinearClausePlusConstant(clause1, const1), LinearClausePlusConstant(clause2, const2)) =>
        Some((clause1 - clause2, Range(ClosedBound(const2 - const1), NoBound)))
      case Equals(LinearClausePlusConstant(clause1, const1), LinearClausePlusConstant(clause2, const2)) =>
        Some((clause1 - clause2, Range(ClosedBound(const2 - const1), ClosedBound(const2 - const1))))
      case _ => None
    }
    def normaliseNots(x: EVMData): EVMData = x match {
      case Not(LessThan(a, b)) => GreaterOrEqual(a, b)
      case Not(GreaterThan(a, b)) => LessOrEqual(a, b)
      case Not(LessOrEqual(a, b)) => GreaterThan(a, b)
      case Not(GreaterOrEqual(a, b)) => LessThan(a, b)
      case Not(Not(a)) => normaliseNots(a)
      case _ => x
    }
  }

  object LinearNonEquality {
    def unapply(x: EVMData): Option[(LinearClause[AttackerControlled], Rational)] = x match {
      case Not(Equals(LinearClausePlusConstant(clause1, const1), LinearClausePlusConstant(clause2, const2))) =>
        Some((clause1 - clause2, const2 - const1))
      case _ => None
    }
  }

  override def toString = linearConstraints.toString + "\n" + otherConstraints.mkString("\n")
}
