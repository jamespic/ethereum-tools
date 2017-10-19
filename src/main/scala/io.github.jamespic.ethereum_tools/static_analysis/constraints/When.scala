package io.github.jamespic.ethereum_tools.static_analysis.constraints

sealed trait When[+T] {
  def unary_! : When[T]
  def &[U >: T](that: When[U])(implicit ander: Ander[U]): When[U]
  def |[U >: T](that: When[U])(implicit ander: Ander[U]): When[U]
  def map[U](f: T => U): When[U]
}
case object Always extends When[Nothing] {
  override def unary_! = Never
  override def &[U >: Nothing](that: When[U])(implicit ander: Ander[U]): When[U] = that
  override def |[U >: Nothing](that: When[U])(implicit ander: Ander[U]): When[U] = Always
  override def map[U](f: Nothing => U) = Always
}
case object Never extends When[Nothing] {
  override def unary_! = Always
  override def &[U >: Nothing](that: When[U])(implicit ander: Ander[U]): When[U] = Never
  override def |[U >: Nothing](that: When[U])(implicit ander: Ander[U]): When[U] = that
  override def map[U](f: Nothing => U) = Never
}
case class Sometimes[+T](yesWhen: Set[_ <: T], noWhen: Set[_ <: T]) extends When[T] {
  override def unary_! = Sometimes(noWhen, yesWhen)
  override def &[U >: T](that: When[U])(implicit ander: Ander[U]): When[U] = that match {
    case Never => Never
    case Always => this
    case Sometimes(alsoYesWhen, alsoNoWhen) =>
      val newYesWhen = for (a <-yesWhen; b <- alsoYesWhen; result <- ander.and(a, b)) yield result
      if (newYesWhen.nonEmpty) Sometimes(newYesWhen, noWhen.toSet[U] ++ alsoNoWhen)
      else Never
  }
  override def |[U >: T](that: When[U])(implicit ander: Ander[U]): When[U] = that match {
    case Never => this
    case Always => Always
    case Sometimes(alsoYesWhen, alsoNoWhen) =>
      val newNoWhen = for (a <-noWhen; b <- alsoNoWhen; result <- ander.and(a, b)) yield result
      if (newNoWhen.nonEmpty) Sometimes(yesWhen.toSet[U] ++ alsoYesWhen, newNoWhen)
      else Always
  }
  override def map[U](f: T => U) = Sometimes(yesWhen map f, noWhen map f)
}

trait Ander[T] {
  def and(a: T, b: T): Option[T]
}