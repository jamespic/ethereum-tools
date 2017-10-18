package io.github.jamespic.ethereum_tools.static_analysis.constraints

sealed trait When[+T]
case object Always extends When[Nothing]
case object Never extends When[Nothing]
case class Sometimes[+T](yesWhen: Set[_ <: T], noWhen: Set[_ <: T]) extends When[T]
