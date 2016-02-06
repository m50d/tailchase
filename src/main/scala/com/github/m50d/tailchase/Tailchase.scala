package com.github.m50d.tailchase

object Tailchase {
  implicit class Tailchaseable[F](val inner: F)(implicit u: UnfixHelper[F, Any, F] with NonTrivial) {
    def catap[P <: Poly](p: P)
  }
}