package com.github.m50d.tailchase

import shapeless.Poly
import cats.syntax.functor._

private[tailchase] object Tailchase {
  def cataImpl[U[_], F, A](mapper: U[A] => A)(f: F)(implicit u: UnfixHelper.Unfix[F, U]): A = {
    import u._
    mapper(functor.map(unfix(f)) {f => cataImpl(mapper)(f)})
  }
}

object syntax {
  implicit class TailchaseOps[F](val inner: F)(implicit u: UnfixHelper[F, Any, F] with NonTrivial) {
    def cata[A](f: u.O[A] ⇒ A): A = Tailchase.cataImpl(f)(inner)(u)
  }
}