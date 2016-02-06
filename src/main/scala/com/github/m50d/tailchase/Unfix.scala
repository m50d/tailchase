package com.github.m50d.tailchase

import scalaz.Functor
import scalaz.Leibniz
import scalaz.Leibniz.===
import shapeless._

sealed trait UnfixHelper[F, +Bound, M <: Bound] {
  type O[A] <: Bound
  implicit val unfix: Leibniz[Nothing, Bound, M, O[F]]
  implicit val functor: Functor[O]
}

trait UnfixHelper1 {
  implicit def deriveHNil[F]: UnfixHelper[F, HNil, HNil] = new UnfixHelper[F, HNil, HNil] {
    override type O[A] = HNil
    override implicit val unfix = Leibniz.refl[HNil]
    override implicit val functor = new Functor[O] {
      override def map[A, B](fa: O[A])(f: A ⇒ B) = HNil
    }
  }

  implicit def deriveHCons[F, H, T <: HList](implicit ch: Lazy[UnfixHelper[F, Any, H]], ct: Lazy[UnfixHelper[F, HList, T]]): UnfixHelper[F, HList, H :: T] =
    new UnfixHelper[F, HList, H :: T] {
      override type O[A] = ch.value.O[A] :: ct.value.O[A]
      override implicit val unfix =
        Leibniz.lift2[Nothing, Nothing, Nothing, Any, HList, HList, ::, H, ch.value.O[F], T, ct.value.O[F]](ch.value.unfix, ct.value.unfix)
      override implicit val functor: Functor[O] = new Functor[O] {
        override def map[A, B](fa: O[A])(f: A => B) = ch.value.functor.map(fa.head)(f) :: ct.value.functor.map(fa.tail)(f)
      }
    }

//  implicit def deriveInstance[F, G, H <: HList](implicit gen: Generic.Aux[G, H], cg: Lazy[UnfixHelper[F, HList, H]]): UnfixHelper[F, Any, G] =
//    new UnfixHelper[F, Any, G] {
//      override type 
//    
//  }
}
object UnfixHelper extends UnfixHelper1 {
  implicit def unfix[F]: UnfixHelper[F, Any, F] = new UnfixHelper[F, Any, F] {
    override type O[A] = A
    override implicit val unfix = Leibniz.refl[F]
    override implicit val functor = new Functor[O] {
      override def map[A, B](fa: O[A])(f: A => B) = f(fa)
    }
  }
}

sealed trait Unfix[F] {
  type O[A]

  implicit val unfix: F === O[F]
  implicit val fix: O[F] === F

  implicit val functor: Functor[O]
}

object Unfix {}