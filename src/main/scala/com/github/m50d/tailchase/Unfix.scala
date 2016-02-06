package com.github.m50d.tailchase

import scalaz.Functor
import scalaz.Leibniz
import scalaz.Leibniz.===
import shapeless._

sealed trait UnfixHelper[F, M] {
  type O[A]

  implicit val unfix: M === O[F]
  implicit val fix: O[F] === M

  implicit val functor: Functor[O]
}

object UnfixHelper {
  sealed trait UnfixHelper_[F] {
    final type O[M] = UnfixHelper[F, M]
  }
  
  private[this] def typeClass[F] =
    new ProductTypeClass[UnfixHelper_[F]#O] {
      override def emptyProduct = new UnfixHelper[F, HNil] {
        override type O[A] = HNil
        override implicit val unfix = Leibniz.refl
        override implicit val fix = Leibniz.refl
        override implicit val functor = new Functor[O] {
          override def map[A, B](fa: O[A])(f: A => B) = HNil
        }
      }
      override def product[H, T <: HList](ch: UnfixHelper[F, H], ct: UnfixHelper[F, T]{ type O[A] <: HList }) =
        new UnfixHelper[F, H :: T] {
          override type O[A] = ch.O[A] :: ct.O[A]
          override implicit val unfix = {
            Leibniz.lift2(ch.unfix, ct.unfix)
          }
          override implicit val fix = {
            Leibniz.lift2(ch.fix, ct.fix)
          }
      }
    
  }
  
  implicit def deriveHNil[F]: UnfixHelper[F, HNil] = typeClass.emptyProduct

  implicit def deriveHCons[F, H, T <: HList](implicit ch: Lazy[UnfixHelper[F, H]], ct: Lazy[UnfixHelper[F, T]]): UnfixHelper[F, H :: T] =
    typeClass.product(ch.value, ct.value)

  implicit def deriveInstance[F, G, H](implicit gen: Generic.Aux[G, H], cg: Lazy[UnfixHelper[F, H]]): UnfixHelper[F, G] =
    typeClass.project(cg.value, gen.to _, gen.from _)
}

sealed trait Unfix[F] {
  type O[A]

  implicit val unfix: F === O[F]
  implicit val fix: O[F] === F

  implicit val functor: Functor[O]
}

object Unfix {}