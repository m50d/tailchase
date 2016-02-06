package com.github.m50d.tailchase

import cats.Functor
import shapeless._

/**
 * Ensure that we aren't merely "unfixing" in the trivial way Unfix[F, A] = F.
 * Arguably it would be more correct to treat HNil/CNil as nontrivial
 * in the sense that traversing them is likely to have the expected result 
 */
sealed trait NonTrivial

sealed trait UnfixHelper[F, +Bound, M] {
  type O[A] <: Bound
  implicit val functor: Functor[O]
  def unfix(m: M): O[F]
  def fix(of: O[F]): M
}

trait UnfixHelper2 {
  implicit def deriveA[F, A0] = new UnfixHelper[F, Any, A0] {
    override type O[A] = A0
    override implicit val functor = new Functor[O] {
      override def map[A, B](fa: O[A])(f: A ⇒ B) = fa
    }
    override def unfix(m: A0) = m
    override def fix(of: A0) = of
  }
}

trait UnfixHelper1 extends UnfixHelper2 {
  implicit def deriveHNil[F] = new UnfixHelper[F, HNil, HNil] {
    override type O[A] = HNil
    override implicit val functor = new Functor[O] {
      override def map[A, B](fa: O[A])(f: A ⇒ B) = HNil
    }
    override def unfix(m: HNil) = m
    override def fix(of: HNil) = of
  }

  implicit def deriveHCons[F, H, T <: HList](implicit ch: Lazy[UnfixHelper[F, Any, H]], ct: Lazy[UnfixHelper[F, HList, T]]) =
    new UnfixHelper[F, HList, H :: T] with NonTrivial {
      override type O[A] = ch.value.O[A] :: ct.value.O[A]
      override implicit val functor: Functor[O] = new Functor[O] {
        override def map[A, B](fa: O[A])(f: A ⇒ B) = ch.value.functor.map(fa.head)(f) :: ct.value.functor.map(fa.tail)(f)
      }
      override def unfix(m: H :: T) = ch.value.unfix(m.head) :: ct.value.unfix(m.tail)
      override def fix(of: O[F]) = ch.value.fix(of.head) :: ct.value.fix(of.tail)
    }

  implicit def deriveInstance[F, G, H <: HList](implicit gen: Generic.Aux[G, H], cg: Lazy[UnfixHelper[F, HList, H]]) =
    new UnfixHelper[F, HList, G] with NonTrivial {
      override type O[A] = cg.value.O[A]
      override implicit val functor = cg.value.functor
      override def unfix(m: G) = cg.value.unfix(gen.to(m))
      override def fix(of: O[F]) = gen.from(cg.value.fix(of))
    }
  implicit def deriveCNil[F] = new UnfixHelper[F, CNil, CNil] {
    override type O[A] = CNil
    override implicit val functor = new Functor[O] {
      override def map[A, B](fa: O[A])(f: A => B) = fa
    }
    override def unfix(m: CNil) = m
    override def fix(of: CNil) = of
  }

  implicit def deriveCCons[F, H, T <: Coproduct] (implicit ch: Lazy[UnfixHelper[F, Any, H]], ct: Lazy[UnfixHelper[F, Coproduct, T]]) =
    new UnfixHelper[F, Coproduct, H :+: T] {
      override type O[A] = ch.value.O[A] :+: ct.value.O[A]
      override implicit val functor = new Functor[O] {
        override def map[A, B](fa: O[A])(f: A => B) = fa match {
          case Inl(h) => Inl(ch.value.functor.map(h)(f))
          case Inr(t) => Inr(ct.value.functor.map(t)(f))
        } 
      }
      override def unfix(m: H :+: T) = m match {
        case Inl(h) => Inl(ch.value.unfix(h))
        case Inr(t) => Inr(ct.value.unfix(t))
      }
      override def fix(of: O[F]) = of match {
        case Inl(h) => Inl(ch.value.fix(h))
        case Inr(t) => Inr(ct.value.fix(t))
      }
  }
}
object UnfixHelper extends UnfixHelper1 {
  implicit def deriveF[F] = new UnfixHelper[F, Any, F] {
    override type O[A] = A
    override implicit val functor = new Functor[O] {
      override def map[A, B](fa: O[A])(f: A ⇒ B) = f(fa)
    }
    override def fix(f: F) = f
    override def unfix(f: F) = f
  }
}

sealed trait Unfix[F] {
  type O[A] <: Coproduct
  implicit val functor: Functor[O]
  def unfix(f: F): O[F]
  def fix(of: O[F]): F
}

object Unfix {
  
}
