package com.github.m50d.tailchase

import org.junit.Test
import shapeless._
import org.fest.assertions.Assertions.assertThat

case class TestCaseClass(f: Float, i: Int)

class UnfixTest {
  @Test def unfixHelperHList(): Unit = {
    val uh = the[UnfixHelper[Int, Any, String :: Int :: HNil]]
    assertThat(uh.functor.map("Hello" :: 4 :: HNil)({ i: Int ⇒ i / 2.0 })).isEqualTo("Hello" :: 2.0 :: HNil)
  }

  @Test def unfixHelperCaseClass(): Unit = {
    val uh = the[UnfixHelper[Float, Any, TestCaseClass]]
    assertThat(uh.functor.map(2f :: 4 :: HNil)(_.toString)).isEqualTo("2.0" :: 4 :: HNil)
  }

  @Test def unfixHelperSealedFamily(): Unit = {
    val uh = the[UnfixHelper[Int, Any, Either[Int, String]] with NonTrivial]
    assertThat(uh.fix(uh.functor.map(uh.unfix(Left(3))) { i: Int ⇒ i + 1 })).isEqualTo(Left(4))
  }
}