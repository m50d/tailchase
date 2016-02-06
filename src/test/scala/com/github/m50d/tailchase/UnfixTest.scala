package com.github.m50d.tailchase

import org.junit.Test
import shapeless._
import org.fest.assertions.Assertions.assertThat

class UnfixTest {
  @Test def unfixHelperHList(): Unit = {
    val uh = the[UnfixHelper[Int, Any, String :: Int :: HNil]]
    assertThat(uh.functor.map("Hello" :: 4 :: HNil)({i: Int => i / 2.0})).isEqualTo("Hello" :: 2.0 :: HNil)
  }
}