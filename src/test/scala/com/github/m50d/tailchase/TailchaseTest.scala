package com.github.m50d.tailchase

import shapeless._
import syntax._
import org.junit.Test
import org.fest.assertions.Assertions.assertThat

class TailchaseTest {
  @Test def cata(): Unit = {
    val l = List(1, 2, 3, 4, 5)
    assertThat(l.cata[Int] {
      case Inl((x: Int) :: (y: Int) :: HNil) ⇒ x + y
      case Inr(_) ⇒ 0
    }).isEqualTo(15)
    assertThat(l.cata[String] {
      case Inl((x: Int) :: (y: String) :: HNil) ⇒ y + x
      case Inr(_) ⇒ ""
    }).isEqualTo("54321")
  }
}