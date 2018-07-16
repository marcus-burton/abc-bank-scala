package com.abc.mutable

import java.time.Instant
import org.scalatest.{Matchers, WordSpec}

class SystemInstantProviderTest extends WordSpec with Matchers {

  "SystemInstantProvider.now" should {
    "create Instants on demand" when {
      "called" in {
        val time1, time2: Instant = SystemInstantProvider.now
        time1 should not be null
        time1 shouldNot be theSameInstanceAs time2
      }
    }
  }
}
