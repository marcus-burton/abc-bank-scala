package com.abc

import org.joda.time._

object DateProvider {

  def getInstance: DateProvider =
  {
    if (instance.isEmpty) instance = Some(new DateProvider)
    instance.get
  }

  private var instance: Option[DateProvider] = None
}

class DateProvider {
  def now = DateTime.now
}

