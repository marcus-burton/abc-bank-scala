package com.abc

import org.joda.time.DateTime

object DateProvider {
  def getInstance: DateProvider = {
    //TODO this will not always get a new datestamp
    if (instance.isEmpty) instance = Some(new DateProvider)
    instance.get
  }

  // TODO unclear why any reqt to maintain previous date
  private var instance: Option[DateProvider] = None
}

class DateProvider {
  def now: DateTime = DateTime.now
}

