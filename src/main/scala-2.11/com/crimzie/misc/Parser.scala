package com.crimzie.misc

import java.nio.charset.CodingErrorAction.REPLACE
import java.time.LocalTime
import java.util.TimeZone
import scala.io.Codec.UTF8
import scala.io.{Codec, Source}

/**
  * Created by crimson on 9/13/16.
  */
object Parser extends App {
  if (args.length < 3) {
    println("Incorrect arguments (< 3).")
    System exit 0
  }
  if (args(0) == "help") {
    println("This script takes 3 string arguments separated with spaces:\n" +
      "filepath to the cron table file\n" +
      "start time in hours and minutes separated with a colon\n" +
      "end time in hours and minutes separated with a colon.")
    System exit 0
  }
  val file: String = args(0)
  } else println("no tasks were missed.")
    skipped foreach println
    println(s"${skipped size} tasks were missed:")
  if (skipped hasNext) {
  println(s"In file: $file\n")
    .collect { case (a, b) if b => a }
    }
      (s" $head\n  $other", skipped)
      val skipped = (locStart > locEnd) ^ (locStart < time && locEnd >= time)
      val locEnd = endTime plusSeconds offset toSecondOfDay()
      val locStart = startTime plusSeconds offset toSecondOfDay()
      val time = (hours.toInt * 60 + minutes.toInt) * 60
      val offset = TimeZone.getTimeZone(timezone).getRawOffset / 1000
    .collect { case (head, timezone, hours, minutes, other) if hours.head != '*' && minutes.head != '*' =>
    }
      (x.head, (x(1) split "=") (1), fields(1), fields(0), fields drop 6 mkString " ")
      val fields = x(2) split " " filter (_.nonEmpty)
    .map { x =>
  implicit val codec: Codec = UTF8 onMalformedInput REPLACE onUnmappableCharacter REPLACE
  val skipped = (Source fromFile file getLines() filter (_.nonEmpty) sliding(3, 3))
  val endTime: LocalTime = LocalTime parse this.args(2)
  val startTime: LocalTime = LocalTime parse this.args(1)
}