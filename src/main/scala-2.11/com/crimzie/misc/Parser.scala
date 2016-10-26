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
    System.exit(0)
  }

  if (args(0) == "help") {
    println("This script takes 3 string arguments separated with spaces:\n" +
      "filepath to the cron table file\n" +
      "start time in hours and minutes separated with a colon\n" +
      "end time in hours and minutes separated with a colon.")
    System.exit(0)
  }

  val file: String = args(0)
  val startTime: LocalTime = LocalTime parse this.args(1)
  val endTime: LocalTime = LocalTime parse this.args(2)

  implicit val codec: Codec =
    UTF8
      .onMalformedInput(REPLACE)
      .onUnmappableCharacter(REPLACE)

  val skipped =
    Source.fromFile(file)
      .getLines
      .filter(_.nonEmpty)
      .sliding(3, 3)
      .map { x =>
        val fields: Array[String] =
          x(2)
            .split(" ")
            .filter(_.nonEmpty)
        (x.head,
          x(1).split("=")(1), // timezone
          fields(1), // hours
          fields(0), // minutes
          fields.drop(6).mkString(" "))
      }
      .filter(x => x._3.head != '*' && x._4.head != '*')
      .map { x =>
        val offset: Int = TimeZone.getTimeZone(x._2).getRawOffset / 1000
        val time: Int = (x._3.toInt * 60 + x._4.toInt) * 60
        val locStart: Int = startTime.plusSeconds(offset).toSecondOfDay
        val locEnd: Int = endTime.plusSeconds(offset).toSecondOfDay
        val daySpan: Boolean = locStart > locEnd
        val inTimeFrame: Boolean = locStart < time && locEnd >= time
        val skipped: Boolean = (!daySpan && inTimeFrame) || (daySpan && !inTimeFrame)
        (s" ${x._1}\n  ${x._5}", skipped)
      }
      .withFilter(_._2)
      .map(_._1)

  println(s"In file: $file\n")
  if (skipped hasNext) {
    println(s"${skipped size} tasks were missed:")
    skipped foreach println
  } else {
    println("no tasks were missed.")
  }

}
