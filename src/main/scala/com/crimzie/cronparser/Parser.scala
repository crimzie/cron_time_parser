package com
package crimzie
package cronparser

import java.io.FileNotFoundException
import java.nio.charset.CodingErrorAction.REPLACE
import java.time.LocalTime
import java.time.format.DateTimeParseException
import java.util.TimeZone

import scala.io.Codec.UTF8
import scala.io.{Codec, Source}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object Parser extends App {
  implicit val codec: Codec = UTF8 onMalformedInput REPLACE onUnmappableCharacter REPLACE

  if (args(0) == "help") {
    println("This script takes 3 arguments:\n" +
      "cron table file;\n" +
      "start time in hours and minutes separated with a colon;\n" +
      "end time in hours and minutes separated with a colon.")
    System exit 0
  }
  if (args.length < 3) {
    println("Not enough arguments.")
    System exit 1
  }

  val crontab: String = args(0)
  Try {
    val startTime: LocalTime = LocalTime parse args(1)
    val endTime: LocalTime = LocalTime parse args(2)
    /* Read the file, drop empty lines, group lines by 3: */
    Source fromFile crontab getLines() filter (_.nonEmpty) sliding(3, 3) map { seq =>
      /* Split third lines by spaces: */
      val fields = seq(2) split ' ' filter (_.nonEmpty)
      /* Return Tuple5 with task name, task timezone, hours, minutes, task command: */
      (seq.head, seq(1) split '=' apply 1, fields(1), fields(0), fields drop 5 mkString " ")
      /* Filter in tasks with hours and minutes set: */
    } collect { case (head, timezone, hours, minutes, other) if hours.head != '*' && minutes.head != '*' =>
      /* Convert timezone to offset seconds: */
      val offset = (TimeZone getTimeZone timezone getRawOffset) / 1000
      /* Convert task time to seconds of day: */
      val time = (hours.toInt * 60 + minutes.toInt) * 60
      /* Localize provided time frame: */
      val locStart = startTime plusSeconds offset toSecondOfDay()
      val locEnd = endTime plusSeconds offset toSecondOfDay()
      /* Check if task falls into provided time frame: */
      val skipped = (locStart > locEnd) ^ (locStart < time && locEnd >= time)
      /* Format task name and task command into output string and filter in skipped tasks: */
      (s" $head\n  $other", skipped)
    } collect { case (a, b) if b => a }
  } match {
    case Failure(e) => e match {
      case _: DateTimeParseException => println("Failed to parse time frame.")
      case _: FileNotFoundException => println("File not found.")
      case _: SecurityException => println("File is not accessible for reading.")
      case _: ArrayIndexOutOfBoundsException => println("Bad cron table formatting.")
      case _ => println(s"Error: ${e.getMessage}")
    }
      System exit 1
    case Success(iter) =>
      println(s"In file $crontab\n")
      if (iter.nonEmpty) {
        val seq = iter.toSeq
        println(s"${seq.size} tasks were missed:")
        seq foreach println
      } else println("no tasks were missed.")
  }
}
