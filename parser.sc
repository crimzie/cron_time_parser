#!/usr/bin/env amm

import java.io.FileNotFoundException
import java.nio.charset.CodingErrorAction.REPLACE
import java.time.LocalTime
import java.time.format.DateTimeParseException
import java.util.TimeZone

import scala.io.Codec.UTF8
import scala.io.{Codec, Source}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

implicit val codec: Codec = UTF8 onMalformedInput REPLACE onUnmappableCharacter REPLACE

@doc("This script parses a cron table file and selects tasks that fit into a time frame.")
@main
def main(
          crontab: String @doc("cron table file to scan for missed tasks"),
          from: String @doc("time frame start as time of day in hours and minutes separated with a colon"),
          till: String @doc("time frame end as time of day in hours and minutes separated with a colon")) = println {
  Try {
    val startTime: LocalTime = LocalTime parse from
    val endTime: LocalTime = LocalTime parse till
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
    } collect { case (a, b) if b => a } toSeq
  } match {
    case Failure(e: DateTimeParseException) => s"Failed to parse time frame: ${e.getMessage}."
    case Failure(_: FileNotFoundException) => "File not found."
    case Failure(_: SecurityException) => "File is not accessible for reading."
    case Failure(_: ArrayIndexOutOfBoundsException) => "Bad cron table formatting."
    case Failure(e) => s"Error: ${e.getMessage}"
    case Success(seq) => s"In file $crontab\n${
      if (seq.nonEmpty) s"${seq.size} task${if (seq.size > 1) "s were" else " was"} missed:\n${seq mkString "\n"}"
      else "no tasks were missed."
    }"
  }
}
