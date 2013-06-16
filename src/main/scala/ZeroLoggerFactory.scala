import com.dongxiguo.fastring.Fastring.Implicits._
import com.dongxiguo.zeroLog.appenders.ConsoleAppender
import com.dongxiguo.zeroLog.context.{CurrentMethodName, CurrentClass, CurrentLine, CurrentSource}
import com.dongxiguo.zeroLog.{Level, Formatter, Filter}
import com.dongxiguo.zeroLog.formatters.SimpleFormatter
import java.util.Calendar
import java.util.Calendar._
import scala.compat.Platform
import vultura.fastfactors.algorithms.{CBP, BeliefPropagation}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/11/13
 */
object ZeroLoggerFactory {
    // Set package com.yourDomain.yourProject's default logging level to Info
  final def newLogger(singleton: Singleton) =
    (Filter.Info, SimplerFormatter, ConsoleAppender)

  final def newLogger(singleton: BeliefPropagation.type) =
    (Filter.Info, SimplerFormatter, ConsoleAppender)

  final def newLogger(singleton: CBP.type) =
    (Filter.Info, SimplerFormatter, ConsoleAppender)

  object SimplerFormatter extends Formatter {
    private def now: Fastring = {
      val calendarNow = Calendar.getInstance
      import calendarNow.get
      import Calendar._
      fast"${
        get(YEAR).filled(4, '0')
      }-${
        (get(MONTH) + 1).filled(2, '0')
      }-${
        get(DATE).filled(2, '0')
      } ${
        get(HOUR_OF_DAY).filled(2, ':')
      }-${
        get(MINUTE).filled(2, ':')
      }-${
        get(SECOND).filled(2, '0')
      }"
    }
    override final def format(
      level: Level,
      message: Fastring,
      currentSource: CurrentSource,
      currentLine: CurrentLine,
      currentClass: CurrentClass,
      currentMethodNameOption: Option[CurrentMethodName]): Fastring = {
      fast"${level.name}:$now: $message${Platform.EOL}"
    }

    override final def format[A](
      level: Level,
      message: Fastring,
      throwable: Throwable,
      currentSource: CurrentSource,
      currentLine: CurrentLine,
      currentClass: CurrentClass,
      currentMethodNameOption: Option[CurrentMethodName]): Fastring = {
      fast"${level.name}:$now: $message ${throwable}"
    }

    override final def format(
      level: Level,
      throwable: Throwable,
      currentSource: CurrentSource,
      currentLine: CurrentLine,
      currentClass: CurrentClass,
      currentMethodNameOption: Option[CurrentMethodName]): Fastring = {
      fast"${level.name}:$now: ${throwable}"
    }
  }
}
