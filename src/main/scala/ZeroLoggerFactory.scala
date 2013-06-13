import com.dongxiguo.zeroLog.appenders.ConsoleAppender
import com.dongxiguo.zeroLog.Filter
import com.dongxiguo.zeroLog.formatters.SimpleFormatter
import vultura.fastfactors.algorithms.{CBP, BeliefPropagation}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/11/13
 */
object ZeroLoggerFactory {
  // Set package com.yourDomain.yourProject's default logging level to Info
  final def newLogger(singleton: Singleton) =
    (Filter.Fine, SimpleFormatter, ConsoleAppender)

  // Set Sample's logging level to Finest
  final def newLogger(singleton: BeliefPropagation.type) =
    (Filter.Info, SimpleFormatter, ConsoleAppender)

  // Set Sample's logging level to Finest
  final def newLogger(singleton: CBP.type) =
    (Filter.Finer, SimpleFormatter, ConsoleAppender)
}
