package vultura.util

import java.lang.management.ManagementFactory

/**
 * Contains functions for simple benchmarking.
 * User: Thomas Geier
 * Date: 03.02.12
 */

object Benchmark {
  def benchmarkWallTime[A](f: => A): (A, Long) = {
    val startTime = System.nanoTime()
    val result = f
    val runningTime = System.nanoTime() - startTime

    (result, runningTime)
  }

  def benchmarkCPUTime[A](f: => A): (A, Long) = {

    val bean = ManagementFactory.getThreadMXBean
    if (!bean.isThreadCpuTimeSupported) throw new RuntimeException("measuring CPU time is not supported")

    val startTime = bean.getThreadCpuTime(Thread.currentThread.getId)
    val result = f
    val endTime = bean.getThreadCpuTime(Thread.currentThread.getId)
    val cpuTime = endTime - startTime

    (result, cpuTime)
  }

  /**@return The result of the computation and a tuple with first entry wall time and second entry CPU time. */
  def benchmarkBothTimes[A](f: => A): (A, (Long, Long)) = {
    val bean = ManagementFactory.getThreadMXBean
    if (!bean.isThreadCpuTimeSupported) throw new RuntimeException("measuring CPU time is not supported")

    val startTimeCPU = bean.getThreadCpuTime(Thread.currentThread.getId)
    val startTimeWall = System.nanoTime()
    val result = f

    val endTimeCPU = bean.getThreadCpuTime(Thread.currentThread.getId)
    val endTimeWall = System.nanoTime()

    (result, (endTimeWall - startTimeWall, endTimeCPU - startTimeCPU))
  }
}
