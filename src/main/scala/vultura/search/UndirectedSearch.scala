package vultura.search

import util.Random

/**
 * @author Thomas Geier
 * @since 20.02.12
 */

trait UndirectedSearch[A,S,R] {
  def initialState(a: A, random: Random): S
  def evaluate(a: A, state: S): R
  def randomSuccessor(a: A, state: S, random: Random): S
}