package vultura.fastfactors.algorithms

import vultura.fastfactors.Problem

/**
 * @author Thomas Geier
 * @since 6/23/13
 */
trait AlgConfig { outer =>
  def iterator(p: Problem, seed: Long): Iterator[InfAlg]
  def iterable(p: Problem, seed: Long) = new Iterable[InfAlg]{
    def iterator: Iterator[InfAlg] = outer.iterator(p,seed)
  }
}