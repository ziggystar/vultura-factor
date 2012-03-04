package vultura.playground

import vultura.cnf.dimacs.DIMACSParser
import util.Random
import vultura.search.UndirectedSearch
import vultura.cnf.CNF
import vultura.util._
import java.util.BitSet
import collection.IndexedSeq

/**
 * @author Thomas Geier
 * @since 20.02.12
 */

object WalkTW {
  def treeWidthBS(cliques: IndexedSeq[BitSet], ordering: Array[Int], hints: Array[Int] = null): Int = {
    val bitSetSize = ordering.size
    ordering.foldLeft((cliques,0)){case ((remainingCliques,max),variable) =>
      val newCliques = new Array[BitSet](remainingCliques.size)
      var ncPointer = 0
      val newClique = new BitSet(bitSetSize)
      var i = 0
      while(i < remainingCliques.size && remainingCliques(i) != null){
        if(remainingCliques(i).get(variable)){
          newClique.or(remainingCliques(i))
        } else {
          newCliques(ncPointer) = remainingCliques(i)
          ncPointer += 1
        }
        i += 1
      }
      //remove variable itself from the newly formed clique
      newClique.set(variable,false)
      val newCliqueSize = newClique.cardinality()
      if(newCliqueSize > 0){
        newCliques(ncPointer) = newClique
        ncPointer += 1
      }

      newCliques(ncPointer) = null

      if(hints != null){
        hints(variable) = newCliqueSize
      }
      (newCliques,math.max(max,newCliqueSize))
    }._2
  }

  def cnfAsUndirectedTWSearch: UndirectedSearch[CompiledCNF,IndexedSeq[Int],Double] = new UndirectedSearch[CompiledCNF,IndexedSeq[Int],Double] {

    def initialState(a: CompiledCNF, random: Random): IndexedSeq[Int] = random.shuffle(a.variables)
    def evaluate(a: CompiledCNF, state: IndexedSeq[Int]): Double = {
      treeWidthBS(a.variableSets,state.toArray,a.hints)
      math.log(a.hints.map(math.exp(_)).sum)
    }
    def randomSuccessor(a: CompiledCNF, ordering: IndexedSeq[Int], random: Random): IndexedSeq[Int] = {
//      //select one greedy and one random variable
//      val greedyVar = vultura.util.drawRandomlyBy(a.variables,random){v =>
//        if(a.hints(v) == 0) 0 else math.pow(a.hints(v),greed)
//      }
//      val nonGreedy = random.nextInt(ordering.size)
//      val builder = IndexedSeq.newBuilder[Int]
//
//      builder.sizeHint(ordering.size)
//      var idx = 0
//      while (idx < ordering.size) {
//        if (idx == greedyVar)
//          builder += ordering(nonGreedy)
//        else if (idx == nonGreedy)
//          builder += ordering(greedyVar)
//        else
//          builder += ordering(idx)
//        idx += 1
//      }
//      builder.result()
//    }
      Iterator.iterate(ordering)(vultura.util.randomFlip(_,random))
        .drop(1 + (random.nextGaussian().abs * 3).toInt)
        .next()
    }
  }

  case class CompiledCNF(variableSets: IndexedSeq[BitSet], variables: Array[Int]){
    val hints = new Array[Int](variables.max + 1)
  }

  def compileCNF(aCnf: CNF): CompiledCNF = {
    import vultura.cnf.CNFasBIFun._
    import vultura.factors._

    def variableBitSets(a: CNF): IndexedSeq[BitSet] = {
      a.clauses.map{clause =>
        val bs = new BitSet
        variables(clause).foreach(bs.set(_))
        bs
      }
    }
    CompiledCNF(variableBitSets(aCnf),aCnf.clauses.flatMap(variables(_)).distinct.toArray)
  }

  def randomSearch[A,S](problem: A, steps: Int, random: Random, acceptThreshold: Double = 0, noiseRatio: Double = 0)(implicit evUS: UndirectedSearch[A,S,Double]): Double = {
    var state = evUS.initialState(problem,random)
    var score = evUS.evaluate(problem,state)
    var stepCount = 0
    var best = score
    var lastBestStep = 0
    var lastBestTime = System.nanoTime()
    while(stepCount < steps){
      val candidate = evUS.randomSuccessor(problem,state,random)
      val candidateScore = evUS.evaluate(problem,candidate)
      if(candidateScore <= (score + acceptThreshold) || random.nextDouble() < noiseRatio / (candidateScore - score)) {
        state = candidate
        score = candidateScore

        if(score < best){
          best = score
//          val stepsSince = stepCount - lastBestStep
//          val timeDiff = System.nanoTime() - lastBestTime
//          lastBestTime = System.nanoTime()
//          lastBestStep = stepCount
//          println("%f at step %d, avg %.2fms per step (tw: %d)".format(
//            best,
//            stepCount,
//            timeDiff * 1e-6 / stepsSince,
//            treeWidthBS(problem.asInstanceOf[CompiledCNF].variableSets, state.asInstanceOf[IndexedSeq[Int]].toArray)
//          ))
        }
      }
      stepCount += 1
    }
    best
  }

  def fromToBy(f: Double, t: Double, by: Double): Seq[Double] = Iterator.iterate(f)(_ + by).takeWhile(_ <= t).toSeq

  def main(args: Array[String]) {
    val formulaFile = "../modelcounting/problems/AAAI06-suite-v2/other/php-010-020.cnf-DONT-USE"
    val cnf = DIMACSParser.readFile(formulaFile).toCNF
//    randomSearch(compileCNF(cnf),100000,new Random)(cnfAsUndirectedTWSearch)
    val runs = 20
    val searchSteps = 2000
    println("acceptTh\tnoise\tmean\tsd")
    for(
      acceptThreshold <- fromToBy(-0.1,0.1,0.05).par;
      noiseRatio <- fromToBy(0,0.002,0.0005)
    ) {
      val results = Seq.fill(runs)(randomSearch(compileCNF(cnf),searchSteps,new Random,acceptThreshold,noiseRatio)(cnfAsUndirectedTWSearch))
      println("%f\t%f\t%.1f\t%.2f".format(acceptThreshold,noiseRatio,results.mean,math.sqrt(results.variance)))
    }
  }
}