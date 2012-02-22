package vultura.search

import vultura.util._
import util.Random

/**
 * Type class that defines a search problem with states of type `S` and result of type `R`.
 * @author Thomas Geier
 * @since 13.02.12
 */

trait ProgressiveSearch[A,S,R] {
  def initialState(a: A): S
  def successors(a: A, state: S): IndexedSeq[S]
  def evaluate(a: A, state: S): Option[R]
  def numSuccessors(a: A, state: S): Int = successors(a,state).size
  def randomSuccessor(a: A, state: S, random: Random): Option[S] = successors(a,state).pickRandomOpt(random)
  def randomPlayout(a: A, state: S, random: Random): R =
    this.evaluate(a,Iterator.iterate(Some(state): Option[S])(_.flatMap(s => this.randomSuccessor(a,s,random))).flatten.last).get
}

object ProgressiveSearch {
  import scalaz._
  import Scalaz._

  def mctsMin[A,S](problem: A, random: Random, playouts: Int)(implicit evS: ProgressiveSearch[A,S,Double]): Double = {
    //returns a tree and the result of the play-out
    def oneStep(tree: Tree[(S,Double,Int)]): (Tree[(S,Double,Int)],Double) = tree match {
      //reached a node where not all children are explored, yet; might also be a leaf or even a terminal node
      case Node((state,minParent,visits),children) if(children.size != evS.numSuccessors(problem,state)) => {
        if(evS.numSuccessors(problem,state) == 0)
          //this might be a terminal node
          (leaf((state,minParent,visits + 1)),minParent)
        else {
          //simple take the next successor, that has not been visited, yet
          val nextState = evS.successors(problem,state).filterNot(children.contains).head
          val playoutValue: Double = evS.randomPlayout(problem,nextState,random)
          val newChildNode: Tree[(S, Double, Int)] = node(
            (state, math.min(minParent,playoutValue), visits + 1),
            leaf((nextState, playoutValue, 1)) +: children
          )
          (newChildNode, playoutValue)
        }
      }
      //reached a node where all children are explored; make tree descent
      case Node((state,minParent,visitsParent),children) => {
        def uctValue(child: (S,Double,Int)): Double = {
          val (_,minChild,visitsChild) = child
          val uct = math.sqrt(math.log(visitsParent)/(visitsChild*50))
          val minSigmoid = 1/(1+math.exp(minParent - minChild) * 20)
          val result = 0.03 * uct + minSigmoid
          //println("%f/%d <- %f/%d : %f".format(minParent,visitsParent,minChild,visitsChild,result))
          result
        }
        val selectedChild = maxByMultiple(children)(subTree => uctValue(subTree.rootLabel)).pickRandom(random)
        val (updatedChildTree,playout) = oneStep(selectedChild)
        (node((state,minParent min playout,visitsParent + 1), children.filterNot(_ == selectedChild) :+ updatedChildTree), playout)
      }
    }

    //contains (state,mean,numberPlayouts)
    val initialTree: Tree[(S,Double,Int)] = leaf((evS.initialState(problem),Double.MaxValue,0))

    val treeSeq = Iterator.iterate((initialTree,Double.MaxValue)){case (tree,bestValue) =>
      val ((newTree, playout),stepTime) = vultura.util.Benchmark.benchmarkCPUTime(oneStep(tree))
      println(stepTime/1e6 + "ms :" + (playout * 200,bestValue * 200))
      (newTree,math.min(bestValue,playout))
    }

    treeSeq.drop(playouts).next()._2
  }
}