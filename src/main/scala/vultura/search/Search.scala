package vultura.search

import vultura.factors.Factor
import vultura.util._
import util.Random

/**
 * Type class that defines a search problem with states of type `S` and result of type `R`.
 * @author Thomas Geier
 * @since 13.02.12
 */

trait Search[A,S,R] {
  def initialState(a: A): S
  def successors(a: A, state: S): IndexedSeq[S]
  def evaluate(a: A, state: S): Option[R]
  def numSuccessors(a: A, state: S): Int = successors(a,state).size
  def randomSuccessor(a: A, state: S, random: Random): Option[S] = successors(a,state).pickRandomOpt(random)
  def randomPlayout(a: A, state: S, random: Random): R =
    this.evaluate(a,Iterator.iterate(Some(state): Option[S])(_.flatMap(s => this.randomSuccessor(a,s,random))).flatten.last).get
}

object Search {
  import scalaz._
  import Scalaz._

  def mctsMinInt[A,S](problem: A, random: Random, playouts: Int)(implicit evS: Search[A,S,Int]): Int = {
    //returns a tree and the result of the play-out
    def oneStep(tree: Tree[(S,Int,Int)]): (Tree[(S,Int,Int)],Int) = tree match {
      //reached a node where not all children are explored, yet; might also be a leaf or even a terminal node
      case Node((state,total,visits),children) if(children.size != evS.numSuccessors(problem,state)) => {
        if(evS.numSuccessors(problem,state) == 0)
          //this might be a terminal node
          (leaf((state,(total / visits) * (visits + 1),visits + 1)),total)
        else {
          //simple take the next successor, that has not been visited, yet
          val nextState = evS.successors(problem,state).filterNot(children.contains).head
          val playoutValue: Int = evS.randomPlayout(problem,nextState,random)
          val newChildNode: Tree[(S, Int, Int)] = node(
            (state, total + playoutValue, visits + 1),
            leaf((nextState, playoutValue, 1)) +: children
          )
          (newChildNode, playoutValue)
        }
      }
      //reached a node where all children are explored; make tree descent
      case Node((state,total,visits),children) => {
        def uctValue(child: (S,Int,Int)): Double = {
          val mean = child._2.toDouble / child._3
          val uct = math.sqrt(2 * math.log(visits)/child._3)
          uct / mean
        }
        val selectedChild = drawRandomlyBy(children,random)(subTree => uctValue(subTree.rootLabel))
        val (updatedChildTree,playout) = oneStep(selectedChild)
        (node((state,total + playout,visits + 1), children.filterNot(_ == selectedChild) :+ updatedChildTree), playout)
      }
    }

    //contains (state,mean,numberPlayouts)
    val initialTree: Tree[(S,Int,Int)] = leaf((evS.initialState(problem),0,0))

    val treeSeq = Iterator.iterate((initialTree,Integer.MAX_VALUE)){case (tree,bestValue) =>
      val ((newTree, playout),stepTime) = vultura.util.Benchmark.benchmarkCPUTime(oneStep(tree))
      println(stepTime/1e6 + "ms :" + (playout,bestValue))
      (newTree,math.min(bestValue,playout))
    }

    treeSeq.drop(playouts).next()._2
  }
}