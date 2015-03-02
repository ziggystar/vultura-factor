package vultura.factor.generators

import vultura.factor.{Problem, LabeledPS, FactorGenerator}

import scala.collection.immutable
import scala.util.Random

/** Two-Layer hierarchical grid problems.
 */
case class GridH2(cellWidth: Int = 3, cellHeight: Int = 3, cellRows: Int = 3, cellCols: Int = 3) {
  type N = (Int,Int,Int)
  type F = (N,N)
  val labeledPS: LabeledPS[N,F] = {
    val lwidth = cellWidth * cellCols
    val lheight = cellHeight * cellRows

    def isValidLowerNode(ln: N) = ln._3 == 0 &&
      (0 until lwidth).contains(ln._1) && (0 until lheight).contains(ln._2)

    val lowerGridScopes: IndexedSeq[(F, Set[N])] = for {
      xl <- 0 until lwidth
      yl <- 0 until lheight
      n <- Seq((xl,yl+1,0),(xl + 1,yl,0)).filter(isValidLowerNode)
    } yield {
      val lowerNode: N = (xl, yl, 0)
      (lowerNode, n) -> Set(lowerNode, n)
    }

    val lowerGrid: LabeledPS[N,F] = LabeledPS.empty.addFactors(lowerGridScopes:_*)
    def isHighLowConnection(hn: N, ln: N): Boolean =
      hn._3 == 1 && ln._3 == 0 &&
        (0 until cellWidth).map(_ + hn._1 * cellWidth).contains(ln._1) &&
        (0 until cellHeight).map(_ + hn._2 * cellHeight).contains(ln._2)
    def lowerNeighbours(hn: N): Set[N] = lowerGrid.variables.filter(isHighLowConnection(hn, _))

    val higherNodes: IndexedSeq[N] = for(hx <- 0 until cellCols; hy <- 0 until cellRows) yield (hx,hy,1)
    val twoTierProblem = lowerGrid.addFactors(
      higherNodes.flatMap(hn => lowerNeighbours(hn).map(ln => (hn,ln) -> Set(hn,ln))):_*)

    def isValidUpperNode(n: N) = n._3 == 1 && (0 until cellCols).contains(n._1) && (0 until cellRows).contains(n._2)

    val highInteractions: IndexedSeq[(F,Set[N])] = for{
      hx <- 0 until cellCols
      hy <- 0 until cellRows
      n <- Seq((hx,hy+1,1),(hx + 1,hy,1)).filter(isValidUpperNode)
    } yield {
      val hn = (hx,hy,1)
      (hn,n) -> Set(hn,n)
    }

    twoTierProblem.addFactors(highInteractions:_*)
  }

  def generate(domainSize: Int, lowerPot: FactorGenerator, interPot: FactorGenerator, upperPot: FactorGenerator, random: Random): Problem =
    labeledPS.generateProblem(
      domainSize = _=> domainSize,
      potGen = {
        case ((_,_,0),(_,_,0)) => lowerPot
        case ((_,_,1),(_,_,0)) => interPot
        case ((_,_,1),(_,_,1)) => upperPot
      },
      random = random
    )
}
