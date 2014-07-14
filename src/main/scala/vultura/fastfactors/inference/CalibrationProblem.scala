package vultura.fastfactors.inference

import vultura.fastfactors.{FastFactor, Problem}

import scala.collection.immutable.IntMap
import scala.util.Random

/**
 * Describes a propagation problem by a directed graph with messages as nodes and weighted edges. Nodes without incoming
 * edges are not changed.
 */
trait CalibrationProblem[A] {
  /** For domains and ring. */
  def problem: Problem
  /** Map each node to a set of parents. Each parent has a weight and is either another node or a constant factor. */
  def parents: Map[A, Set[(Either[FastFactor,A],Double)]]
  def nodes: Set[A]
  def targetNodes: Set[A]
  /** Each node has a set of problem-variables as its scope. */
  def nodeScope: Map[A,Set[Int]]

  def toDot: String = {
    val (normalized,map) = CalibrationProblem.normalizeNodes(this)
    val revMap = map.map(_.swap)
    val nodeStrings = normalized.nodes.map(id => f"""n$id [label="${revMap(id)}"];""")
    val edgeStrings = for{
      n <- normalized.nodes
      (Right(parent),weight) <- normalized.parents(n)
    } yield f"""n$parent -> n$n;"""

    f"""digraph RegionGraph {
        | rankdir = TB;
        | ${nodeStrings.mkString("\n")}
        | ${edgeStrings.mkString("\n")}
        |}
      """.stripMargin
  }
}

object CalibrationProblem {

  def normalizeNodes[A](cp: CalibrationProblem[A]): (CalibrationProblem[Int], Map[A,Int]) = {
    val map: Map[A, Int] = cp.nodes.zipWithIndex.toMap
    val p = new CalibrationProblem[Int]{
      /** For domains and ring. */
      def problem: Problem = cp.problem

      /** Map each node to a set of parents. Each parent has a weight and is either another node or a constant factor. */
      def parents: Map[Int, Set[(Either[FastFactor, Int], Double)]] = cp.parents.map{
        case (a,preds) => map(a) -> preds.map{
          case (Right(parent),x) => (Right(map(parent)): Either[FastFactor,Int]) -> x
          case (Left(f),x) => Left(f) -> x
        }
      }

      def nodes: Set[Int] = cp.nodes.map(map)

      def targetNodes: Set[Int] = cp.targetNodes.map(map)

      /** Each node has a set of problem-variables as its scope. */
      def nodeScope: Map[Int, Set[Int]] = cp.nodeScope.map{case (k,v) => map(k) -> v}
    }
    (p,map)
  }

  sealed trait BetheNode
  case class FactorN(f: FastFactor) extends BetheNode
  case class VariableN(v: Int) extends BetheNode
  case class F2V(f: FastFactor,v: Int) extends BetheNode
  case class V2F(v: Int, f: FastFactor) extends BetheNode

  def betheCalibrationProblem(p: Problem) = new CalibrationProblem[BetheNode] {
    /** For domains and ring. */
    def problem: Problem = p

    /** One message along each direction of a connected variable/factor pair. */
    def nodes: Set[BetheNode] = p.variables.map(VariableN) ++
      p.factors.map(FactorN) ++
      (for(f <- p.factors; v <- f.variables) yield F2V(f,v)) ++
      (for(f <- p.factors; v <- f.variables) yield V2F(v,f))

    /** Map each node to a set of parents. Each parent has a weight and is either another node or a constant factor. */
    def parents: Map[BetheNode, Set[(Either[FastFactor, BetheNode], Double)]] =
      nodes.map{
        case n@F2V(f,v) => {
          val incomingMsgs = for {
            otherVar <- f.variables if otherVar != v
          } yield Right(V2F(otherVar,f)) -> 1d
          val localFactor = Left(f)  -> 1d
          val allParentNodes: Set[(Either[FastFactor, BetheNode], Double)] = incomingMsgs.toSet + localFactor
          n -> allParentNodes
        }
        case n@V2F(v,f) => {
          val incomingMsgs: IndexedSeq[(Right[Nothing, BetheNode], Double)] = for{
            otherFactor <- p.factors if otherFactor.variables.contains(v) && otherFactor != f
          } yield Right(F2V(otherFactor, v)) -> 1d
          n -> (incomingMsgs.toSet: Set[(Either[FastFactor, BetheNode], Double)])
        }
        case n@VariableN(v) => {
          val incomingMsgs: IndexedSeq[(Right[Nothing, BetheNode], Double)] = for{
            otherFactor <- p.factors if otherFactor.variables.contains(v)
          } yield Right(F2V(otherFactor, v)) -> 1d
          n -> (incomingMsgs.toSet: Set[(Either[FastFactor, BetheNode], Double)])
        }
        case n@FactorN(f) => {
          val incomingMsgs = for {
            otherVar <- f.variables
          } yield Right(V2F(otherVar,f)) -> 1d
          val localFactor = Left(f)  -> 1d
          val allParentNodes: Set[(Either[FastFactor, BetheNode], Double)] = incomingMsgs.toSet + localFactor
          n -> (allParentNodes: Set[(Either[FastFactor, BetheNode], Double)])
        }

      }(collection.breakOut)

    def targetNodes: Set[BetheNode] = p.variables.map(VariableN) ++ p.factors.map(FactorN)

    /** Each node has a set of problem-variables as its scope. */
    def nodeScope: Map[BetheNode, Set[Int]] = nodes.map{
      case n@FactorN(f) => n -> f.variables.toSet
      case n@VariableN(v) => n -> Set(v)
      case n@F2V(_,v) => n -> Set(v)
      case n@V2F(v,_) => n -> Set(v)
    }(collection.breakOut)
  }
}

//TODO the schedule should always be the same with the same random seed
class RoundRobinCalibrator[A](_graph: CalibrationProblem[A], maxSteps: Int = 10, tol: Double = 1e-10, random: Random = new Random(0)){
  val (graph,nodeMap) = CalibrationProblem.normalizeNodes(_graph)

  private val domains = graph.problem.domains
  private val ring = graph.problem.ring

  private val messages: IntMap[FastFactor] = {
    def createFactor(n: Int): FastFactor = {
      FastFactor.maxEntropy(graph.nodeScope(n).toArray.sorted, graph.problem.domains, graph.problem.ring)
    }
    IntMap(graph.nodes.map(n => n -> createFactor(n)).toSeq:_*)
  }

  val schedule = graph.nodes
  for{
    s <- 1 to maxSteps
    n <- schedule
  }{
    updateNode(n)
  }

  private def updateNode(n: Int): Unit = {
    val incoming = graph.parents(n).toSeq.map{
      case (Left(f),1d) => f
      case (Right(node),1d) => messages(node)
      case (_,x) => sys.error("Calibrator cannot handle weights different from 1: " + x)
    }
    val result = FastFactor.multiplyRetain(ring)(domains)(incoming,messages(n).variables).normalize(ring)
    System.arraycopy(result.values,0,messages(n).values,0,result.values.size)
  }

  def resultOf(node: A): FastFactor = messages(nodeMap(node))
}
