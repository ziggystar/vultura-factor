package vultura.factor

import java.io._

import fastparse.WhitespaceApi
import fastparse.core.Parsed.Success
import vultura.factor.inference.VariableElimination
import vultura.util.SSet

/** A problem is basically a collection of factors, together with a domain and a ring.
  * It provides several inference methods based on the exact junction tree algorithm. */
case class Problem(factors: IndexedSeq[Factor], domains: Array[Int], ring: Ring[Double]) extends ProblemStructure {

  lazy val scopeOfFactor: Array[Array[VI]] = factors.map(_.variables)(collection.breakOut)

  def factorsOfVariable(v: Int): Array[Factor] = factorIdxOfVariable(v).map(factors)

  def filter(p: Factor => Boolean): Problem = this.copy(factors=factors.filter(p))
  def map(p: Factor => Factor): Problem = this.copy(factors=factors.map(p))

  def logFactor(fi: Int): Factor = if(ring == LogD) factors(fi) else factors(fi).map(math.log)

  //noinspection SameElementsToEquals
  def uaiString: String = {
    def writeDouble(d: Double): String = d match {
      case 0d => "0"
      case x => x.toString
    }
    Seq[Any](
      "MARKOV",
      variables.size,
      domains.mkString(" "),
      factors.size,
      factors.map(f => f.variables.length + " " + f.variables.reverse.mkString(" ")).mkString("\n"),
      factors.map(f => f.values.length + " " + ring.decode(f.values).map(writeDouble).mkString(" ")).mkString("\n")
    ).mkString("\n")
  }

  override def hashCode: Int = {
    import scala.util.hashing.MurmurHash3._
    val mix1: Int = mix(arrayHash(domains), orderedHash(factors))
    val mix2: Int = mixLast(mix1, ring.hashCode())
    finalizeHash(mix2, factors.size)
  }
  override def equals(obj: Any): Boolean = obj match {
    case Problem(oFactors, oDomains, oRing) => factors == oFactors && domains.deep == oDomains.deep && ring == oRing
    case _ => false
  }

  lazy val hasDuplicateFactors: Boolean = factors.size != factors.toSet.size

  def toRing(newRing: Ring[Double]): Problem = Problem(factors.map(f => newRing.encode(ring.decode(f))),domains,newRing)

  def toBriefString: String = f"(Problem: ${variables.size} variables, ${factors.size} factors, ring: $ring)"

  /** @return Exact log Z obtained by junction tree algorithm. */
  lazy val logZ: Double = VariableElimination(this).logZ

  /** merges factors into other factors where possible */
  def simplify: Problem = {
    val sset: SSet[Int] = new SSet(factors.map(_.variables.toSet)(collection.breakOut))
    val aggregatedFactors: IndexedSeq[Factor] = factors
      .groupBy(f => sset.maximalSuperSetsOf(f.variables.toSet).maxBy(_.size))
      .map{case (_,v) => Factor.multiply(ring)(domains)(v)}(collection.breakOut)
    copy(factors = aggregatedFactors)
  }

  /** Set some variables to values and simplify the problem.
    * @param condition Maps variables to the values they shall assume.
    * @return The resulting problem. It will contain a constant representing the product over the now assigned factors.
    */
  def condition(condition: Map[Var,Val]): Problem = {
    val factorsConditioned: Problem = map(_.condition(condition,domains))
    factorsConditioned.copy(factors = factorsConditioned.factors ++ condition.map(kv => Factor.deterministicMaxEntropy(Array(kv._1),Map(kv),domains,ring)))
  }

  def fixUncoveredVariables: Problem = {
    val newFactors = for{
      v <- variables if factorDegreeOfVariable(v) == 0
    } yield Factor.deterministicMaxEntropy(Array(v),Map(),domains, ring)
    this.copy(factors = factors ++ newFactors)
  }

  def hasUncoveredVariable: Boolean = factorDegreeOfVariable.contains(0)

  /** Compute the mutual information between a pair of variables induced only by direct interactions between them.
    * This is not the exact mutual information induced by the joint distribution.
    * @param v1
    * @param v2
    * @return None if the variables are not neighbours.
    */
  def localMutualInformation(v1: VI, v2: VI): Double = {
    def factorMI(f: Factor): Double = {
      require(f.variables.length == 2)
      val hxy = ring.entropy(f.values)
      val h1 = ring.entropy(Factor.multiplyRetain(ring)(domains)(Seq(f),Array(v1)).values)
      val h2= ring.entropy(Factor.multiplyRetain(ring)(domains)(Seq(f),Array(v2)).values)
      h1 + h2 - hxy
    }
    val coveringFactors: Array[VI] = factorIdxOfVariable(v1).filter(factorIdxOfVariable(v2).toSet)

    if(coveringFactors.isEmpty) 0d
    else factorMI(Factor.multiplyRetain(ring)(domains)(coveringFactors.map(factors), Array(v1,v2)).normalize(ring))
  }
}

object Problem{
  /** @return Parser for a problem, and a boolean that indicates whether the file contained a bayesian network. */
  def uaiParser: fastparse.all.P[(Problem,Boolean)] = {
    val White = WhitespaceApi.Wrapper{
      import fastparse.all._
      NoTrace(P(CharIn(" \t\n").rep))
    }
    import White._

    import fastparse.noApi._

    def uintP: P[Int] = P(CharIn("1234567890").repX(min = 1).!).map(_.toInt).opaque("uint")

    def doubleP: P[Double] = {
      val digits        = P(CharIn("1234567890").repX(min = 1))
      val exponent      = P(CharIn("eE") ~~ CharIn("+-").? ~~ digits )
      val fractional    = P("." ~~ digits )
      val integral      = P("0" | CharIn('1' to '9') ~~ digits.? )

      P( CharIn("+-").? ~~ integral ~~ fractional.? ~~ exponent.? ).!.map(_.toDouble).opaque("float")
    }

    {
      //this is required because fastparse's `flatMap` does not consume whitespace
      //I still don't know why `~ Pass` doesn't work, but `~~ white` does
      val white = P(CharIn(" \t\n").repX) //be sure to not use .rep, which would consume whitespace
      for {
        isBayes <- P("BAYES").map(_ => true) | P("MARKOV").map(_ => false) ~~ white
        numVars <- uintP ~~ white
        domains <- uintP.rep(exactly = numVars).map(_.toArray) ~~ white
        numFactors <- uintP ~~ white
        factorScopes <- (for{
          size <- uintP ~~ white
          vars <- uintP.rep(exactly = size).map(_.reverse)
        } yield vars).rep(exactly = numFactors) ~~ white
        factorValues <- (for{
          size <- uintP ~~ white
          vals <- doubleP.rep(exactly = size)
        } yield vals).rep(exactly = numFactors) ~~ white
        factors = factorScopes.zip(factorValues).map{case (sc,vals) => Factor(sc.toArray, vals.toArray)}
      } yield (Problem(factors.toIndexedSeq, domains, NormalD), isBayes)
    }
  }

  def fromUaiString(s: String): Problem = parseUAIProblem(new StringReader(s)).right.get
  def loadUaiFile(s: String): Either[String,Problem] = loadUaiFile(new File(s))
  def loadUaiFile(f: File): Either[String,Problem] = {
    val reader = new FileReader(f)
    try {
      parseUAIProblem(reader)
    } catch  {
      case e: Exception => Left("Error while reading file: \n " + e.toString)
    } finally {
      reader.close()
    }
  }

  def parseUAIProblem(in: InputStream): Either[String, Problem] = parseUAIProblem(new InputStreamReader(in))
  def parseUAIProblem(in: Reader): Either[String,Problem] = parseBayesOrMarkov(in).right.map(_._1)

  /** @return first: the parsed problem; second: true if the input was a bayeschen network ("BAYES"). */
  def parseBayesOrMarkov(in: Reader): Either[String,(Problem,Boolean)] = {
    val r = new BufferedReader(in)
    val lines = Iterator
      .continually(r.readLine)
      .takeWhile(_ != null)
      .map(_ + "\n")

    uaiParser.parseIterator(lines) match {
      case Success(v,_) => Right(v)
      case failure => Left(failure.toString)
    }
  }
}