package vultura.fastfactors

import scala.util.Random
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
package object generators {
  trait FactorGenerator extends ((Seq[Int],IndexedSeq[Int],Random) => FastFactor) {
    def generate(variables: Array[Int], domains: Array[Int], random: Random): FastFactor
    def apply(v1: Seq[Int], v2: IndexedSeq[Int], v3: Random): FastFactor = generate(v1.toArray.sorted,v2.toArray,v3)
  }
  trait SimpleGenerator extends FactorGenerator {
    final def generate(variables: Array[Int], domains: Array[Int], random: Random): FastFactor =  {
      val entries: Int = variables.map(domains).product
      FastFactor(variables, generateValues(entries,random))
    }
    def generateValues(numValues: Int, random: Random): Array[Double]
  }

  implicit def fun2SimpleGenerator(f: ( Int, Random) => Array[Double]) = new SimpleGenerator {
    def generateValues(numValues: Int, random: Random): Array[Double] = f(numValues,random)
  }

  /** @return a neutral factor. */
  val maxEntropy: FactorGenerator = (entries: Int, random: Random) => Array.fill(entries)(1d/entries)

  /** @return a factor that is 1 for a random entry and `weight` for all others. */
  def clause(weight: Double, neutral: Double = 1d): FactorGenerator =
    (entries: Int, random: Random) => Array.fill(entries)(weight).updated(random.nextInt(entries),neutral)

  /** @return a factor that is 1 for all entries except one, which is 0. */
  def deterministicClause = clause(1d,0d)
  def sigmaClause(sigma: Double): FactorGenerator =
    (entries: Int, random: Random) =>
      Array.fill(entries)(math.exp(random.nextGaussian() * sigma)).updated(random.nextInt(entries),1d)

  def expGauss(sigma: Double = 1d, mean: Double = 0d): FactorGenerator =
    (entries: Int, random: Random) => Array.fill(entries)(math.exp(random.nextGaussian() * sigma + mean))

  def grid(width: Int, height: Int, domainSize: Int, factorGenerator: FactorGenerator, random: Random): Problem = {
    val variables: Map[(Int, Int), Int] = (for (x <- 0 until width; y <- 0 until height) yield (x, y)).zipWithIndex.toMap
    val domains = Array.fill(variables.size)(domainSize)
    val horizontalPairs =
      for(left <- variables.keys if left._1 + 1 < width)
      yield factorGenerator(Seq(left,(left._1 + 1,left._2)).map(variables),domains,random)
    val verticalPairs =
      for(above <- variables.keys if above._2 + 1 < height)
      yield factorGenerator(Seq(above,(above._1,above._2 + 1)).map(variables),domains,random)
    val singletons = variables.keys.map(v => factorGenerator(Seq(variables(v)),domains,random))
    Problem((singletons ++ horizontalPairs ++ verticalPairs)(collection.breakOut), domains, NormalD)

  }
  def randomK(numVariables: Int,
              numFactors: Int,
              factorSize: Int,
              domainSize: Int,
              factorGenerator: FactorGenerator,
              random: Random): Problem = {
    val domains = Array.fill(numVariables)(domainSize)
    def genFactorVariables: Array[Int] = Iterator
      .continually(Array.fill(factorSize)(random.nextInt(numVariables)))
      .filter(_.toSet.size == factorSize)
      .next()
    Problem(Array.fill(numFactors)(factorGenerator(genFactorVariables,domains,random)),domains,NormalD)
  }
  def treeK(numFactors: Int,
            k: Int,
            domainSize: Int,
            fgen: FactorGenerator,
            random: Random): Problem = {
    val numVars = numFactors * (k-1) + 1
    val domains = Array.fill(numVars)(domainSize)
    val factors = Seq.iterate((k,fgen(Seq.range(0,k),domains,random)),numFactors){case (nextVar,_) =>
      (nextVar + (k-1),fgen(random.nextInt(nextVar) +: Array.range(nextVar, nextVar + (k-1)),domains,random))
    }
    Problem(factors.map(_._2)(collection.breakOut),domains,NormalD)
  }

  def generateFromString(desc: String): Either[String,Long => Problem] = {
    import GeneratorParser._
    (parseAll(problemGen,desc): ParseResult[Long => Problem]) match {
      case Success(gen,_) => Right(gen)
      case NoSuccess(msg,_) => Left(msg)
    }
  }

  object GeneratorParser extends JavaTokenParsers{
    def named[A](name: String, p: Parser[A]): Parser[A] = opt(name ~ "=") ~> p
    def pInt: Parser[Int] = wholeNumber ^^ (_.toInt)
    def pDecimal: Parser[Double] = decimalNumber ^^ (_.toDouble)
    def problemGen: Parser[Long => Problem] = pRandomK | pGrid
    def pRandomK: Parser[Long => Problem] =
      "randomK(" ~>
        named("variables",pInt) ~ "," ~
        named("factors",pInt) ~ "," ~
        named("k",pInt) ~ "," ~
        named("domains",pInt) ~ "," ~
        named("potential",potential) <~ ")" ^^ {
          case v ~ _ ~ f ~ _ ~ k ~ _ ~ doms ~ _ ~ pot => (seed: Long) => randomK(v,f,k,doms,pot,new Random(seed))
        }
    def pGrid: Parser[Long => Problem] =
      "grid(" ~>
        named("width",pInt) ~ "," ~
        named("height",pInt) ~ "," ~
        named("domains",pInt) ~ "," ~
        named("potential",potential) <~ ")" ^^ {
        case w ~ _ ~ h ~ _ ~ d ~ _ ~ pots => (seed: Long) => grid(w,h,d,pots,new Random(seed))
      }
    def potential: Parser[FactorGenerator] = pFmaxEnt | pFexpGauss | pFdetClause | pFClause | pFSigmaClause
    def pFmaxEnt: Parser[FactorGenerator] = "max-entropy" ^^^ maxEntropy
    def pFexpGauss: Parser[FactorGenerator] = "expgauss(" ~> named("sigma",pDecimal) <~ ")" ^^ {sigma => expGauss(sigma)}
    def pFdetClause: Parser[FactorGenerator] = "det-clause" ^^^ deterministicClause
    def pFClause: Parser[FactorGenerator] = "clause(" ~> named("weight",pDecimal) <~ ")" ^^ {weight => clause(weight)}
    def pFSigmaClause: Parser[FactorGenerator] = "sigma-clause(" ~> named("sigma",pDecimal) <~ ")" ^^ {sigma => sigmaClause(sigma)}
  }

}
