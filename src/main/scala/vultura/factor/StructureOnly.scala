package vultura.factor

/** Implementation of [[ProblemStructure]]. */
case class StructureOnly(domains: Array[Int], scopeOfFactor: Array[Array[Int]]) extends ProblemStructure {
  override def equals(obj: scala.Any): Boolean = obj match {
    case ps: ProblemStructure => domains.deep == ps.domains.deep && scopeOfFactor.deep == ps.scopeOfFactor.deep
  }
  override val hashCode: Int = (domains.deep,scopeOfFactor.deep).hashCode
}
