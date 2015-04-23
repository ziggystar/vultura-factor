# Library for probabilistic inference

## Usage

Include the following in your `build.sbt` file:

    resolvers += "mirkwood" at "http://mirkwood.informatik.uni-ulm.de/mvn"

    libraryDependencies += "de.uni-ulm" %% "vultura-factor" % "22.3.0"

It is worth checking `build.sbt` for the current version.

## Fast Implementation of Sum/Product on Factors

See either `vultura.factor.Factor` companion for on-the-fly operations, or
`vultura.factor.SumProductTask` for operation specifications. The latter
can be used to persist lookup tables and temporary storage for better performance
with small operations.

## Inference Algorithms
### Exact

- Junction Tree
- Variable Elimination

### Approximate

- Mean field
- Belief Propagation
- Conditioned Belief Propagation
- Locally Conditioned Belief Propagation

## Parameter Learning

- maximum likelihood for data with hidden variables
