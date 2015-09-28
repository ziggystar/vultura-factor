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

#### Belief Propagation
- `vultura.propagation.BP` fastest version for small factors
- `vultura.factor.inference.BeliefPropagation` slowest implementation; supports damping
- Conditioned Belief Propagation
- Locally Conditioned Belief Propagation

## Parameter Learning

- maximum likelihood for data with hidden variables

## Problem Generation

`vultura.factor.generation.Generator` implements a monadic type that enables the construction of random generators.

### Graph Structures

In package object `vultura.factor.generation.graph`, there are implementations for different generators for undirected graphs.
  
  - n-dimensional lattice with optional wrapping boundaries
  - complete graph
  - single cycles 
  - erdoes-renyi random graphs (constant edge appearance probability)

### Adding Potentials

In package object `vultura.factor.generation`:

 - use `addPottsFactors` to convert a hyper-graph to a problem with Potts factors; 
   positive parameters yield attractive/fero-magnetic interactions, 
   while negative values yield repulsive/anti-ferromagnetic interactions
 - add magnetic field with `withMagneticField` for problems with only binary variables


