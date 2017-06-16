# Library for probabilistic inference

**This readme is not up-to-date!**

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
 
### New generator API
Problem generation is now separated into three phases:

 1. (hyper)-graph generation, resulting in a `N`-labeled graph generator`Generator[Graph[N]]`
 2. domain size generation via `Domainification[N]`
 3. parameter generation via `Parameterization[N]`
 
If you have decided upon each one, you can use

    import vultura.factor.generation._
    
    problemGenerator(Constant(graph.lattice(2 -> true, 2 -> true)), FixedDomainsSize(2), IIDValuedParam(Generator.uniform(0, 1)))
