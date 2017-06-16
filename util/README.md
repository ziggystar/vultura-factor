# General purpose utlities - vultura.util

 - types to handle `Array[Array[T: Manifest]]` types identically to `Seq[Seq[T: Manifest]]`
 - some simple statistical pimps on numeric collections like mean, variance, sd
 - wheel of fortune sampling
 - randomly picking from collections
 - trait to memoize hashcodes `vultura.util.HashMemo`
 - fast mutable queue implementation in `vultura.util.MutableArrayQueue` 
 - building of integer isomorphisms/lookups in `vultura.util.ArrayIndex`

## Tree Decomposition

`vultura.util.TreeWidth` contains methods to find minimum degree and min-fill vertex ordering. 
Different heuristics can be easily added.

## Multi-dimensional Arrays

 - an optimized cross-product iterator, which can be used to build table-based multi-variate functions