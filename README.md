# [pomaps][] [![Build Status](https://travis-ci.org/sgraf812/pomaps.svg?branch=master)](https://travis-ci.org/sgraf812/pomaps)

Fast maps (and possibly sets) based on keys satisfying [`PartialOrd`](https://hackage.haskell.org/package/lattices-1.6.0/docs/Algebra-PartialOrd.html#t:PartialOrd).

This package tries to load off as much work as possible to the excellent [`containers`](https://hackage.haskell.org/package/containers) library, in order to achieve acceptable performance.

`POMap`s basically store a decomposition of totally ordered chains (e.g. something `Map`s can handle). This is still untested and a work in progress.

A rather naive implementation leads to `O(w*n*log n)` lookups, where `w` is the width of the decomposition (which should be the size of the biggest anti-chain).
This is enough for me at the moment to get things going, but there is room for improvement ([Sorting and Selection in Posets](https://arxiv.org/abs/0707.1532)).

[pomaps]: https://github.com/sgraf812/pomaps
