# [pomaps][] [![Build Status](https://travis-ci.org/sgraf812/pomaps.svg?branch=master)](https://travis-ci.org/sgraf812/pomaps)

Reasonably fast maps (and possibly sets) based on keys satisfying [`PartialOrd`](https://hackage.haskell.org/package/lattices-1.6.0/docs/Algebra-PartialOrd.html#t:PartialOrd).

This package tries to load off as much work as possible to the excellent [`containers`](https://hackage.haskell.org/package/containers) library, in order to achieve acceptable performance.
The interface is kept as similar to [`Data.Map.{Strict/Lazy}](https://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Map-Strict.html) as possible, which is an excuse for somewhat lacking documentation.

`POMap`s basically store a decomposition of totally ordered chains (e.g. something `Map`s can handle). 
Functionality should be pretty much covered by the testsuite, but it's not battle-tested yet.
Also strictness is tricky to get right in a strictness-polymorphic setting, so it's entirely possible that the implementation is too lazy in some arguments.
So, if you fight with space leaks, let me know!

A rather naive implementation leads to `O(w*n*log n)` lookups, where `w` is the width of the decomposition (which should be the size of the biggest anti-chain).
This is enough for me at the moment to get things going, but there is room for improvement ([Sorting and Selection in Posets](https://arxiv.org/abs/0707.1532)).
Let me know if things are too slow and I'll see what I can do!

[pomaps]: https://github.com/sgraf812/pomaps
