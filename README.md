# Immutable red-black tree

[![Build Status](https://travis-ci.org/kbaldyga/FSharp-Red-Black-Tree.svg?branch=measure-tools)](https://travis-ci.org/kbaldyga/FSharp-Red-Black-Tree)

# Deleting from Okasaki's red-black trees

See http://matt.might.net/articles/red-black-delete/ for explanation of the algorithm.

# Performance

* rb -> Stefan Kahrs' algorithm
* rb2 -> Matt Might's algorithm
* set -> Immutable AVL tree in C# (http://justinmchase.com/2011/12/13/immutable-avl-tree-in-c/)
* fset -> F# set
* immset -> https://www.nuget.org/packages/System.Collections.Immutable/

```bash
C:\ [master +0 ~1 -0]> .\RBTree.Console.exe
rb  insert: 4000
rb2  insert: 4906
set insert: 4899
fset insert: 4766
immset insert: 5424

rb  contains: 623
rb2  contains: 694
set  contains: 590
fset  contains: 728
immset  contains: 688

rb  remove: 4549
rb2  remove: 4644
set  remove: 4383
fset  remove: 3680
immset  remove: 4432
```

Same run on a MacBook Pro

```bash
rb  insert: 2931
rb2  insert: 3555
set insert: 4646
fset insert: 3808
immset insert: 6486

rb  contains: 575
rb2  contains: 683
set  contains: 719
fset  contains: 586
immset  contains: 763

rb  remove: 3626
rb2  remove: 3851
set  remove: 4745
fset  remove: 3329
immset  remove: 5717
```
