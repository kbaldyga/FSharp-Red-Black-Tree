# Immutable red-black tree

[![Build Status](https://travis-ci.org/kbaldyga/Tree.svg)](https://travis-ci.org/kbaldyga/Tree)

# Deleting from Okasaki's red-black trees

See http://matt.might.net/articles/red-black-delete/ for explanation of the algorithm.

# Performance

* rb -> Stefan Kahrs' implementation
* rb2 -> Matt Might's implementation
* set -> Immutable AVL tree in C# (http://justinmchase.com/2011/12/13/immutable-avl-tree-in-c/)
* fset -> F# set
* immset -> https://www.nuget.org/packages/System.Collections.Immutable/

```bash
C:\Tree [master +0 ~1 -0]> .\RBTree.Console\bin\Release\RBTree.Console.exe
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
