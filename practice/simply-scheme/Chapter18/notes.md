Terms
-----
(tree: an abstract data type, collection of nodes)
(node: components of a tree each with a datum and children)
(root node: the base node of a tree)
(branch node: interior nodes of a tree)
(leaf node: nodes that contain no children)
(datum: a piece of data, in this context stored with a node)
(subtree: a tree within a tree)
(forest: a list of trees)
(mutual recursion: two functions where recursive definition includes call from one to the other and vice versa)
(shallow: a short tree)
(deep: a tall tree)
(tree recursion: any situation in which a procedure invocation results in more than one recursive call, even if there isn't an argument that's a tree)
(respecting the data abstraction: using the appropriate selectors and constructors that don't access the low-level representation of an abstract data type)
(data abstraction violation: failing to use the appropriate selectors and constructors)

Notes
-----
* {make-node, datum, children, leaf, cities}
* {in-tree?} used for showing efficient mutual recursion
