namespace RBTree2

type color = Red | Black | BB | NB

type Tree<'t> = 
    | Empty // black leaf
    | EEmpty // double black leaf
    | Node of color * Tree<'t> * 't * Tree<'t>


module Tree =
    open System.Collections.Generic

    let toString (x:'a) = 
        match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with | case, _ -> case.Name

    let redden = function
        | Empty | EEmpty -> failwith "cannot redden empty"
        | Node(Red, _, _, _) as r -> r
        | Node(_, l, v, r ) -> Node(Red, l, v, r)

    let blacken = function
        | Empty | EEmpty -> failwith "cannot blacken empty"
        | Node(Black, _, _, _) as node -> node
        | Node(_, l, v, r) -> Node(Black, l, v, r)

    let isBB = function
        | Node(BB, _, _, _) -> true
        | _ -> false

    let blacker = function
        | NB -> Red
        | Red -> Black
        | Black -> BB
        | BB -> failwith "cannot blacker, is double black already"

    let redder = function
        | NB -> failwith "cannot redder, not black enough"
        | Red -> NB
        | Black -> Red
        | BB -> Black


    let blacker' = function
        | Empty -> EEmpty
        | Node(c, l, v, r) -> Node(blacker c, l, v, r)

    let redder' = function
        | EEmpty -> Empty
        | Node(c, l, v, r) -> Node(redder c, l, v, r)

    let balance tree = 
        match tree with
            //Okasaki's original cases:
            | (Black, Node(Red, Node(Red, a, x, b), y, c), z, d)
            | (Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
            | (Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
            | (Black, a, x, Node(Red, b, y, Node(Red, c, z, d)))
                -> Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
            //Six cases for deletion:

            //balance BB (T R (T R a x b) y c) z d = T B (T B a x b) y (T B c z d)
            //balance BB (T R a x (T R b y c)) z d = T B (T B a x b) y (T B c z d)
            //balance BB a x (T R (T R b y c) z d) = T B (T B a x b) y (T B c z d)
            //balance BB a x (T R b y (T R c z d)) = T B (T B a x b) y (T B c z d)
            //
            //balance BB a x (T NB (T B b y c) z d@(T B _ _ _)) 
            //    = T B (T B a x b) y (balance B c z (redden d))
            //balance BB (T NB a@(T B _ _ _) x (T B b y c)) z d
            //    = T B (balance B (redden a) x b) y (T B c z d)

            | (c, l, x, r) -> Node(c, l, x, r)


//
// -- `balance` rotates away coloring conflicts:
//balance :: Color -> RBSet a -> a -> RBSet a -> RBSet a
//
// -- Okasaki's original cases:
//balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
//balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
//balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
//balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
//
// -- Six cases for deletion:
//balance BB (T R (T R a x b) y c) z d = T B (T B a x b) y (T B c z d)
//balance BB (T R a x (T R b y c)) z d = T B (T B a x b) y (T B c z d)
//balance BB a x (T R (T R b y c) z d) = T B (T B a x b) y (T B c z d)
//balance BB a x (T R b y (T R c z d)) = T B (T B a x b) y (T B c z d)
//
//balance BB a x (T NB (T B b y c) z d@(T B _ _ _)) 
//    = T B (T B a x b) y (balance B c z (redden d))
//balance BB (T NB a@(T B _ _ _) x (T B b y c)) z d
//    = T B (balance B (redden a) x b) y (T B c z d)
//
//balance color a x b = T color a x b
//
// -- `bubble` "bubbles" double-blackness upward:
//bubble :: Color -> RBSet a -> a -> RBSet a -> RBSet a
//bubble color l x r
// | isBB(l) || isBB(r) = balance (blacker color) (redder' l) x (redder' r)
// | otherwise          = balance color l x r
//
//
//
//
// -- Public operations --
//
//empty :: RBSet a
//empty = E
//
//
//member :: (Ord a) => a -> RBSet a -> Bool
//member x E = False
//member x (T _ l y r) | x < y     = member x l
//                     | x > y     = member x r
//                     | otherwise = True
//
//max :: RBSet a -> a
//max E = error "no largest element"
//max (T _ _ x E) = x
//max (T _ _ x r) = max r
//
//
// -- Insertion:
//
//insert :: (Ord a) => a -> RBSet a -> RBSet a                    
//insert x s = blacken (ins s) 
// where ins E = T R E x E
//       ins s@(T color a y b) | x < y     = balance color (ins a) y b
//                             | x > y     = balance color a y (ins b)
//                             | otherwise = s
//
//
// -- Deletion:
//
//delete :: (Ord a,Show a) => a -> RBSet a -> RBSet a
//delete x s = blacken(del s)
// where del E = E
//       del s@(T color a y b) | x < y     = bubble color (del a) y b
//                             | x > y     = bubble color a y (del b)
//                             | otherwise = remove s
//
//remove :: RBSet a -> RBSet a
//remove E = E
//remove (T R E _ E) = E
//remove (T B E _ E) = EE
//remove (T B E _ (T R a x b)) = T B a x b
//remove (T B (T R a x b) _ E) = T B a x b
//remove (T color l y r) = bubble color l' mx r 
// where mx = max l
//       l' = removeMax l
//
//removeMax :: RBSet a -> RBSet a
//removeMax E = error "no maximum to remove"
//removeMax s@(T _ _ _ E) = remove s
//removeMax s@(T color l x r) = bubble color l x (removeMax r)
//
// -- Conversion:
//
//toAscList :: RBSet a -> [a]
//toAscList E = []
//toAscList (T _ l x r) = (toAscList l) ++ [x] ++ (toAscList r)
//
// -- Equality
//
//instance Eq a => Eq (RBSet a) where
// rb == rb' = (toAscList rb) == (toAscList rb')
//
