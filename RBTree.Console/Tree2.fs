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

    let rec balance = function
            //Okasaki's original cases:
            | (Black, Node(Red, Node(Red, a, x, b), y, c), z, d)
            | (Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
            | (Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
            | (Black, a, x, Node(Red, b, y, Node(Red, c, z, d)))
                -> Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
            //Six cases for deletion:
            | (BB, Node(Red, Node(Red, a, x, b), y, c), z, d ) -> Node(Black, Node(Black, a, x, b), y, Node(Black, c, z, d))
            | (BB, Node(Red, a, x, Node(Red, b, y, c)), z, d) -> Node(Black, Node(Black, a, x, b), y, Node(Black, c, z, d))
            | (BB, a, x, Node(Red, Node(Red, b, y, c), z, d)) -> Node(Black, Node(Black, a, x, b), y, Node(Black, c, z, d))
            | (BB, a, x, Node(Red, b, y, Node(Red, c, z, d))) -> Node(Black, Node(Black, a, x, b), y, Node(Black, c, z, d))
            //
            | (BB, a, x, Node(NB, Node(Black, b, y, c), z, (Node(Black, _, _, _) as d))) ->
                Node(Black, Node(Black, a, x, b), y, (balance (Black, c,  z, (redden d))))
            | (BB, Node(NB, (Node(Black, _, _, _) as a), x, Node(Black, b, y, c)), z, d) ->
                Node(Black, balance (Black, redden a, x, b), y, Node(Black, c, z, d))
            | (c, l, x, r) -> Node(c, l, x, r)

    let bubble color l x  r = 
        match isBB(l) || isBB(r) with
        | true -> balance ((blacker color), (redder' l), x, (redder' r))
        | _ -> balance (color, l, x, r)

    let empty = Empty

    let rec lookup x = function
        | Empty -> None
        | Node(_, l, v, r) ->
            if x = v then Some(v) 
            elif x < v then lookup x l
            else lookup x r 

    let rec max = function
        | Empty -> failwith "no largest element"
        | Node(_, _, x, Empty) -> x
        | Node(_, _, x, r) -> max r

    let insert x s =
        let rec ins = function
            | Empty -> Node(Red, Empty, x, Empty)
            | Node(color, a, y, b) as s ->
                if x < y then balance (color, (ins a), y, b)
                elif x > y then balance (color, a, y, (ins b))
                else s
        in blacken (ins s)

    let rec removeMax = function
        | Empty -> failwith "no maximum to remove"
        | Node(_, _, _, Empty) as s -> remove s
        | Node(color, l, x, r)-> bubble color l x (removeMax r)

    and remove = function
        | Empty -> Empty
        | Node(Red, Empty, _, Empty) -> Empty
        | Node(Blac, Empty, _, Empty) -> EEmpty
        | Node(Black, Empty, _, Node(Red, a, x, b)) -> Node(Black, a, x, b)
        | Node(Black, Node(Red, a, x, b), _, Empty) -> Node(Black, a, x, b)
        | Node(color, l, y,  r) -> bubble color (removeMax l) (max l) r

    let delete x s = 
        let rec del = function
            | Empty -> Empty
            | Node(c, a, y, b) as s ->
                if x < y then bubble c (del a) y b
                elif x > y then bubble c a y (del b)
                else remove s
        in blacken(del s)


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

    let rec toJson = function
        | Empty -> ""
        | Node(c, Empty, v, Empty) ->
            """ { "name" : " """ + (toString c) + " " + (v.ToString()) + """ ", "children": [] }"""
        | Node(c, Empty, v, (Node(_, _, _, _) as r)) ->
            """{ "name": " """ + (toString c) + " " + (v.ToString()) + """", "children":[ """ +
             (toJson r) + "]}"   
        | Node(c, (Node(_,_,_,_) as l), v, Empty) ->
            """{ "name": " """ + (toString c) + " " + (v.ToString()) + """", "children":[ """ +
             (toJson l) + "]}"
        | Node(c, l, v, r) ->
            """{ "name": " """ + (toString c) + " " + (v.ToString()) + """", "children":[ """ +
             (toJson l) + ","+ (toJson r)
            + "]}"

    let test  = 
        Seq.fold (fun acc item -> 
                    (insert item acc)) 
                Empty [1..100]
                |> delete  40 
                |> toJson