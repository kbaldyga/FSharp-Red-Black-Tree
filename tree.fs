namespace RBTree

open Microsoft.FSharp.Reflection

type color = Red | Black

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
[<NoEquality; NoComparison>]
type Tree<'T> = 
    | Empty
    | Node of color * Tree<'T> * 'T * Tree<'T>

module Tree = 
    open System.Collections.Generic

    let toString (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, _ -> case.Name

    let hd = function 
        | Empty -> failwith "empty"
        | Node(c, l, v, r) -> v

    let left = function
        | Empty -> failwith "empty"
        | Node(_, l, _, _) -> l

    let right = function
        | Empty -> failwith "empty"
        | Node(_, _, _, r) -> r

    let rec exist item = function
        | Empty -> false
        | Node(_, l, v, r) -> 
            if item = v then true
            elif item < v then exist item l
            else exist item r

    let balance = function
        | (Black, Node(Red, Node(Red, a, x, b), y, c), z, d)
        | (Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
        | (Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
        | (Black, a, x, Node(Red, b, y, Node(Red, c, z, d)))
            -> Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
        | (c, l, x, r) -> Node(c, l, x, r)

    let insert (comparer: IComparer<'T>) item tree = 
        let rec ins = function
            | Empty -> Node(Red, Empty, item, Empty)
            | Node(c, l, v, r) as node ->
                let comp = comparer.Compare(item, v)
                if comp = 0 then node
                elif comp < 0 then balance(c, ins l, v, r)
                else balance(c, l, v, ins r)
        match ins tree with
            | Empty -> failwith "Should never return empty from an insert"
            | Node(_, l, v, r) -> Node(Black, l, v, r)

    let rec print = function
        | Empty -> " Empty "
        | Node(c, l, v, r) -> print r + "    " + c.ToString() + " " + v.ToString() + "    " + print l

    let rec count = function
        | Empty -> 0
        | Node(_, l, _, r) -> 1 + count l + count r

    let rec lookup key = function
        | Empty -> None
        | Node(_, l, v, r) when key < v -> lookup key l
        | Node(_, l, v, r) when key > v -> lookup key r
        | Node(_, l, v, r) as node when key = v -> Some(v)

    // "copied" from https://github.com/scala/scala/blob/2.11.x/src/library/scala/collection/immutable/RedBlackTree.scala
    let rec delete (comparer: IComparer<'T>) key tree =

        let blacken = function
            | Empty -> Empty
            | Node(Black, _, _, _ ) as b -> b
            | Node(Red, l, v, r) -> Node(Black, l, v, r)
        let redden = function
            | Empty -> Empty
            | Node(Black, l, v, r ) -> Node(Red, l, v, r)
            | Node(Red, _, _, _) as r -> r

        let balance item left right = 
             match left, right with
                | Node(Red, _, _, _), Node(Red, _, _, _) ->
                    Node(Red, blacken left, item, blacken right)
                | Node(Red, (Node(Red, lll, llv, llr) as ll), lv, lr), _ ->
                    Node(Red, blacken ll, lv, Node(Black, lr, item, right))
                | Node(Red, ll, lv, (Node(Red, lrl, lrv, lrr) as lr)), _ ->
                    Node(Red, Node(Black, ll, lv, lrl), lrv, Node(Black, lrr, item, right))
                | Node(Red, ll, lv, lr), _ ->
                    Node(Black, left, item, right)

                | Node(Black, _, _, _), Node(Red, rl, rv, (Node(Red, rrl, rrv, rrr) as rr)) ->
                    Node(Red, Node(Black, left, item, rl), rv, blacken rr)
                | Node(Black, _, _, _), Node(Red, (Node(Red, rll, rlv, rlr) as rl), rv, rr) ->
                    Node(Red, Node(Black, left, item, rll), rlv, Node(Black, rlr, rv, rr))
                | _ -> Node(Black, left, item, right)

        let subl = function
            | Node(Black, _, _, _) as tree -> redden tree
            | _ -> failwith "Defect: invariance violation; expected black"

        let balLeft item left right = 
            match left, right with
                | Node(Red, _, _, _), _ -> Node(Red, blacken left, item, right)
                | _, Node(Black, _, _, _) -> balance item left (redden right)
                | _, Node(Red, Node(Black, rll, rlv, rlr), rv, rr) ->
                    Node(Red, Node(Black, left, item, rll), rlv, balance rv rlr (subl rr))
                | _ -> failwith "balLeft Defect: invariance violation"
        let balRight item left right = 
            match left, right with
                | _, Node(Red, _, _, _) -> Node(Red, left, item, blacken right)
                | Node(Black, _, _, _), _ -> balance item (redden left) right
                | Node(Red, ll, lv, Node(Black, lrl, lrv, lrr)), _ ->
                    Node(Red, (balance lv (subl ll) lrl), lrv, Node(Black, lrr, item, right))
                | _ -> failwith "balRight Defect: invariance violation"

        let delLeft = function
            | Node(_, (Node(Black, _, _, _ ) as l), v, r) -> balLeft v (delete comparer key l) r
            | Node(_, l, v, r) -> Node(Red, delete comparer key l, v, r)
            | Empty -> failwith "can not delLeft on Empty"
        let delRight = function
            | Node(_, l, v, (Node(Black, _, _, _) as r)) -> balRight v l (delete comparer key r)
            | Node(_, l, v, r) -> Node(Red, l, v, delete comparer key r)
            | Empty -> failwith "can not delRight on Empty"

        let rec append left right =
                match left, right with
                | Empty, _ -> right
                | _, Empty -> left
                | Node(Red, l1, v1, r1), Node(Red, l2, v2, r2) ->
                    let bc = append r1 l2
                    match bc with
                        | Node(Red, bl, bv, br)   -> Node(Red, Node(Red, l1, v1, bl), bv, Node(Red, br, v2, r2))
                        | _ -> Node(Red, l1, v1, Node(Red, bc, v2, r2))
                | Node(Black, l1, v1, r1), Node(Black, l2, v2, r2) ->
                    let bc = append r1 l2
                    match bc with
                        | Node(Red, bl, bv, br) -> Node(Red, Node(Black, l1, v1, bl), bv, Node(Black, br, v2, r2))
                        | _ -> balLeft v1 l1 (Node(Black, bc, v2, r2))

                | _, Node(Red, l2, v2, r2) -> Node(Red, append left l2, v2, r2)
                | Node(Red, l1, v1, r1), _ -> Node(Red, l1, v1, append r1 right)

        match tree with
        | Empty -> Empty
        | Node(_, l, v, r) ->
            let comp = comparer.Compare(key, v)
            if comp = 0 then append l r
            elif comp > 0 then delLeft tree
            else delRight tree

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

//
//    let test = Seq.fold (fun acc item -> insert item acc) Empty [1..100]
//                |> delete 40 |> toJson

[<Sealed>]
type RBTree<[<EqualityConditionalOn>]'T when 'T : comparison >(comparer:System.Collections.Generic.IComparer<'T>, tree: Tree<'T>) = 
    member internal set.Tree : Tree<'T> = tree
    member internal set.Comparer = comparer
    
    static member Empty : RBTree<'T> = new RBTree<'T>(LanguagePrimitives.FastGenericComparer<'T> , Empty)

    member s.Add(x) : RBTree<'T> = new RBTree<'T>(s.Comparer, Tree.insert s.Comparer x s.Tree)
    member s.Remove(x) : RBTree<'T> = new RBTree<'T>(s.Comparer, Tree.delete s.Comparer x s.Tree)
    member s.Count = Tree.count s.Tree
    member s.Contains(x) = Tree.exist x s.Tree
    member s.Find(x) = Tree.lookup x s.Tree
    member s.First = Tree.hd s.Tree
