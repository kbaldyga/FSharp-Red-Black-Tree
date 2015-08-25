namespace RedBlackTree

type color = Red | Black | BB | NB

[<NoEquality; NoComparison>]
type Tree<'t> = 
    | Empty // black leaf
    | EEmpty // double black leaf
    | Node of color * Tree<'t> * 't * Tree<'t>

module RedBlackTree =
    open System.Collections.Generic

    let toString (x:'a) = 
        match Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with | case, _ -> case.Name

    let hd = function 
        | Node(c, l, v, r) -> v
        | _ -> failwith "empty"

    let redden = function
        | Empty | EEmpty -> failwith "cannot redden empty"
        | Node(Red, _, _, _) as r -> r
        | Node(_, l, v, r ) -> Node(Red, l, v, r)

    // blacken' for insert
    let blacken' = function
        | Empty | EEmpty -> failwith "cannot blacken' empty"
        | Node(Black, _, _, _) as node -> node
        | Node(_, l, v, r) -> Node(Black, l, v, r)
    // blacken for delete
    let blacken = function
        | Node(Black, _, _, _) as node -> node
        | Node(BB, l, v, r) as node -> Node(Black, l, v, r)
        | Empty -> Empty
        | EEmpty -> Empty
        | _ -> failwith "cannot blacken Red"

    let isBB = function
        | EEmpty -> true
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
        | _ as s -> failwith ("cant blacker' " + (toString s))

    let redder' = function
        | EEmpty -> Empty
        | Node(c, l, v, r) -> Node(redder c, l, v, r)
        | _ as s -> failwith ("cant redder' " + (toString s))

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

    let rec lookup (comparer: IComparer<'T>) x = function
        | Empty -> None
        | Node(_, l, v, r) ->
            let comp = comparer.Compare(x, v)
            if comp = 0 then Some(v) 
            elif comp < 0 then lookup comparer x l
            else lookup comparer x r 
        | _ as s -> failwith ("cant lookup " + (toString s))

    let rec max = function
        | Empty -> failwith "no largest element"
        | Node(_, _, x, Empty) -> x
        | Node(_, _, x, r) -> max r
        | _ as s -> failwith ("cant max " + (toString s))

    let insert (comparer: IComparer<'T>) x s =
        let rec ins = function
            | Empty -> Node(Red, Empty, x, Empty)
            | Node(color, a, y, b) as s ->
                let comp = comparer.Compare(x, y)
                if comp < 0 then balance (color, (ins a), y, b)
                elif comp > 0 then balance (color, a, y, (ins b))
                else s
            | _ as s -> failwith ("cant insert " + (toString x))
        in blacken' (ins s)

    let rec removeMax = function
        | Empty -> failwith "no maximum to remove"
        | Node(_, _, _, Empty) as s -> remove s
        | Node(color, l, x, r)-> bubble color l x (removeMax r)
        | _ as s -> failwith ("cant removeMax " + (toString s))

    and remove = function
        | Empty -> Empty
        | Node(Red, Empty, _, Empty) -> Empty
        | Node(Black, Empty, _, Empty) -> EEmpty
        | Node(Black, Empty, _, Node(Red, a, x, b)) -> Node(Black, a, x, b)
        | Node(Black, Node(Red, a, x, b), _, Empty) -> Node(Black, a, x, b)
        | Node(color, l, y,  r) -> bubble color (removeMax l) (max l) r
        | _ as s -> failwith ("cant remove case" + (toString s))

    let delete (comparer: IComparer<'T>) x s = 
        let rec del = function
            | Empty -> Empty
            | Node(c, a, y, b) as s ->
                let comp = comparer.Compare(x, y)
                if comp < 0 then bubble c (del a) y b
                elif comp > 0 then bubble c a y (del b)
                else remove s
            | _ as s -> failwith ("cant delete " + (toString x))
        in blacken(del s)

    let rec count = function
        | Node(_, l, _, r) -> 1 + count l + count r
        | _ -> 0

    let rec exist (comparer: IComparer<'T>) item tree = match lookup comparer item tree with | None -> false | _ -> true
  
    let rec iter f = function
        | Node(_, l, v, r) -> iter f l ; f v ; iter f r
        | _ -> ()

    let copyToArray s (arr: _[]) i =
        let j = ref i 
        iter (fun x -> arr.[!j] <- x; j := !j + 1) s

    let toArray s = 
        let n = (count s) 
        let res = Array.zeroCreate n 
        copyToArray s res 0;
        res


    //--------------------------------------------------------------------------
    // Imperative left-to-right iterators, based on FSharp.Core/set.fs
    //--------------------------------------------------------------------------
    open System.Collections
    open System.Collections.Generic

    [<NoEquality; NoComparison>]
    type TreeIterator<'T> when 'T : comparison = { mutable stack: Tree<'T> list; mutable started : bool }

    let rec collapse stack =
        match stack with
        | []                        -> []
        | Empty                     :: rest 
        | EEmpty                    :: rest
            -> collapse rest
        | Node(_, Empty, v, Empty)  :: _    
        | Node(_, EEmpty, v, EEmpty):: _    
        | Node(_, Empty, v, EEmpty) :: _    
        | Node(_, EEmpty, v, Empty) :: _    
            -> stack
        | Node(c, l, v, r)          :: rest -> collapse (l::Node(c, Empty, v, Empty)::r::rest)
          
    let mkIterator s = { stack = collapse [s]; started = false }

    let notStarted() = raise (new System.InvalidOperationException("enumerationNotStarted"))
    let alreadyFinished() = raise (new System.InvalidOperationException("enumerationAlreadyFinished"))

    let current i =
        if i.started then
            match i.stack with
                | Node(_, Empty, v, Empty) :: _ -> v
                | []            -> alreadyFinished()
                | _             -> failwith "Tree iterator, unexpected stack for current"
        else
            notStarted()

    let rec moveNext i =
        if i.started then
            match i.stack with
                | Node(_, Empty, _, Empty) :: rest -> 
                    i.stack <- collapse rest;
                    not i.stack.IsEmpty 
                | [] -> false
                | _ -> failwith "Tree iterator, unexpected stack for moveNext"
        else
            i.started <- true;  // The first call to MoveNext "starts" the enumeration.
            not i.stack.IsEmpty 

    let mkIEnumerator s = 
        let i = ref (mkIterator s) 
        { new IEnumerator<_> with 
                member x.Current = current !i
            interface IEnumerator with 
                member x.Current = box (current !i)
                member x.MoveNext() = moveNext !i
                member x.Reset() = i :=  mkIterator s
            interface System.IDisposable with 
                member x.Dispose() = () }

    //--------------------------------------------------------------------------
    // Debugging purposes
    //--------------------------------------------------------------------------
    let rec toJson = function
        | Empty -> ""
        | EEmpty -> ""
        | Node(c, Empty, v, Empty) | Node(c, EEmpty, v, EEmpty) ->
            """ { "name" : " """ + (toString c) + " " + (v.ToString()) + """ ", "children": [] }"""
        | Node(c, Empty, v, (Node(_, _, _, _) as r))| Node(c, EEmpty, v, (Node(_, _, _, _) as r)) ->
            """{ "name": " """ + (toString c) + " " + (v.ToString()) + """", "children":[ """ +
             (toJson r) + "]}"   
        | Node(c, (Node(_,_,_,_) as l), v, Empty) | Node(c, (Node(_,_,_,_) as l), v, EEmpty) ->
            """{ "name": " """ + (toString c) + " " + (v.ToString()) + """", "children":[ """ +
             (toJson l) + "]}"
        | Node(c, l, v, r) ->
            """{ "name": " """ + (toString c) + " " + (v.ToString()) + """", "children":[ """ +
             (toJson l) + ","+ (toJson r)
            + "]}"

    let test  = 
        Seq.fold (fun acc item -> 
                    (insert LanguagePrimitives.FastGenericComparer<int> item acc)) 
                Empty [1..100]
                |> delete LanguagePrimitives.FastGenericComparer<int> 40 
                |> delete LanguagePrimitives.FastGenericComparer<int> 74
                |> toJson


open System.Collections
open System.Collections.Generic

[<Sealed>]
type RBTree<[<EqualityConditionalOn>]'T when 'T : comparison >(comparer:System.Collections.Generic.IComparer<'T>, tree: Tree<'T>) = 
    member internal set.Tree : Tree<'T> = tree
    member internal set.Comparer = comparer
    
    static member Empty : RBTree<'T> = new RBTree<'T>(LanguagePrimitives.FastGenericComparer<'T> , Empty)

    member s.Add(x) : RBTree<'T> = new RBTree<'T>(s.Comparer, RedBlackTree.insert s.Comparer x s.Tree)
    member s.Remove(x) : RBTree<'T> = new RBTree<'T>(s.Comparer, RedBlackTree.delete s.Comparer x s.Tree)
    member s.Count = RedBlackTree.count s.Tree
    member s.Contains(x) = RedBlackTree.exist s.Comparer x s.Tree
    member s.Find(x) = RedBlackTree.lookup x s.Tree
    member s.First = RedBlackTree.hd s.Tree

    interface IEnumerable<'T> with
        member s.GetEnumerator() = RedBlackTree.mkIEnumerator s.Tree

    interface IEnumerable with
        override s.GetEnumerator() = (RedBlackTree.mkIEnumerator s.Tree :> IEnumerator)

    interface ICollection<'T> with 
        member s.Add(x)      = ignore(x); raise (new System.NotSupportedException("ReadOnlyCollection"))
        member s.Clear()     = raise (new System.NotSupportedException("ReadOnlyCollection"))
        member s.Remove(x)   = ignore(x); raise (new System.NotSupportedException("ReadOnlyCollection"))
        member s.Contains(x) = RedBlackTree.exist s.Comparer x s.Tree
        member s.CopyTo(arr,i) = RedBlackTree.copyToArray s.Tree arr i
        member s.IsReadOnly = true
        member s.Count = RedBlackTree.count s.Tree