module Tests

open NUnit.Framework
open RBTree
open RBTree.Tests.SampleData
open Shouldly

let createTree = SampleData.nineThousandNumbers |>
                    Seq.fold (fun (acc:RBTree<int>) (item:int) -> acc.Add(item)) RBTree<int>.Empty
                  
let createTree2 = SampleData.nineThousandNumbers |>
                    Seq.fold (fun (acc:RBTree2.RBTree<int>) (item:int) -> acc.Add(item)) RBTree2.RBTree<int>.Empty
                  

[<Test>]
let ``Insert has correct count``() = 
    let tree = createTree
    let tree2 = createTree2
    in tree.Count.ShouldBe 9000 ; tree2.Count.ShouldBe 9000

[<Test>]
let ``Delete one has correct count``() = 
    let tree = createTree
                |> fun i -> i.Remove SampleData.nineThousandNumbers.[0]
    let tree2 = createTree2
                |> fun i -> i.Remove SampleData.nineThousandNumbers.[0]
                          
    in tree.Count.ShouldBe (9000-1) ; tree2.Count.ShouldBe (9000-1)

[<Test>]
let ``Delete all creates an empty tree``() = 
    let tree = createTree
                |> fun t -> Seq.fold (fun (acc:RBTree<int>) (item:int) -> acc.Remove(item)) t SampleData.nineThousandNumbers
    let tree2 = createTree2
                |> fun t -> Seq.fold (fun (acc:RBTree2.RBTree<int>) (item:int) -> acc.Remove(item)) t SampleData.nineThousandNumbers
    in tree.Count.ShouldBe 0 ; tree2.Count.ShouldBe 0

open System.Collections.Generic
open System.Linq
[<Test>]
let ``Get enumerable creates ordered collection``() =
    let tree = createTree
    let orderedTree = (tree :> IEnumerable<int>)
    let mutable orderedArray = Array.zeroCreate tree.Count
    let mutable idx = 0
    for i in orderedTree do
        orderedArray.[idx] <- i
        idx <- idx+1
    let orderedTreeArray = orderedArray
    in 
    for i in 0..(orderedTreeArray.Length-2) do
        (orderedTreeArray.[i]).ShouldBeLessThan(orderedTreeArray.[i+1])

[<Test>]
let ``Get enumerable RBTree 2creates ordered collection``() =
    let tree = createTree2
    let orderedTree = (tree :> IEnumerable<int>)
    let mutable orderedArray = Array.zeroCreate tree.Count
    let mutable idx = 0
    for i in orderedTree do
        orderedArray.[idx] <- i
        idx <- idx+1
    let orderedTreeArray = orderedArray
    in 
    for i in 0..(orderedTreeArray.Length-2) do
        (orderedTreeArray.[i]).ShouldBeLessThan(orderedTreeArray.[i+1])