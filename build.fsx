#I @"packages/FAKE/tools"
#r "FakeLib.dll"
#r "System.Xml.Linq"

open System
open System.IO
open System.Text
open Fake
open Fake.FileUtils
open Fake.MSTest
open Fake.NUnitCommon
open Fake.TaskRunnerHelper
open Fake.ProcessHelper

cd __SOURCE_DIRECTORY__

//--------------------------------------------------------------------------------
// Information about the project for Nuget and Assembly info files
//--------------------------------------------------------------------------------


let product = "RBTree"
let configuration = "Release"
let toolDir = "tools"
//--------------------------------------------------------------------------------
// Directories

let binDir = "bin"
let testOutput = "TestResults"

let nugetDir = binDir @@ "nuget"
let workingDir = binDir @@ "build"
let libDir = workingDir @@ @"lib\net45\"
let nugetExe = FullName @".nuget\NuGet.exe"

open Fake.RestorePackageHelper
Target "RestorePackages" (fun _ -> 
     "./RBTree.sln"
     |> RestoreMSSolutionPackages (fun p ->
         { p with
             OutputPath = "./packages"
             Retries = 4 })
 )

//--------------------------------------------------------------------------------
// Clean build results

Target "Clean" <| fun _ ->
    DeleteDir binDir

//--------------------------------------------------------------------------------
// Build the solution

Target "Build" <| fun _ ->

    !!"RBTree.sln"
    |> MSBuildRelease "" "Rebuild"
    |> ignore

//--------------------------------------------------------------------------------
// Tests targets
//--------------------------------------------------------------------------------

//--------------------------------------------------------------------------------
// Clean test output

Target "CleanTests" <| fun _ ->
    DeleteDir testOutput
//--------------------------------------------------------------------------------
// Run tests

open XUnit2Helper
Target "RunTests" <| fun _ ->  
    let nunitTestAssemblies = !! "**/*.Tests.dll"

    nunitTestAssemblies
    |> NUnit (fun p -> 
        {p with
            DisableShadowCopy = true; 
            OutputFile = testOutput + @"\NUnitTestResults.xml"})

Target "RunTestsMono" <| fun _ ->  
    let xunitTestAssemblies = !! "**/*.Tests.dll"

    mkdir testOutput

    let xunitToolPath = findToolInSubPath "xunit.console.exe" "packages/xunit.runner.console*/tools"
    printfn "Using XUnit runner: %s" xunitToolPath
    xUnit2
        (fun p -> { p with OutputDir = testOutput; ToolPath = xunitToolPath })
        xunitTestAssemblies

//--------------------------------------------------------------------------------
//  Target dependencies
//--------------------------------------------------------------------------------

// build dependencies
"Clean" ==> "RestorePackages" ==> "Build" 

// tests dependencies
"CleanTests" ==> "RunTests"


Target "All" DoNothing
"RunTests" ==> "All"

Target "AllTests" DoNothing 
"RunTests" ==> "AllTests"

RunTargetOrDefault "RunTestsMono"
