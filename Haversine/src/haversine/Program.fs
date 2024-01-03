module haversine.Program

open System
open System.Diagnostics
open System.IO
open Diagnostics

open haversine
 
module Test =
    [<CLIMutable>]
    type Coordinates ={
        x0: float
        y0: float
        x1: float
        y1: float }

    [<CLIMutable>]
    type Pairs = {
        paris: Coordinates[]
    }
 
[<EntryPoint>]
let main (_: string []) =
    let earthRadius = 6372.8
    let n = 1_000_000
    
    printfn "Generating data"
    let random = Random()
    
    let generatedSum, coordinates = Generator.generateCoordinates (random.Next()) n earthRadius
    let json = Json.toJson coordinates
    
    use t = new Time("write")
    File.WriteAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\data.json", json)
    
    (t :> IDisposable).Dispose()
    
    printfn "Deserializing data"
    use t = new Time("read")
    let json = File.ReadAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\data.json")
    (t :> IDisposable).Dispose()
    
    let pairs = Json.fromJson json
    let sum = Haversine.sumHaversineDistances earthRadius pairs
    
    printfn $"Number of pairs: {pairs.Length}\nActual sum: {sum}\nExpected sum: {generatedSum}\nDifference: {sum-generatedSum}"
    Time.Print()
    testsTimers ()
    0