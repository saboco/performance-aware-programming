module haversine.Program

open System
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
    Time.InitTime()
    let earthRadius = 6372.8
    let mutable json = ""
    
    // let n = 10_000_000
    // let random = Random()
    // let mutable generatedSum = 0.0
    
    //
    // printfn "Generating data"
    // (
    //     use t = new Time(int64 (n * 4 * sizeof<float>) * 1L<byte>, "write")
    //     let sum, coordinates = Generator.generateCoordinates (random.Next()) n earthRadius
    //     generatedSum <- sum
    //     json <- Json.toJson coordinates
    // )
    //
    // printfn "Writing data"
    // (
    //     use t = new Time(int64 (n * 4 * sizeof<float>) * 1L<byte>, "write")
    //     File.WriteAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\data.json", json)
    //     File.WriteAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\expectedSum.data", generatedSum.ToString())
    // )
    
    let mutable expectedSum = 0.0
    printfn "Reading data"
    (
        use t = new Time(0L<byte>, "read")
        json <- File.ReadAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\data.json")
        expectedSum <- File.ReadAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\expectedSum.data") |> float
        t.CountBytes(int64 json.Length * 1L<byte>)
    )
    
    printfn "Deserializing data"
    let pairs = Json.fromJson json
    
    printfn "Haversine sum"
    let sum = Haversine.sumHaversineDistances earthRadius pairs
    
    printfn $"Number of pairs: {pairs.Length}\nActual sum: {sum}\nExpected sum: {expectedSum}\nDifference: {sum-expectedSum}"
    Time.EndTime()
    Time.Print()
    0