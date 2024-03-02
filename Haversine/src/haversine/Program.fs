module haversine.Program

#nowarn "9"

open System
open System.IO
open Diagnostics
open Timing

open haversine
open haversine.Memory
 
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
 
let generateHaversineData earthRadius =
    let n = 10_000_000
    let random = Random()
    let mutable generatedSum = 0.0
    let mutable json = ""
    
    printfn "Generating data"
    (
        use t = new Timer(int64 (n * 4 * sizeof<float>) * 1L<b>, "write")
        let sum, coordinates = Generator.generateCoordinates (random.Next()) n earthRadius
        generatedSum <- sum
        json <- Json.toJson coordinates
    )
    
    printfn "Writing data"
    (
        use t = new Timer(int64 (n * 4 * sizeof<float>) * 1L<b>, "write")
        File.WriteAllText($"{__SOURCE_DIRECTORY__}\..\..\input\data.json", json)
        File.WriteAllText($"{__SOURCE_DIRECTORY__}\..\..\input\expectedSum.data", generatedSum.ToString())
    )
    
let TreatHaversineData () =
    Timer.BeginTime()
    let earthRadius = 6372.8
    let mutable json = ""
    
    let mutable expectedSum = 0.0
    printfn "Reading data"
    (
        use t = new Timer(0L<b>, "read")
        json <- File.ReadAllText($"{__SOURCE_DIRECTORY__}\..\..\input\data.json")
        expectedSum <- File.ReadAllText($"{__SOURCE_DIRECTORY__}\..\..\input\expectedSum.data") |> float
        t.CountBytes(int64 json.Length * 1L<b>)
    )
    
    printfn "Deserializing data"
    let pairs = Json.fromJson json
    
    printfn "Haversine sum"
    let sum = Haversine.sumHaversineDistances earthRadius pairs
    
    printfn $"Number of pairs: {pairs.Length}\nActual sum: {sum}\nExpected sum: {expectedSum}\nDifference: {sum-expectedSum}"
    Timer.EndTime()
    Timer.Print()

[<EntryPoint>]
let main (_: string []) =
    printfn $"CPU Frequency {estimateCpuFrequency ()} Hz"
    ReadWriteTests.runCacheTests ()
    0