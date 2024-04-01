module Haversine.Program

open System
open System.IO
open SystemTesting
open Diagnostics
open Timing
open Calculator

let generateHaversineData earthRadius =
    let n = 10_000_000
    let random = Random()
    let mutable generatedSum = 0.0
    let mutable json = ""

    printfn "Generating data"

    (use _ = new Timer(int64 (n * 4 * sizeof<float>) * 1L<b>, "write")
     let sum, coordinates = Generator.generateCoordinates (random.Next()) n earthRadius
     generatedSum <- sum
     json <- Json.toJson coordinates)

    printfn "Writing data"

    (use _ = new Timer(int64 (n * 4 * sizeof<float>) * 1L<b>, "write")
     File.WriteAllText($"{__SOURCE_DIRECTORY__}\..\..\input\data.json", json)
     File.WriteAllText($"{__SOURCE_DIRECTORY__}\..\..\input\expectedSum.data", generatedSum.ToString()))

let treatHaversineData earthRadius =
    Timer.BeginTime()
    let mutable json = ""

    let mutable expectedSum = 0.0
    printfn "Reading data"

    (use t = new Timer(0L<b>, "read")
     json <- File.ReadAllText($"{__SOURCE_DIRECTORY__}\..\..\input\data.json")

     expectedSum <-
         File.ReadAllText($"{__SOURCE_DIRECTORY__}\..\..\input\expectedSum.data")
         |> float

     t.CountBytes(int64 json.Length * 1L<b>))

    printfn "Deserializing data"
    let pairs = Json.fromJson json Json.toCoordinates

    printfn "Haversine sum"
    let sum = sumHaversineDistances earthRadius pairs

    printfn
        $"Number of pairs: {pairs.Length}\nActual sum: {sum}\nExpected sum: {expectedSum}\nDifference: {sum - expectedSum}"

    Timer.EndTime()
    Timer.Print()
   
[<EntryPoint>]
let main (argv : string []) =
    printfn $"CPU Frequency {estimateCpuFrequency ()} Hz"

    let earthRadius = 6372.8
    // generateHaversineData earthRadius
    treatHaversineData earthRadius

    0