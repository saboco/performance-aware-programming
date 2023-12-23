module haversine.Program

open System
open System.Diagnostics
open System.IO

open haversine
open System.Runtime.InteropServices

module Native =
    let [<Literal>] dll = "rdtsc_debug"
    [<DllImport(dll, CallingConvention = CallingConvention.Cdecl)>]
    extern int64 Rdtsc()

    
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

let timeResult label totalTime start ``end`` =
    let frequency = Stopwatch.Frequency
    let elapsedTime = ``end`` - start
    let percentage = ((float elapsedTime) / (float totalTime)) * 100.0
    let elapsedSeconds = (float elapsedTime) * 1.0/(float frequency)
    $"{label} (%.2f{percentage}%%): {elapsedTime} ticks ({elapsedSeconds} s)"
    
let test () =
    let frequency = Stopwatch.Frequency
    let start = Stopwatch.GetTimestamp()
    let mutable sum = 0
    for i in 1..1_000_000_000 do
        sum <- sum + 1
            
    let ``end`` = Stopwatch.GetTimestamp()
    let elapsedTime = ``end`` - start
    let elapsedSeconds = (float elapsedTime) * 1.0/(float frequency)
    printfn $"[Frequency={frequency}]\nTicks: {elapsedTime}\nSeconds: {elapsedSeconds}"
    
let testsTimers () =
    let osFreq = Stopwatch.Frequency
    let cpuStart = Native.Rdtsc()
    let osStart = Stopwatch.GetTimestamp()
    let mutable osEnd = 0L
    let mutable osElapsed = 0L
    while osElapsed < osFreq do
        osEnd <- Stopwatch.GetTimestamp()
        osElapsed <- osEnd - osStart

    let cpuEnd = Native.Rdtsc()
    printfn $"OS Timer {osStart} -> {osEnd} = {osElapsed}"
    printfn $"OS seconds %.4f{float osElapsed/float osFreq}"
    printfn $"CPU Timer {cpuStart} -> {cpuEnd} = {cpuEnd - cpuStart}"
    
[<EntryPoint>]
let main (_: string []) =
    // let earthRadius = 6372.8
    // let n = 1_000_000
    //
    // printfn "Generating data"
    // let random = Random()
    // let start = InteropWithNative.Rdtsc()
    // let startGeneration = Stopwatch.GetTimestamp()
    // let generatedSum, coordinates = Generator.generateCoordinates (random.Next()) n earthRadius
    // let endGeneration = Stopwatch.GetTimestamp()
    // let startSerialization = Stopwatch.GetTimestamp()
    // let json = Json.toJson coordinates
    // let endSerialization = Stopwatch.GetTimestamp()
    //
    // let startWritingFile = Stopwatch.GetTimestamp()
    // File.WriteAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\data.json", json)
    // let endWritingFile = Stopwatch.GetTimestamp()
    //
    // printfn "Deserializing data"
    // let startReadingFile = Stopwatch.GetTimestamp()
    // let json = File.ReadAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\data.json")
    // let endReadingFile = Stopwatch.GetTimestamp()
    // let startDeserializing = Stopwatch.GetTimestamp()
    // let pairs = Json.fromJson json
    // let endDeserializing = Stopwatch.GetTimestamp()
    //
    // let startHaversineSum = Stopwatch.GetTimestamp()
    // let sum = Haversine.sumHaversineDistances earthRadius pairs
    // let endHaversineSum = Stopwatch.GetTimestamp()
    // let ``end`` = InteropWithNative.Rdtsc()
    //
    // let totalTime =
    //     (endGeneration - startGeneration)
    //     + (endSerialization - startSerialization)
    //     + (endWritingFile - startWritingFile)
    //     + (endReadingFile - startReadingFile)
    //     + (endDeserializing - startDeserializing)
    //     + (endHaversineSum - startHaversineSum)
    //         
    // let generationTime = timeResult "Generation" totalTime startGeneration endGeneration
    // let serializationTime = timeResult "Serialization" totalTime startSerialization endSerialization 
    // let writingTime =  timeResult "Writing file" totalTime startWritingFile endWritingFile
    // let readingTime = timeResult "Reading file" totalTime startReadingFile endReadingFile
    // let deserializationTime = timeResult "Deserialization" totalTime startDeserializing endDeserializing 
    // let haversineSumTime = timeResult "Haversine" totalTime startHaversineSum endHaversineSum        
    //
    // printfn $"Number of pairs: {pairs.Length}\nActual sum: {sum}\nExpected sum: {generatedSum}\nDifference: {sum-generatedSum}"
    // printfn $"Timing\nFrequency [{Stopwatch.Frequency} Hz]\n{generationTime}\n{serializationTime}\n{writingTime}\n{readingTime}\n{deserializationTime}\n{haversineSumTime}"
    // printfn $"TSC Timing: {``end``- start}"
    
    testsTimers () 
    0