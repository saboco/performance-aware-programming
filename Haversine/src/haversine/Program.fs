module haversine.Program

#nowarn "9"

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open Argu
open Diagnostics
open Timing
open haversine
open haversine.Memory
open haversine.CommandLine
 
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


let checkStructure =
#if DEBUG
    true
#else
    false
#endif

[<EntryPoint>]
let main (argv: string []) =
    printfn $"CPU Frequency {estimateCpuFrequency ()} Hz"
    let helpMessage =
            "\nusage example:\nhaversine.exe systeminfo"
            
    let errorHandler: IExiter =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )
            
    let parser =
        ArgumentParser.Create<SystemArgs>(
            programName = "haversine",
            errorHandler = errorHandler,
            checkStructure = checkStructure,
            helpTextMessage = helpMessage
        )
    
    let results = parser.ParseCommandLine argv
    
    if results.IsUsageRequested then
        let usage = parser.PrintUsage()
        printfn $"{usage}"
        
    let command = results.GetSubCommand()
    
    match command with
    | SystemInfo ->
        let mutable systemInfo = Unchecked.defaultof<SystemInfo>
        GetSystemInfo(&&systemInfo)
        printfn $"%A{systemInfo}"
    | Cache ->
        ReadWriteTests.runCacheTests ()
    | PointerAnatomy -> PointerAnatomyTests.run()
    | ReadWithsTests -> ReadWriteTests.runReadWithsTests()
    0