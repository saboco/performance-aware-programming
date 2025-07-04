module SystemTesting.Program

#nowarn "9"

open System
open System.IO
open Argu
open Diagnostics
open Timing
open Memory
open CommandLine
open Windows.Native

let checkStructure =
#if DEBUG
    true
#else
    false
#endif

[<EntryPoint>]
let main (argv: string[]) =
    printfn $"CPU Frequency {estimateCpuFrequency ()} Hz"
    let helpMessage = "\nusage example:\nSystemTesting.exe systeminfo"

    let errorHandler: IExiter =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<SystemArgs>(
            programName = "SystemTesting",
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
    | Cache args ->
        if args.Contains CacheArgs.PowerOfTwo then
            ReadWriteTests.runPowerOfTwoCacheTests ()
        else
            ReadWriteTests.runCacheTests ()
    | UnalignedPenalty -> ReadWriteTests.runUnalignedPenalty ()
    | PointerAnatomy -> PointerAnatomyTests.run ()
    | ReadWidthsTests -> ReadWriteTests.runReadWidthsTests ()
    | Chart args ->
        let log2 = args.Contains ChartArgs.Log
        let filePath = args.GetResult(ChartArgs.Path)

        File.ReadAllLines filePath
        |> Array.map (fun (l: string) ->
            let arr = l.Split(',')
            let x = Int32.Parse(arr[0])
            let y = Double.Parse(arr[1])
            let x = if log2 then Math.Log2 x else x
            x, y)
        |> Print.showChart
    | SetAssociativity -> SetAssociativity.runSetAssociativity ()
    | NonTemporalStores -> NonTemporalStores.runNonTemporalStores ()
    | Prefetching -> Prefetching.runPrefetching ()
    | FileRead args ->
        let path = args.GetResult(FileReadArgs.Path)
        FileRead.runFileRead path
    | FileReadAndSum args ->
        let path = args.GetResult(FileReadArgs.Path)
        FileRead.runFileReadAndSum path
    | FileReadAndSumOverlapped args ->
        let path = args.GetResult(FileReadArgs.Path)
        FileRead.runFileReadAndSumOverlapped path
    | MemoryMappedFileAndSumOverlapped args ->
        let path = args.GetResult(FileReadArgs.Path)
        FileRead.runMemoryMappedFileAndSumOverlapped path
    0
