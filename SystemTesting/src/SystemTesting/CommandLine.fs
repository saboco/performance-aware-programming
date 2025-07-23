module SystemTesting.CommandLine

open Argu

type CacheArgs =
    | [<Unique>] PowerOfTwo

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | PowerOfTwo -> "Only use power of two sizes to test the caches"

type ChartArgs =
    | [<Unique; Mandatory>] Path of string
    | [<Unique; AltCommandLine("-l")>] Log

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "The path to the csv file"
            | Log -> "Whether or not to apply Log2 to x axis"
            
type FileReadArgs =
    | [<Unique; Mandatory>] Path of string
    
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Path _ -> "The path to the file to read"

type SystemArgs =
    | [<SubCommand; CliPrefix(CliPrefix.None)>] SystemInfo
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Cache of ParseResults<CacheArgs>
    | [<SubCommand; CliPrefix(CliPrefix.None)>] UnalignedPenalty
    | [<SubCommand; CliPrefix(CliPrefix.None)>] PointerAnatomy
    | [<SubCommand; CliPrefix(CliPrefix.None)>] ReadWidthsTests
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Chart of ParseResults<ChartArgs>
    | [<SubCommand; CliPrefix(CliPrefix.None)>] SetAssociativity
    | [<SubCommand; CliPrefix(CliPrefix.None)>] NonTemporalStores
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Prefetching
    | [<SubCommand; CliPrefix(CliPrefix.None)>] FileRead of ParseResults<FileReadArgs>
    | [<SubCommand; CliPrefix(CliPrefix.None)>] FileReadAndSum of ParseResults<FileReadArgs>
    | [<SubCommand; CliPrefix(CliPrefix.None)>] FileReadAndSumOverlapped of ParseResults<FileReadArgs>
    | [<SubCommand; CliPrefix(CliPrefix.None)>] MemoryMappedFileAndSumOverlapped of ParseResults<FileReadArgs>
    | [<SubCommand; CliPrefix(CliPrefix.None)>] MathTest

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | SystemInfo -> "Prints system information"
            | Cache _ ->
                "Execute tests to evaluate caches sizes. Optionally you can run only power of two sizes tests"
            | UnalignedPenalty -> "Execute read at unalinged cache lines"
            | PointerAnatomy -> "Shows some information about pointers"
            | ReadWidthsTests -> "run read tests with different widths"
            | Chart _ -> "read a csv file with x,y information and show a chart with that information"
            | SetAssociativity -> "test the cache set associativity"
            | NonTemporalStores -> "test the non temporal stores"
            | Prefetching -> "test the prefetching"
            | FileRead _ -> "test the file read"
            | FileReadAndSum _ -> "test the file read and sum the values"
            | FileReadAndSumOverlapped _ -> "test the file read and sum the values with overlapped reads"
            | MemoryMappedFileAndSumOverlapped _ -> "test the memory mapped file read and sum the values with overlapped reads"
            | MathTest -> "test some Math functions and compare precision with references values"
