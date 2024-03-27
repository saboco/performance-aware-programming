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

type SystemArgs =
    | [<SubCommand; CliPrefix(CliPrefix.None)>] SystemInfo
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Cache of ParseResults<CacheArgs>
    | [<SubCommand; CliPrefix(CliPrefix.None)>] UnalignedPenalty
    | [<SubCommand; CliPrefix(CliPrefix.None)>] PointerAnatomy
    | [<SubCommand; CliPrefix(CliPrefix.None)>] ReadWidthsTests
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Chart of ParseResults<ChartArgs>

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | SystemInfo -> "Prints system information"
            | Cache _ ->
                "Execute tests to evaluate caches sizes. Optionaly you  Optionaly you can run only power of two sizes tests"
            | UnalignedPenalty -> "Execute read at unalinged cache lines"
            | PointerAnatomy -> "Shows some information about pointers"
            | ReadWidthsTests -> "run read tests with different widths"
            | Chart _ -> "read a csv file with x,y information and show a chart with that information"
