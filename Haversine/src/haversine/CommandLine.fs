module haversine.CommandLine

open Argu

type SystemArgs =
    | [<SubCommand; CliPrefix(CliPrefix.None)>] SystemInfo
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Cache
    | [<SubCommand; CliPrefix(CliPrefix.None)>] PointerAnatomy
    | [<SubCommand; CliPrefix(CliPrefix.None)>]  ReadWithsTests
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | SystemInfo -> "Prints system information"
            | Cache -> "Execute tests to evaluate caches sizes. To stop the evaluations use <CTRL>+C"
            | PointerAnatomy -> "Shows some information about pointers"
            | ReadWiths -> "run read tests with different widths"
