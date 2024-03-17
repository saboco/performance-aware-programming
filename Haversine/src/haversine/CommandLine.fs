module haversine.CommandLine

open Argu

type CacheArgs =
    | [<AltCommandLine("-2"); Unique>] PowerOfTwo

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | PowerOfTwo -> "use only power of 2 sizes"

type SystemArgs =
    | [<SubCommand; CliPrefix(CliPrefix.None)>] SystemInfo
    | [<SubCommand; CliPrefix(CliPrefix.None)>] Cache of CacheArgs option
    | [<SubCommand; CliPrefix(CliPrefix.None)>] PointerAnatomy
    | [<SubCommand; CliPrefix(CliPrefix.None)>] ReadWidthsTests

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | SystemInfo -> "Prints system information"
            | Cache _ -> "Execute tests to evaluate caches sizes. To stop the evaluations use <CTRL>+C"
            | PointerAnatomy -> "Shows some information about pointers"
            | ReadWiths -> "run read tests with different widths"
