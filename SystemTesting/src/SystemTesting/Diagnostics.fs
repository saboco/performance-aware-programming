module SystemTesting.Diagnostics

open System
open System.Collections.Generic
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Measure>]
type ms

[<Measure>]
type cycle

[<Measure>]
type b

[<Measure>]
type Kb

[<Measure>]
type Mb

[<Measure>]
type Gb

[<Measure>]
type s

let bytesPerMegabyte: float<b Mb^-1> = 1024.0 * 1024.0<b / Mb>
let bytesPerKilobyte: float<b Kb^-1> = 1024.0<b / Kb>
let megabytesPerGigabyte: float<Mb Gb^-1> = 1024.0<Mb / Gb>

let bytesToMegabytes (x: int64<b>) = ((float x) * 1.0<b>) / bytesPerMegabyte
let bytesToKilobytes (x: int64<b>) = ((float x) * 1.0<b>) / bytesPerKilobyte
let megabytesToGigabytes (x: float<Mb>) = x / megabytesPerGigabyte

module Print =
    open Plotly.NET

    let showChart dataPoints =
        let chartLayout = Layout.init (Width = 1200, Height = 900)

        Chart.Line(xy = dataPoints, ShowMarkers = true)
        |> Chart.withLayout chartLayout
        |> Chart.withLineStyle (Width = 2., Dash = StyleParam.DrawingStyle.Dot)
        |> Chart.withTitle "Cache throughput"
        |> Chart.withXAxisStyle "Kb"
        |> Chart.withYAxisStyle "GB/s"
        |> Chart.show

    let showMultiLineChart (title: string) (dataPoints: (string * (#IConvertible * #IConvertible) seq) seq) =
        let chartLayout = Layout.init (Width = 1200, Height = 900)

        dataPoints
        |> Seq.map (fun (name, dp) -> Chart.Line(xy = dp, ShowMarkers = true, Name = name))
        |> Chart.combine
        |> Chart.withLayout chartLayout
        |> Chart.withLineStyle (Width = 2., Dash = StyleParam.DrawingStyle.Dot)
        |> Chart.withTitle title
        |> Chart.withXAxisStyle "Kb"
        |> Chart.withYAxisStyle "GB/s"
        |> Chart.show

module Native =
    [<Literal>]
    let rdtsc_dll = "rdtsc_debug"

    [<Literal>]
    let windows_metrics_dll = "windows_metrics_debug"

    [<Literal>]
    let page_fault_probe = "page_fault_probe_debug"

    [<DllImport(rdtsc_dll, CallingConvention = CallingConvention.StdCall)>]
    extern int64 Rdtsc()

    [<DllImport(windows_metrics_dll, CallingConvention = CallingConvention.StdCall)>]
    extern void InitializeMetrics()

    [<DllImport(windows_metrics_dll, CallingConvention = CallingConvention.StdCall)>]
    extern UInt64 ReadPageFaultCount()

    [<UnmanagedFunctionPointer(CallingConvention.StdCall)>]
    type PutResult = delegate of UInt64 * UInt64 * UInt64 * UInt64 -> unit

    [<DllImport(page_fault_probe, CallingConvention = CallingConvention.StdCall)>]
    extern void ProbePageFaults(UInt64 pageCount, PutResult putResult)

    [<DllImport(page_fault_probe, CallingConvention = CallingConvention.StdCall)>]
    extern void ProbePageFaultsHeapAllocation(UInt64 pageCount, PutResult putResult)

/// Utilities to the execution of a function
/// Supports imbricated timing
module Timing =
    let inline rdTsc () : int64<cycle> = Native.Rdtsc() * 1L<cycle>

    [<Struct>]
    type TimeInfo =
        { ElapsedCyclesInclusive: int64<cycle>
          ElapsedCyclesExclusive: int64<cycle>
          ByteCount: int64<b>
          HitCount: int
          ParentIndex: string
          Label: string }

    let secondsFromCpuTime (cpuFrequency: int64) (cpuTime: int64<cycle>) =
        (float cpuTime) / (float cpuFrequency) * 1.0<s>

    let bandwidth (bytes: int64<b>) (seconds: float<s>) =
        ((float bytes) * 1.0<b>) / bytesPerMegabyte / megabytesPerGigabyte / seconds

    let sprintTimeInfo (cpuFrequency: int64) (totalCycles: int64<cycle>) (timeInfo: TimeInfo) =
        let percentage =
            ((float timeInfo.ElapsedCyclesExclusive) / (float totalCycles)) * 100.0

        let elapsedSeconds = secondsFromCpuTime cpuFrequency timeInfo.ElapsedCyclesExclusive

        let sprint =
            $"{timeInfo.Label} [{timeInfo.HitCount}] {timeInfo.ElapsedCyclesExclusive} %.3f{elapsedSeconds}s "

        let childrenSprint =
            if timeInfo.ElapsedCyclesExclusive <> timeInfo.ElapsedCyclesInclusive then
                let percentageWithChildren =
                    ((float timeInfo.ElapsedCyclesInclusive) / (float totalCycles)) * 100.0

                $"(%.2f{percentage}%%, %.2f{percentageWithChildren}%% w/children)"
            else
                $"(%.2f{percentage}%%)"

        let dataThroughput =
            if timeInfo.ByteCount > 0L<b> then
                let seconds = (float timeInfo.ElapsedCyclesInclusive) / (float cpuFrequency)
                let megabytes = (bytesToMegabytes timeInfo.ByteCount)
                let throughput = (megabytesToGigabytes megabytes) / seconds
                $", %.3f{megabytes}Mb, %.2f{throughput}Gb/s"
            else
                ""

        $"{sprint}{childrenSprint}{dataThroughput}"

    let printTimeInfos (cpuFrequency: int64) totalProgramCycles (timeInfos: Dictionary<string, TimeInfo>) =
        printfn $"Total time: {(float totalProgramCycles) / (float cpuFrequency) * 1000.0} ms @ {cpuFrequency / 1000_000L} MHz"

        for timeInfo in timeInfos.Values do
            sprintTimeInfo cpuFrequency totalProgramCycles timeInfo |> printfn "%s"

    let testsTimers (timeToWait: int64<ms>) =
        let osFreq = Stopwatch.Frequency
        let osStart = Stopwatch.GetTimestamp()
        let osWaitTime = osFreq * timeToWait / 1000L<ms>
        let mutable osEnd = 0L
        let mutable osElapsed = 0L
        let cpuStart = Native.Rdtsc()

        while osElapsed < osWaitTime do
            osEnd <- Stopwatch.GetTimestamp()
            osElapsed <- osEnd - osStart

        let cpuEnd = Native.Rdtsc()
        let cpuElapsed = cpuEnd - cpuStart
        let cpuFrequency = osFreq * cpuElapsed / osElapsed

        printfn $"OS Timer {osStart} -> {osEnd} = {osElapsed}"
        printfn $"OS seconds %.4f{float osElapsed / float osFreq}"
        printfn $"CPU Frequency {cpuStart} -> {cpuEnd} = @ {cpuFrequency} Hz"

    let testPagesFaults () =
        Native.InitializeMetrics()
        let startPageFaults = Native.ReadPageFaultCount()
        let arr: Int64[] = Array.create<Int64> (2 * 4096) 1L
        let i = arr[4097]
        let endPageFaults = Native.ReadPageFaultCount()
        printfn $"Page Faults[{i}] {endPageFaults - startPageFaults}"

    let estimateCpuFrequency () =
        let timeToWait = 10L<ms>
        let cpuStart = Native.Rdtsc() // strange bug: this has to be called before Stopwatch.GetTimestamp() or the result will be wrong... it is weird it does not happen on the testTimers function above
        let osFreq = Stopwatch.Frequency
        let osStart = Stopwatch.GetTimestamp()
        let osWaitTime = osFreq * timeToWait / 1000L<ms>
        let mutable osEnd = 0L
        let mutable osElapsed = 0L

        while osElapsed < osWaitTime do
            osEnd <- Stopwatch.GetTimestamp()
            osElapsed <- osEnd - osStart

        let cpuEnd = Native.Rdtsc()
        let cpuElapsed = cpuEnd - cpuStart
        let cpuFrequency = osFreq * cpuElapsed / osElapsed
        cpuFrequency

#if FULL_PROFILING
    type Timer([<Optional; DefaultParameterValue(0L<b>)>] bytesCount: int64<b>, [<CallerMemberName; Optional; DefaultParameterValue("")>] callerName: string, [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int) =

        let startCycles = rdTsc ()
        let key = $"{line}_{callerName}"
        let mutable disposed = false
        let mutable bytesCount = bytesCount

        let parentIndex =
            let parentIndex = Timer.CurrentIndex
            Timer.CurrentIndex <- key
            parentIndex

        let initDefault () =
            let times: Dictionary<_, _> = Timer.Times

            if times.ContainsKey key |> not then
                times[key] <-
                    { ElapsedCyclesInclusive = 0L<cycle>
                      ElapsedCyclesExclusive = 0L<cycle>
                      Label = callerName
                      ByteCount = 0L<b>
                      ParentIndex = parentIndex
                      HitCount = 0 }

        do initDefault ()

        let oldElapsedInclusive =
            if Timer.Times.ContainsKey parentIndex then
                Timer.Times[parentIndex].ElapsedCyclesInclusive
            else
                0L<cycle>

        static let mutable startTotalCycles = 0L<cycle>
        static let mutable endTotalCycles = 0L<cycle>

        static member val CPUFrequency = estimateCpuFrequency ()
        static member val CurrentIndex: string = "" with get, set
        static member val Times = Dictionary<string, TimeInfo>()

        static member BeginTime() = startTotalCycles <- rdTsc ()
        static member EndTime() = endTotalCycles <- rdTsc ()

        static member Print() =
            let totalProgramCycles = endTotalCycles - startTotalCycles
            printTimeInfos Timer.CPUFrequency totalProgramCycles Timer.Times

        member _.CountBytes(bytes: int64<b>) = bytesCount <- bytesCount + bytes

        interface IDisposable with

            member _.Dispose() =
                if not disposed then
                    disposed <- true

                    let elapsedCycles = rdTsc () - startCycles
                    Timer.CurrentIndex <- parentIndex

                    if Timer.Times.ContainsKey parentIndex then
                        let parentTimeInfo = Timer.Times[parentIndex]

                        Timer.Times[parentIndex] <-
                            { parentTimeInfo with
                                ElapsedCyclesExclusive = parentTimeInfo.ElapsedCyclesExclusive - elapsedCycles }

                    let timeInfo = Timer.Times[key]

                    Timer.Times[key] <-
                        { timeInfo with
                            HitCount = timeInfo.HitCount + 1
                            ByteCount = timeInfo.ByteCount + bytesCount
                            ElapsedCyclesInclusive = oldElapsedInclusive + elapsedCycles
                            ElapsedCyclesExclusive = timeInfo.ElapsedCyclesExclusive + elapsedCycles }
#else
    type Timer([<Optional; DefaultParameterValue(0L<b>)>] _bytesCount: int64<b>, [<CallerMemberName; Optional; DefaultParameterValue("")>] _callerName: string, [<CallerLineNumber; Optional; DefaultParameterValue(0)>] _line: int) =
        static let mutable startTotalCycles = 0L<cycle>
        static let mutable endTotalCycles = 0L<cycle>

        static member val CPUFrequency = estimateCpuFrequency ()
        static member val Times = Dictionary<string, TimeInfo>()

        static member BeginTime() = startTotalCycles <- rdTsc ()
        static member EndTime() = endTotalCycles <- rdTsc ()

        member _.CountBytes(_: int64<b>) = ()

        static member Print() =
            let totalProgramCycles = endTotalCycles - startTotalCycles
            printTimeInfos Timer.CPUFrequency totalProgramCycles Timer.Times

        interface IDisposable with
            member _.Dispose() = ()
#endif

module Repetition =
    type RepetitionTestResult =
        { CPUTime: int64<cycle>
          BytesCount: int64<b>
          MemPageFaults: uint64 }

        static member Empty =
            { CPUTime = 0L<cycle>
              BytesCount = 0L<b>
              MemPageFaults = 0UL }

    type RepetitionsResult =
        { Count: int64
          CPUFreq: int64
          Total: RepetitionTestResult
          Min: RepetitionTestResult
          Max: RepetitionTestResult }

    let private printResult label cpuFreq count (result: RepetitionTestResult) =
        let seconds = Timing.secondsFromCpuTime cpuFreq (result.CPUTime / count)
        let bandwidth = Timing.bandwidth (result.BytesCount / count) seconds

        printf $"{label}{result.CPUTime} (%.3f{seconds * 1000.0}ms) %.3f{bandwidth}gb/s"

        if result.MemPageFaults > 0UL then
            printf $" PF: %i{result.MemPageFaults} (%0.4f{(float result.BytesCount) / (float result.MemPageFaults * 1024.0)}k/fault)"
        else
            printf "                                               "

    let print (results: RepetitionsResult) =
        printResult "Min: " results.CPUFreq 1 results.Min
        printfn ""
        printResult "Max: " results.CPUFreq 1 results.Max
        printfn ""
        printResult "Avg: " results.CPUFreq results.Count results.Total
        printfn ""

    let repeat printProgression (secondsToTry: int64<s>) (fn: unit -> int64<b>) =
        let cpuFreq = Timing.estimateCpuFrequency ()
        Native.InitializeMetrics()
        let timeToTry = cpuFreq * 1L<cycle / s> * secondsToTry

        let mutable repetitionsResults =
            { Count = 0L
              CPUFreq = cpuFreq
              Total = RepetitionTestResult.Empty
              Max = RepetitionTestResult.Empty
              Min =
                { RepetitionTestResult.Empty with
                    CPUTime = Int64.MaxValue * 1L<cycle> } }

        let mutable testStartedAt = Timing.rdTsc ()

        while (Timing.rdTsc () - testStartedAt) < timeToTry do
            let startPageFaults = Native.ReadPageFaultCount()
            let readStart = Timing.rdTsc ()
            let bytesCount = fn () // function under test
            let readEnd = Timing.rdTsc ()
            let endPageFaults = Native.ReadPageFaultCount()

            let elapsed = readEnd - readStart

            let currentTestResult =
                { CPUTime = elapsed
                  BytesCount = bytesCount
                  MemPageFaults = endPageFaults - startPageFaults }

            let min =
                if elapsed < repetitionsResults.Min.CPUTime then
                    testStartedAt <- Timing.rdTsc ()
                    currentTestResult
                else
                    repetitionsResults.Min

            let max =
                if elapsed > repetitionsResults.Max.CPUTime then
                    currentTestResult
                else
                    repetitionsResults.Max

            let total =
                { CPUTime = repetitionsResults.Total.CPUTime + currentTestResult.CPUTime
                  BytesCount = repetitionsResults.Total.BytesCount + currentTestResult.BytesCount
                  MemPageFaults = repetitionsResults.Total.MemPageFaults + currentTestResult.MemPageFaults }

            repetitionsResults <-
                { repetitionsResults with
                    Count = repetitionsResults.Count + 1L
                    Min = min
                    Max = max
                    Total = total }

            if printProgression then
                printResult "" cpuFreq 1 currentTestResult
                let struct (_, top) = Console.GetCursorPosition()
                Console.SetCursorPosition(0, top)

        repetitionsResults

/// Utilities to test the precision of a function compared to a reference function
module Precision =
    type PrecisionResult =
        { Max: double
          Min: double
          Name: string }

    type PrecisionTest =
        { ReferenceFn: double -> double
          Fn: double -> double
          Range: double * double
          Name: string }

    type Sample = { Input: double; Output: double }

    type PrecisionTestWithSamples =
        { Name: string
          Fn: double -> double
          Samples: Sample[] }

    let private nextDouble =
        let random = Random()

        let nextDouble lowerBound upperBound : double =
            let rDouble = random.NextDouble()
            let rRangeDouble = rDouble * upperBound + (1.0 - rDouble) * lowerBound
            rRangeDouble

        nextDouble

    let private generateSamples (referenceFunction: double -> double) (inputs: double[]) =
        inputs
        |> Array.map (fun input ->
            { Input = input
              Output = referenceFunction input })

    let compareSamples inspectFn (test: PrecisionTestWithSamples) =

        let folder (state: PrecisionResult) (sample: Sample) =
            let result = test.Fn sample.Input
            let diff = result - sample.Output

            inspectFn test.Name sample result diff

            { Min = Math.Min(state.Min, diff)
              Max = Math.Max(state.Max, diff)
              Name = test.Name }

        let initialState =
            { Max = Int32.MinValue
              Min = Int32.MaxValue
              Name = test.Name }

        Array.fold folder initialState test.Samples

    let compare inspectFn (test: PrecisionTest) (n: int) =

        let low, hi = test.Range
        let inputs = Array.zeroCreate n

        for i in 0 .. n - 1 do
            inputs[i] <- nextDouble low hi

        let samples = generateSamples test.ReferenceFn inputs

        compareSamples
            inspectFn
            { Name = test.Name
              Fn = test.Fn
              Samples = samples }
