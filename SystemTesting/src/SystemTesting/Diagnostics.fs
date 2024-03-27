module SystemTesting.Diagnostics

open System
open System.Collections.Generic
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type [<Measure>] ms
type [<Measure>] cycle
type [<Measure>] b
type [<Measure>] Mb
type [<Measure>] Gb
type [<Measure>] s

let bytesPerMegabyte : float<b Mb^-1> = 1024.0*1024.0<b/Mb> 
let megabytesPerGigabyte : float<Mb Gb^-1> = 1024.0<Mb/Gb>

let bytesToMegabytes (x : int64<b>) =  ((float x) * 1.0<b>) / bytesPerMegabyte
let megabytesToGigabytes (x : float<Mb>) = x / megabytesPerGigabyte

module Native =
    let [<Literal>] rdtsc_dll = "rdtsc_debug"
    let [<Literal>] windows_metrics_dll = "windows_metrics_debug"
    let [<Literal>] page_fault_probe = "page_fault_probe_debug"
    [<DllImport(rdtsc_dll, CallingConvention = CallingConvention.StdCall)>]
    extern int64 Rdtsc()
    [<DllImport(windows_metrics_dll, CallingConvention = CallingConvention.StdCall)>]
    extern void InitializeMetrics()
    [<DllImport(windows_metrics_dll, CallingConvention = CallingConvention.StdCall)>]
    extern UInt64 ReadPageFaultCount()
    
    [<UnmanagedFunctionPointer(CallingConvention.StdCall)>]
    type PutResult = delegate of UInt64 * UInt64 * UInt64 * UInt64 -> unit
    
    [<DllImport(page_fault_probe, CallingConvention = CallingConvention.StdCall)>]
    extern void ProbePageFaults (UInt64 pageCount, PutResult putResult)
    [<DllImport(page_fault_probe, CallingConvention = CallingConvention.StdCall)>]
    extern void ProbePageFaultsHeapAllocation (UInt64 pageCount, PutResult putResult)
    
module Timing =
    let inline rdTsc () : int64<cycle> = Native.Rdtsc() * 1L<cycle>
           
    type [<Struct>] TimeInfo = {
        ElapsedCyclesInclusive : int64<cycle>
        ElapsedCyclesExclusive : int64<cycle>
        ByteCount : int64<b>
        HitCount : int
        ParentIndex : string
        Label: string }
    
    let secondsFromCpuTime (cpuFrequency : int64) (cpuTime : int64<cycle>) = (float cpuTime) / (float cpuFrequency) * 1.0<s>
    let bandwidth (bytes : int64<b>) (seconds : float<s>) = ((float bytes) * 1.0<b>) / bytesPerMegabyte / megabytesPerGigabyte / seconds 
    
    let sprintTimeInfo (cpuFrequency : int64) (totalCycles : int64<cycle>) (timeInfo : TimeInfo) =
        let percentage = ((float timeInfo.ElapsedCyclesExclusive) / (float totalCycles)) * 100.0
        let elapsedSeconds = secondsFromCpuTime cpuFrequency timeInfo.ElapsedCyclesExclusive 
        
        let sprint =
            $"{timeInfo.Label} [{timeInfo.HitCount}] {timeInfo.ElapsedCyclesExclusive} %.3f{elapsedSeconds}s "
            
        let childrenSprint =
            if timeInfo.ElapsedCyclesExclusive <> timeInfo.ElapsedCyclesInclusive then
                let percentageWithChildren = ((float timeInfo.ElapsedCyclesInclusive) / (float totalCycles)) * 100.0
                $"(%.2f{percentage}%%, %.2f{percentageWithChildren}%% w/children)"
            else
                $"(%.2f{percentage}%%)"
                
        let dataThroughput =
                if timeInfo.ByteCount > 0L<b> then
                    let seconds =  (float timeInfo.ElapsedCyclesInclusive) /(float cpuFrequency)
                    let megabytes = (bytesToMegabytes timeInfo.ByteCount)
                    let throughput = (megabytesToGigabytes megabytes) / seconds
                    $", %.3f{megabytes}Mb, %.2f{throughput}Gb/s"
                else
                    ""
        $"{sprint}{childrenSprint}{dataThroughput}"
        
    let printTimeInfos (cpuFrequency : int64) totalProgramCycles (timeInfos : Dictionary<string, TimeInfo>) =
        printfn $"Total time: {(float totalProgramCycles)/(float cpuFrequency) * 1000.0} ms @ {cpuFrequency / 1000_000L} MHz"
        for timeInfo in timeInfos.Values do
            sprintTimeInfo cpuFrequency totalProgramCycles timeInfo 
            |> printfn "%s"
        
    let testsTimers (timeToWait : int64<ms>) =
        let osFreq = Stopwatch.Frequency
        let osStart = Stopwatch.GetTimestamp()
        let osWaitTime = osFreq * timeToWait / 1000L<ms>;
        let mutable osEnd = 0L
        let mutable osElapsed = 0L
        let cpuStart = Native.Rdtsc()
        while osElapsed < osWaitTime do
            osEnd <- Stopwatch.GetTimestamp()
            osElapsed <- osEnd - osStart
    
        let cpuEnd = Native.Rdtsc()
        let cpuElapsed = cpuEnd - cpuStart
        let cpuFrequency =  osFreq * cpuElapsed / osElapsed
        
        printfn $"OS Timer {osStart} -> {osEnd} = {osElapsed}"
        printfn $"OS seconds %.4f{float osElapsed/float osFreq}"
        printfn $"CPU Frequency {cpuStart} -> {cpuEnd} = @ {cpuFrequency} Hz"

    let testPagesFaults () =
        Native.InitializeMetrics()
        let startPageFaults = Native.ReadPageFaultCount()
        let arr : Int64[] = Array.create<Int64>(2*4096) 1L
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
        let cpuFrequency =  osFreq * cpuElapsed / osElapsed
        cpuFrequency

#if FULL_PROFILING
    type Timer (
        [<Optional; DefaultParameterValue(0L<b>)>] bytesCount : int64<b>,
        [<CallerMemberName; Optional; DefaultParameterValue("")>] callerName: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int) =
        
        let startCycles = rdTsc()
        let key = $"{line}_{callerName}"
        let mutable disposed = false
        let mutable bytesCount = bytesCount
        
        let parentIndex =
            let parentIndex = Timer.CurrentIndex
            Timer.CurrentIndex <- key
            parentIndex
        
        let initDefault () =
            let times : Dictionary<_,_> = Timer.Times  
            if times.ContainsKey key |> not then
                times[key] <-
                    {   ElapsedCyclesInclusive = 0L<cycle>
                        ElapsedCyclesExclusive = 0L<cycle>
                        Label = callerName
                        ByteCount = 0L<b>
                        ParentIndex = parentIndex
                        HitCount = 0 }  
                
        do initDefault()
        
        let oldElapsedInclusive =
            if Timer.Times.ContainsKey parentIndex then
                Timer.Times[parentIndex].ElapsedCyclesInclusive
            else
                0L<cycle>
        
        static let mutable startTotalCycles = 0L<cycle>
        static let mutable endTotalCycles = 0L<cycle>
        
        static member val CPUFrequency = estimateCpuFrequency ()
        static member val CurrentIndex : string = "" with get,set
        static member val Times = Dictionary<string, TimeInfo>()    
        
        static member BeginTime () = startTotalCycles <- rdTsc()
        static member EndTime () = endTotalCycles <- rdTsc()
        
        static member Print () =
            let totalProgramCycles = endTotalCycles - startTotalCycles
            printTimeInfos Timer.CPUFrequency totalProgramCycles Timer.Times
        
        member _.CountBytes(bytes : int64<b>) = bytesCount <- bytesCount + bytes
        interface IDisposable with
        
            member _.Dispose() =
                if not disposed then
                    disposed <- true
                                                   
                    let elapsedCycles = rdTsc() - startCycles
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
    type Timer (
        [<Optional; DefaultParameterValue(0L<b>)>] _bytesCount : int64<b>,
        [<CallerMemberName; Optional; DefaultParameterValue("")>] _callerName: string,
        [<CallerLineNumber; Optional; DefaultParameterValue(0)>] _line: int) =
        static let mutable startTotalCycles = 0L<cycle>
        static let mutable endTotalCycles = 0L<cycle>
        
        static member val CPUFrequency = estimateCpuFrequency ()
        static member val Times = Dictionary<string, TimeInfo>()    
        
        static member BeginTime () = startTotalCycles <- rdTsc()
        static member EndTime () = endTotalCycles <- rdTsc()
        
        member _.CountBytes(_ : int64<b>) = ()
        
        static member Print () =
            let totalProgramCycles = endTotalCycles - startTotalCycles
            printTimeInfos Timer.CPUFrequency totalProgramCycles Timer.Times
            
        interface IDisposable with member _.Dispose() = ()
#endif

module Repetition =
    type RepetitionTestResult = {
        CPUTime : int64<cycle>
        BytesCount : int64<b>
        MemPageFaults : uint64
    } 
    with
        static member Empty = {
            CPUTime = 0L<cycle>
            BytesCount = 0L<b>
            MemPageFaults = 0UL }
        
    type RepetitionsResult = {
        Count : int64
        CPUFreq : int64
        Total : RepetitionTestResult
        Min : RepetitionTestResult
        Max : RepetitionTestResult }
    
    let private printResult label cpuFreq count (result : RepetitionTestResult) =
        let seconds = Timing.secondsFromCpuTime cpuFreq (result.CPUTime / count) 
        let bandwidth = Timing.bandwidth (result.BytesCount / count) seconds
        
        printf $"{label}{result.CPUTime} (%.3f{seconds * 1000.0}ms) %.3f{bandwidth}gb/s"
        
        if result.MemPageFaults > 0UL then
            printf $" PF: %i{result.MemPageFaults} (%0.4f{(float result.BytesCount) / (float result.MemPageFaults * 1024.0)}k/fault)"
        else
            printf "                                               "
        
    let print (results : RepetitionsResult) =
        printResult "Min: " results.CPUFreq 1 results.Min
        printfn ""
        printResult "Max: " results.CPUFreq 1 results.Max
        printfn ""
        printResult "Avg: " results.CPUFreq results.Count results.Total
        printfn ""
        
    let repeat printProgression (secondsToTry: int64<s>) (fn : unit -> int64<b>) =
        let cpuFreq = Timing.estimateCpuFrequency()
        Native.InitializeMetrics()
        let timeToTry = cpuFreq * 1L<cycle/s> * secondsToTry 
        
        let mutable repetitionsResults = {
            Count = 0L
            CPUFreq = cpuFreq 
            Total =  RepetitionTestResult.Empty
            Max =  RepetitionTestResult.Empty
            Min = { RepetitionTestResult.Empty with  CPUTime = Int64.MaxValue * 1L<cycle> }
        }
        
        let mutable testStartedAt = Timing.rdTsc()
                
        while (Timing.rdTsc() - testStartedAt) < timeToTry do
            let startPageFaults = Native.ReadPageFaultCount()
            let readStart = Timing.rdTsc()
            let bytesCount = fn() // function under test
            let readEnd = Timing.rdTsc()
            let endPageFaults = Native.ReadPageFaultCount()
            
            let elapsed = readEnd - readStart
            
            let currentTestResult = { CPUTime = elapsed; BytesCount = bytesCount; MemPageFaults = endPageFaults - startPageFaults }
            
            let min =
                if elapsed < repetitionsResults.Min.CPUTime then
                    testStartedAt <- Timing.rdTsc()
                    currentTestResult
                else
                    repetitionsResults.Min
            
            let max =
                if elapsed > repetitionsResults.Max.CPUTime then
                    currentTestResult
                else
                    repetitionsResults.Max
            
            let total = {
                CPUTime = repetitionsResults.Total.CPUTime + currentTestResult.CPUTime
                BytesCount = repetitionsResults.Total.BytesCount + currentTestResult.BytesCount
                MemPageFaults = repetitionsResults.Total.MemPageFaults + currentTestResult.MemPageFaults }
                    
            repetitionsResults <- {
                repetitionsResults with
                    Count = repetitionsResults.Count + 1L
                    Min = min
                    Max = max
                    Total = total }
            
            if printProgression then
               printResult "" cpuFreq 1 currentTestResult
               let struct (_,top) = Console.GetCursorPosition()
               Console.SetCursorPosition(0, top)
        
        repetitionsResults