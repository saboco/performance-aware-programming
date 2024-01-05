module haversine.Diagnostics

open System
open System.Collections.Generic
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type [<Measure>] ms
type [<Measure>] cycle
type [<Measure>] byte
type [<Measure>] Mb
type [<Measure>] Gb

let bytesPerMegabyte : float<byte Mb^-1> = 1024.0*1024.0<byte/Mb> 
let megabytesPerGigabyte : float<Mb Gb^-1> = 1024.0<Mb/Gb>

let bytesToMegabytes (x : int64<byte>) =  ((float x) * 1.0<byte>) / bytesPerMegabyte
let megabytesToGigabytes (x : float<Mb>) = x / megabytesPerGigabyte

module Native =
    let [<Literal>] dll = "rdtsc_debug"
    [<DllImport(dll, CallingConvention = CallingConvention.Cdecl)>]
    extern int64 Rdtsc()
    
let inline rdTsc () : int64<cycle> = Native.Rdtsc() * 1L<cycle>
    
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

let testsTimers (timeToWait : int64<ms>) =
    let osFreq = Stopwatch.Frequency
    let cpuStart = Native.Rdtsc()
    let osStart = Stopwatch.GetTimestamp()
    let osWaitTime = osFreq * timeToWait / 1000L<ms>;
    let mutable osEnd = 0L
    let mutable osElapsed = 0L
    while osElapsed < osWaitTime do
        osEnd <- Stopwatch.GetTimestamp()
        osElapsed <- osEnd - osStart

    let cpuEnd = Native.Rdtsc()
    let cpuElapsed = cpuEnd - cpuStart
    let cpuFrequency =  osFreq * cpuElapsed / osElapsed
    printfn $"OS Timer {osStart} -> {osEnd} = {osElapsed}"
    printfn $"OS seconds %.4f{float osElapsed/float osFreq}"
    printfn $"CPU Frequency {cpuStart} -> {cpuEnd} = @ {cpuFrequency} Hz"
   
type [<Struct>] TimeInfo = {
    ElapsedCyclesInclusive : int64<cycle>
    ElapsedCyclesExclusive : int64<cycle>
    ByteCount : int64<byte>
    HitCount : int
    ParentIndex : string
    Label: string }

type Time (
    [<Optional; DefaultParameterValue(0L<byte>)>] bytesCount : int64<byte>,
    [<CallerMemberName; Optional; DefaultParameterValue("")>] callerName: string,
    [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int) =
    
    let startCycles = rdTsc()
    let key = $"{line}_{callerName}"
    let mutable disposed = false
    let mutable bytesCount = bytesCount
    
    static let sprintTimeInfo (totalCycles : int64<cycle>) (timeInfo : TimeInfo) =
        let frequency = Time.CPUFrequency
        let percentage = ((float timeInfo.ElapsedCyclesExclusive) / (float totalCycles)) * 100.0
        let elapsedSeconds = (float timeInfo.ElapsedCyclesExclusive) /(float frequency)
        
        let sprint =
            $"{timeInfo.Label} [{timeInfo.HitCount}] {timeInfo.ElapsedCyclesExclusive} %.3f{elapsedSeconds}s "
            
        let childrenSprint =
            if timeInfo.ElapsedCyclesExclusive <> timeInfo.ElapsedCyclesInclusive then
                let percentageWithChildren = ((float timeInfo.ElapsedCyclesInclusive) / (float totalCycles)) * 100.0
                $"(%.2f{percentage}%%, %.2f{percentageWithChildren}%% w/children)"
            else
                $"(%.2f{percentage}%%)"
                
        let dataThroughput =
                if timeInfo.ByteCount > 0L<byte> then
                    let seconds =  (float timeInfo.ElapsedCyclesInclusive) /(float frequency)
                    let megabytes = (bytesToMegabytes timeInfo.ByteCount)
                    let throughput = (megabytesToGigabytes megabytes) / seconds
                    $", %.3f{megabytes}Mb, %.2f{throughput}Gb/s"
                else
                    ""
        $"{sprint}{childrenSprint}{dataThroughput}"
    
    let parentIndex =
        let parentIndex = Time.CurrentIndex
        Time.CurrentIndex <- key
        parentIndex
    
    let initDefault () =
        let times : Dictionary<_,_> = Time.Times  
        if times.ContainsKey key |> not then
            times[key] <-
                {   ElapsedCyclesInclusive = 0L<cycle>
                    ElapsedCyclesExclusive = 0L<cycle>
                    Label = callerName
                    ByteCount = 0L<byte>
                    ParentIndex = parentIndex
                    HitCount = 0 }  
            
    do initDefault()
    
    let oldElapsedInclusive =
        if Time.Times.ContainsKey parentIndex then
            Time.Times[parentIndex].ElapsedCyclesInclusive
        else
            0L<cycle>
    
    static let mutable startTotalCycles = 0L<cycle>
    static let mutable endTotalCycles = 0L<cycle>
    
    static member val CPUFrequency =
        let timeToWait = 10L<ms>
        let osFreq = Stopwatch.Frequency
        let cpuStart = Native.Rdtsc()
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
        
    static member val CurrentIndex : string = "" with get,set
    static member val Times = Dictionary<string, TimeInfo>()    
    
    static member InitTime () = startTotalCycles <- rdTsc()
    static member EndTime () = endTotalCycles <- rdTsc()
    
    static member Print () =
        let totalProgramCycles = endTotalCycles - startTotalCycles
        printfn $"Total time: {(float totalProgramCycles)/(float Time.CPUFrequency) * 1000.0} ms @ {Time.CPUFrequency / 1000_000L} MHz"
        for timeInfo in Time.Times.Values do
            sprintTimeInfo totalProgramCycles timeInfo 
            |> printfn "%s"
    
    member _.CountBytes(bytes : int64<byte>) = bytesCount <- bytesCount + bytes
    interface IDisposable with
    
        member _.Dispose() =
            if not disposed then
                disposed <- true
                                               
                let elapsedCycles = rdTsc() - startCycles
                Time.CurrentIndex <- parentIndex
            
                if Time.Times.ContainsKey parentIndex then
                    let parentTimeInfo = Time.Times[parentIndex]
                    Time.Times[parentIndex] <-
                        { parentTimeInfo with
                            ElapsedCyclesExclusive = parentTimeInfo.ElapsedCyclesExclusive - elapsedCycles }
                    
                let timeInfo = Time.Times[key]
                        
                Time.Times[key] <-
                    { timeInfo with
                        HitCount = timeInfo.HitCount + 1
                        ByteCount = timeInfo.ByteCount + bytesCount
                        ElapsedCyclesInclusive = oldElapsedInclusive + elapsedCycles
                        ElapsedCyclesExclusive = timeInfo.ElapsedCyclesExclusive + elapsedCycles }