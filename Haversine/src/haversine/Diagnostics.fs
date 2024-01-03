module haversine.Diagnostics

open System
open System.Diagnostics
open System.Runtime.InteropServices

module Native =
    let [<Literal>] dll = "rdtsc_debug"
    [<DllImport(dll, CallingConvention = CallingConvention.Cdecl)>]
    extern int64 Rdtsc()
    
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
   
type [<Struct>] TimeInfo = {
    StartTime : int64
    EndTime : int64
    StartCycles : int64
    EndCycles : int64
    Label: string
}

let sprintTimeInfo totalTime totalCycles (timeInfo : TimeInfo) =
    let frequency = Stopwatch.Frequency
    let elapsedTime = timeInfo.EndTime - timeInfo.StartTime
    let percentage = ((float elapsedTime) / (float totalTime)) * 100.0
    let elapsedSeconds = (float elapsedTime) * 1.0/(float frequency)
    let elapsedCycles = timeInfo.EndCycles - timeInfo.StartCycles
    $"{timeInfo.Label} (%.2f{percentage}%%): {elapsedTime} ticks ({elapsedSeconds} s)\t({elapsedCycles} cycles/{totalCycles} total cycles)"

type Time (label : string) =
    let startTime = Stopwatch.GetTimestamp()
    let startCycles = Native.Rdtsc()    
    static member val Times = ResizeArray<TimeInfo>()    
    static member Print () =
        let lastTime = Time.Times[Time.Times.Count - 1]
        let firstTime = Time.Times[0]
        let totalProgramTime = lastTime.EndTime - firstTime.StartTime
        let totalProgramCycles = lastTime.EndCycles - firstTime.StartCycles
        for timeInfo in Time.Times do
            sprintTimeInfo totalProgramTime totalProgramCycles timeInfo 
            |> printfn "%s"
    
    interface IDisposable with
    
        member _.Dispose() =
            let endCycles = Native.Rdtsc()
            let endTime = Stopwatch.GetTimestamp()
            
            Time.Times.Add({
                StartTime = startTime
                EndTime = endTime
                StartCycles = startCycles
                EndCycles = endCycles
                Label = label
            })