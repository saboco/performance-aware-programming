module SystemTesting.FileRead

open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.NativeInterop
open Memory
open Diagnostics
open SystemTesting.Diagnostics.Repetition
open Windows.Native
open Buffers

[<Literal>]
let MIN_MEMORY_PAGE_SIZE = 4096UL

let allocateAndTouch bufferSize =
    let mutable buffer = allocateBufferV bufferSize
    let touchCount = (buffer.Count + MIN_MEMORY_PAGE_SIZE - 1UL) / MIN_MEMORY_PAGE_SIZE

    for touchIndex in 0UL .. touchCount - 1UL do
        NativePtr.set buffer.Data (int (MIN_MEMORY_PAGE_SIZE * touchIndex)) 0uy
        
    freeBufferV &buffer

let allocateAndCopy (totalFileSize: uint64) bufferSize (scratch: Buffer) =
    let mutable buffer = allocateBufferV bufferSize

    let readSize = int buffer.Count
    let mutable source = scratch.Data
    
    for sizeRemaining in int totalFileSize .. -readSize .. 0 do
        let readSize = min sizeRemaining readSize
        NativePtr.copyBlock buffer.Data source (int readSize)
        source <- NativePtr.add source (int readSize)
        
    freeBufferV &buffer
        
let openAllocateAndRead (fileName : string) (totalFileSize : UInt64) bufferSize =
    let mutable fileNameBuffer = System.Text.Encoding.UTF8.GetBytes(fileName)
    use ptrFileName = fixed fileNameBuffer
    
    let file = CreateFileA(
        ptrFileName |> NativePtr.toNativeInt,
        GenericAccessRights.GENERIC_READ,
        FileShareMode.FILE_SHARE_READ||| FileShareMode.FILE_SHARE_WRITE,
        0u,
        CreationDisposition.OPEN_EXISTING,
        FileAttributesAndFlags.FILE_ATTRIBUTE_NORMAL,
        0)

    let mutable buffer = allocateBufferV(bufferSize)
    
    if isValid buffer && file <> INVALID_HANDLE_VALUE then
       let readSize = int buffer.Count
       for sizeRemaining in int totalFileSize .. -readSize .. 0 do
           let readSize = min sizeRemaining readSize
           let mutable bytesRead = 0u
           let ptr = NativePtr.toNativeInt buffer.Data
           let _ = ReadFile(file, ptr, uint32 readSize, &&bytesRead, 0)
           if bytesRead <> uint32 readSize then
               failwith "Failed to read file"
    else
       failwith "Failed to open file"
       
    freeBufferV &buffer
       
let openAllocateAndFileRead fileName  bufferSize =
    let buffer = Array.zeroCreate<byte> bufferSize
    let file = File.OpenRead(fileName)
    let readSize = buffer.Length
    let length = int file.Length
    for _ in length .. -readSize .. 0 do
       file.Read(buffer, 0, readSize) |> ignore
       
    file.Length
       
let sum64s (dataSize :uint64) (buffer : nativeptr<int64>) =
    let mutable data = buffer
    let mutable sum0 = 0L
    let mutable sum1 = 0L
    let mutable sum2 = 0L
    let mutable sum3 = 0L
    let mutable sumCount = dataSize / (4UL * 8UL) // 4 sum of 8 bytes each
    while sumCount > 0UL do
        sum0 <- sum0 + NativePtr.get data 0
        sum1 <- sum1 + NativePtr.get data 1
        sum2 <- sum2 + NativePtr.get data 2
        sum3 <- sum3 + NativePtr.get data 3
        data <- NativePtr.add data 4
        sumCount <- sumCount - 1UL
        
    sum0 + sum1 + sum2 + sum3
        
let openAllocateAndSum fileName bufferSize =
    let buffer = Array.zeroCreate<byte> bufferSize
    let file = File.OpenRead(fileName)
    let readSize = buffer.Length
    let length = int file.Length
    let mutable totalBytesRead = 0L
    let mutable sum = 0L
    for _ in length .. -readSize .. 0 do
       let bytesRead = file.Read(buffer, 0, readSize)
       totalBytesRead <- totalBytesRead + int64 bytesRead
       use data = fixed buffer
       let ptr = data |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<int64>
       sum <- sum + sum64s (uint64 bytesRead) ptr
       
    file.Length, sum, totalBytesRead
        
let readEntireFile (filePath: string) : Buffer =
    let fileBytes = File.ReadAllBytes(filePath)
    let byteCount = fileBytes.Length
    let buffer = allocateBufferV (uint64 byteCount)

    for i in 0 .. byteCount - 1 do
        NativePtr.set buffer.Data i fileBytes[i]

    buffer
    
let runFileRead (fileName : string) =
    
    let mutable scratch = readEntireFile fileName
    let totalFileSize = scratch.Count
    let allocateAndTouch bufferSize = fun () ->
        allocateAndTouch bufferSize
        int64 totalFileSize * 1L<b>
    
    let allocateAndCopy bufferSize = fun () ->
        allocateAndCopy totalFileSize bufferSize scratch
        int64 totalFileSize * 1L<b>
        
    let openAllocateAndRead bufferSize = fun () ->
        openAllocateAndRead  fileName totalFileSize bufferSize
        int64 totalFileSize * 1L<b>
        
    let openAllocateAndFileRead bufferSize = fun () ->
        let fileSize = openAllocateAndFileRead fileName (int bufferSize)
        int64 fileSize * 1L<b>
    
    let functions = [|
        "allocateAndTouch", allocateAndTouch
        "allocateAndCopy", allocateAndCopy
        "openAllocateAndRead", openAllocateAndRead
        "openAllocateAndFileRead", openAllocateAndFileRead |]
    
    let allResults = Dictionary<string, ResizeArray<UInt64 * RepetitionsResult>>()
    let initialBufferSize = 256UL * 1024UL
    let gigaByte = 1024UL * 1024UL * 1024UL
    let bufferSizes = seq {
        let mutable bufferSize = initialBufferSize
        while bufferSize <= gigaByte do
            yield bufferSize
            bufferSize <- bufferSize * 2UL
    }
    
    for name, f in functions do
        if not (allResults.ContainsKey name) then
            allResults.Add(name, ResizeArray())
            
        for bufferSize in bufferSizes do
            printfn $"\n--- {name} and bufferSize of {bufferSize/1024UL} KB ---\n"
            let results = Repetition.repeat true 10L<s> (f bufferSize)
            Repetition.print results
            allResults[name].Add(bufferSize, results)
        
    let dataPoints =
        [| for KeyValue(name, results) in allResults do
            let points = 
                [| for (size, result) in results do
                       let cpuFreq = result.CPUFreq
                       let result = result.Min
                       let seconds = Timing.secondsFromCpuTime cpuFreq result.CPUTime
                       let bandwidth = Timing.bandwidth result.BytesCount seconds
                       let size = bytesToKilobytes (int64 size * 1L<b>)

                       size, bandwidth |]
                |> Seq.ofArray
                
            name, points |]

    Print.showMultiLineChart "Read File" dataPoints
    
    dataPoints
    |> Array.collect (fun (name, points) ->
        points
        |> Seq.map (fun (s, bw) -> $"{name},{s},{bw}")
        |> Seq.toArray)
    |> fun lines -> File.AppendAllLines(@"c:\temp\cache_read_file_tests.csv", lines)
    
    freeBufferV &scratch

let runFileReadAndSum (fileName : string) =
    let openAllocateAndFileRead bufferSize = fun () ->
        let fileSize = openAllocateAndFileRead fileName (int bufferSize)
        int64 fileSize * 1L<b>
        
    let openAllocateAndSum bufferSize = fun () ->
        let fileSize, _, totalBytesReadAndSum = openAllocateAndSum fileName (int bufferSize)
        int64 fileSize * 1L<b>
    
    let functions = [|
        "openAllocateAndFileRead", openAllocateAndFileRead
        "openAllocateAndSum", openAllocateAndSum |]
    
    let allResults = Dictionary<string, ResizeArray<UInt64 * RepetitionsResult>>()
    let initialBufferSize = 256UL * 1024UL
    let gigaByte = 1024UL * 1024UL * 1024UL
    let bufferSizes = seq {
        let mutable bufferSize = initialBufferSize
        while bufferSize <= gigaByte do
            yield bufferSize
            bufferSize <- bufferSize * 2UL
    }
    
    for name, f in functions do
        if not (allResults.ContainsKey name) then
            allResults.Add(name, ResizeArray())
            
        for bufferSize in bufferSizes do
            printfn $"\n--- {name} and bufferSize of {bufferSize/1024UL} KB ---\n"
            let results = Repetition.repeat true 10L<s> (f bufferSize)
            Repetition.print results
            allResults[name].Add(bufferSize, results)
        
    let dataPoints =
        [| for KeyValue(name, results) in allResults do
            let points = 
                [| for (size, result) in results do
                       let cpuFreq = result.CPUFreq
                       let result = result.Min
                       let seconds = Timing.secondsFromCpuTime cpuFreq result.CPUTime
                       let bandwidth = Timing.bandwidth result.BytesCount seconds
                       let size = bytesToKilobytes (int64 size * 1L<b>)

                       size, bandwidth |]
                |> Seq.ofArray
                
            name, points |]

    Print.showMultiLineChart "Read File and Sum" dataPoints
    
    dataPoints
    |> Array.collect (fun (name, points) ->
        points
        |> Seq.map (fun (s, bw) -> $"{name},{s},{bw}")
        |> Seq.toArray)
    |> fun lines -> File.AppendAllLines(@"c:\temp\cache_read_file_and_sum_tests.csv", lines)
    
type Ping = Ping of Buffer
type Pong =
    | Pong of BytesRead : UInt32 *  Buffer
    | Done
    
let sumBuffer (count : uint32) (buffer : Buffer) =
    let ptr = buffer.Data |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<int64>
    let sum = sum64s (uint64 count) ptr
    sum
    
let readFile file sizeRemaining (buffer : Buffer) =
   let readSize = min sizeRemaining (int64 buffer.Count)
   let mutable bytesRead = 0u
   let ptr = NativePtr.toNativeInt buffer.Data
   let _ = ReadFile(file, ptr, uint32 readSize, &&bytesRead, 0)
   if bytesRead <> uint32 readSize then
       failwith "Failed to read file"
   bytesRead
   
let openAllocateAndSumOverlappedMailboxProcessorImmediate (fileName : string)  bufferSize checkSum =
    let totalFileSize = FileInfo(fileName).Length
    
    let mutable buffer0 = allocateBufferV bufferSize
    let mutable buffer1 = allocateBufferV bufferSize
    
    let mutable fileNameBuffer = System.Text.Encoding.UTF8.GetBytes(fileName)
    use ptrFileName = fixed fileNameBuffer
    
    let file = CreateFileA(
        ptrFileName |> NativePtr.toNativeInt,
        GenericAccessRights.GENERIC_READ,
        FileShareMode.FILE_SHARE_READ||| FileShareMode.FILE_SHARE_WRITE,
        0u,
        CreationDisposition.OPEN_EXISTING,
        FileAttributesAndFlags.FILE_ATTRIBUTE_NORMAL,
        0)
    
    let mutable readCompleted = false
    let rec readAgent totalFileSize (sumAgent : MailboxProcessor<Pong>) =
        MailboxProcessor<Ping>.StartImmediate
        <| fun inbox ->
            let rec loop sizeRemaining = async {
                if sizeRemaining > 0L then
                    let! (Ping buffer) = inbox.Receive()
                    let bytesRead = readFile file sizeRemaining buffer
                    sumAgent.Post (Pong (bytesRead, buffer))
                    return! loop (sizeRemaining - int64 buffer.Count)
                else
                    sumAgent.Post Done
                    readCompleted <- true
                    return ()
            }
            loop (int64 totalFileSize)
            
    let mutable totalSum = 0L
    let mutable sumCompleted = false
    let sumAgent (readAgentRef : MailboxProcessor<Ping> ref) =
        MailboxProcessor<Pong>.StartImmediate
        <| fun inbox ->
            let rec loop () = async {
                let! msg  = inbox.Receive()
                match msg with
                | Pong (bytesToSum, buffer) ->
                    totalSum <- totalSum + sumBuffer bytesToSum buffer
                    readAgentRef.Value.Post (Ping buffer)
                    return! loop ()
                | Done ->
                    sumCompleted <- true
                    return ()
            }
            loop ()
                   
    let readAgentRef =
        let dummyReadAgent : MailboxProcessor<Ping> =
            MailboxProcessor.Start(fun _ -> async { return () })
            
        ref dummyReadAgent
        
    let sumAgent : MailboxProcessor<Pong> = sumAgent readAgentRef
    let readAgent = readAgent (uint64 totalFileSize) sumAgent
    readAgentRef.Value <- readAgent
    
    if isValid buffer0 && isValid buffer1 && file <> INVALID_HANDLE_VALUE then
        readAgent.Post (Ping buffer0)
        readAgent.Post (Ping buffer1)
        while not readCompleted || not sumCompleted do
            System.Threading.Thread.Sleep(0) 
    else
        failwith "Failed to open file"
        
    if totalSum <> checkSum then
        failwith "Failed to sum file"
        
    freeBufferV &buffer0
    freeBufferV &buffer1
    
    totalFileSize
let openAllocateAndSumOverlappedMailboxProcessor (fileName : string)  bufferSize checkSum =
    let totalFileSize = FileInfo(fileName).Length
    
    let mutable buffer0 = allocateBufferV bufferSize
    let mutable buffer1 = allocateBufferV bufferSize
    
    let mutable fileNameBuffer = System.Text.Encoding.UTF8.GetBytes(fileName)
    use ptrFileName = fixed fileNameBuffer
    
    let file = CreateFileA(
        ptrFileName |> NativePtr.toNativeInt,
        GenericAccessRights.GENERIC_READ,
        FileShareMode.FILE_SHARE_READ||| FileShareMode.FILE_SHARE_WRITE,
        0u,
        CreationDisposition.OPEN_EXISTING,
        FileAttributesAndFlags.FILE_ATTRIBUTE_NORMAL,
        0)
    
    let mutable readCompleted = false
    let rec readAgent totalFileSize (sumAgent : MailboxProcessor<Pong>) =
        MailboxProcessor<Ping>.Start
        <| fun inbox ->
            let rec loop sizeRemaining = async {
                if sizeRemaining > 0L then
                    let! (Ping buffer) = inbox.Receive()
                    let bytesRead = readFile file sizeRemaining buffer
                    sumAgent.Post (Pong (bytesRead, buffer))
                    return! loop (sizeRemaining - int64 buffer.Count)
                else
                    sumAgent.Post Done
                    readCompleted <- true
                    return ()
            }
            loop (int64 totalFileSize)
            
    let mutable totalSum = 0L
    let mutable sumCompleted = false
    let sumAgent (readAgentRef : MailboxProcessor<Ping> ref) =
        MailboxProcessor<Pong>.Start
        <| fun inbox ->
            let rec loop () = async {
                let! msg  = inbox.Receive()
                match msg with
                | Pong (bytesToSum, buffer) ->
                    totalSum <- totalSum + sumBuffer bytesToSum buffer
                    readAgentRef.Value.Post (Ping buffer)
                    return! loop ()
                | Done ->
                    sumCompleted <- true
                    return ()
            }
            loop ()
                   
    let readAgentRef =
        let dummyReadAgent : MailboxProcessor<Ping> =
            MailboxProcessor.Start(fun _ -> async { return () })
            
        ref dummyReadAgent
        
    let sumAgent : MailboxProcessor<Pong> = sumAgent readAgentRef
    let readAgent = readAgent (uint64 totalFileSize) sumAgent
    readAgentRef.Value <- readAgent
    
    if isValid buffer0 && isValid buffer1 && file <> INVALID_HANDLE_VALUE then
        readAgent.Post (Ping buffer0)
        readAgent.Post (Ping buffer1)
        while not readCompleted || not sumCompleted do
            System.Threading.Thread.Sleep(0) 
    else
        failwith "Failed to open file"
        
    if totalSum <> checkSum then
        failwith "Failed to sum file"
        
    freeBufferV &buffer0
    freeBufferV &buffer1
    
    totalFileSize
    
open System.Threading.Tasks
open System.Threading.Channels
open SystemTesting.Actor

let openAllocateAndSumOverlappedChannel (fileName : string)  bufferSize checkSum =
    let totalFileSize = FileInfo(fileName).Length
    
    let mutable buffer0 = allocateBufferV bufferSize
    let mutable buffer1 = allocateBufferV bufferSize
    
    let mutable fileNameBuffer = System.Text.Encoding.UTF8.GetBytes(fileName)
    use ptrFileName = fixed fileNameBuffer
    
    let file = CreateFileA(
        ptrFileName |> NativePtr.toNativeInt,
        GenericAccessRights.GENERIC_READ,
        FileShareMode.FILE_SHARE_READ||| FileShareMode.FILE_SHARE_WRITE,
        0u,
        CreationDisposition.OPEN_EXISTING,
        FileAttributesAndFlags.FILE_ATTRIBUTE_NORMAL,
        0)
    
    let mutable readCompleted = false
    let rec readAgent totalFileSize readSize (sumAgent : ChannelWriter<Pong>) =
        start<Ping> 2
        <| fun inbox -> task {
            for sizeRemaining in totalFileSize .. -readSize .. 0L do
                let readSize = min sizeRemaining readSize
                let! (Ping buffer) = inbox.Reader.ReadAsync()
                let bytesRead = readFile file readSize buffer
                do! sumAgent.PostAsync (Pong (bytesRead, buffer))
            
            do! sumAgent.PostAsync Done
            readCompleted <- true
        }
            
    let mutable totalSum = 0L
    let mutable sumCompleted = false
    let sumAgent (readAgent : ChannelWriter<Ping> ref) =
        start<Pong> 2
        <| fun inbox -> task {
                while not sumCompleted do
                    let! msg  = inbox.Reader.ReadAsync()
                    match msg with
                    | Pong (bytesToSum, buffer) ->
                        totalSum <- totalSum + sumBuffer bytesToSum buffer
                        do! readAgent.Value.PostAsync (Ping buffer)
                    | Done ->
                        sumCompleted <- true
            }
                   
    let readAgentRef  =
        let dummyReadAgent : ChannelWriter<Ping> = Unchecked.defaultof<_>
        ref dummyReadAgent
        
    let sumAgent : ChannelWriter<Pong> = sumAgent readAgentRef
    readAgentRef.Value <- readAgent totalFileSize (int64 bufferSize) sumAgent
    
    if isValid buffer0 && isValid buffer1 && file <> INVALID_HANDLE_VALUE then
        [|  readAgentRef.Value.PostAsync (Ping buffer0) :> Task
            readAgentRef.Value.PostAsync (Ping buffer1) :> Task |]
        |> System.Threading.Tasks.Task.WaitAll
        while not readCompleted || not sumCompleted do
            System.Threading.Thread.Sleep(0) 
    else
        failwith "Failed to open file"
        
    if totalSum <> checkSum then
        failwith "Failed to sum file"
        
    freeBufferV &buffer0
    freeBufferV &buffer1
    
    totalFileSize
    
let openAllocateAndSumOverlappedChannelImmediate (fileName : string)  bufferSize checkSum =
    let totalFileSize = FileInfo(fileName).Length
    
    let mutable buffer0 = allocateBufferV bufferSize
    let mutable buffer1 = allocateBufferV bufferSize
    
    let mutable fileNameBuffer = System.Text.Encoding.UTF8.GetBytes(fileName)
    use ptrFileName = fixed fileNameBuffer
    
    let file = CreateFileA(
        ptrFileName |> NativePtr.toNativeInt,
        GenericAccessRights.GENERIC_READ,
        FileShareMode.FILE_SHARE_READ||| FileShareMode.FILE_SHARE_WRITE,
        0u,
        CreationDisposition.OPEN_EXISTING,
        FileAttributesAndFlags.FILE_ATTRIBUTE_NORMAL,
        0)
    
    let mutable readCompleted = false
    let rec readAgent totalFileSize readSize (sumAgent : ChannelWriter<Pong>) =
        startImmediate<Ping> 2
        <| fun inbox -> task {
            for sizeRemaining in totalFileSize .. -readSize .. 0L do
                let readSize = min sizeRemaining readSize
                let! (Ping buffer) = inbox.Reader.ReadAsync()
                let bytesRead = readFile file readSize buffer
                do! sumAgent.PostAsync (Pong (bytesRead, buffer))
            
            do! sumAgent.PostAsync Done
            readCompleted <- true
        }
            
    let mutable totalSum = 0L
    let mutable sumCompleted = false
    let sumAgent (readAgent : ChannelWriter<Ping> ref) =
        startImmediate<Pong> 2
        <| fun inbox -> task {
                while not sumCompleted do
                    let! msg  = inbox.Reader.ReadAsync()
                    match msg with
                    | Pong (bytesToSum, buffer) ->
                        totalSum <- totalSum + sumBuffer bytesToSum buffer
                        do! readAgent.Value.PostAsync (Ping buffer)
                    | Done ->
                        sumCompleted <- true
            }
                   
    let readAgentRef  =
        let dummyReadAgent : ChannelWriter<Ping> = Unchecked.defaultof<_>
        ref dummyReadAgent
        
    let sumAgent : ChannelWriter<Pong> = sumAgent readAgentRef
    readAgentRef.Value <- readAgent totalFileSize (int64 bufferSize) sumAgent
    
    if isValid buffer0 && isValid buffer1 && file <> INVALID_HANDLE_VALUE then
        [|  readAgentRef.Value.PostAsync (Ping buffer0) :> Task
            readAgentRef.Value.PostAsync (Ping buffer1) :> Task |]
        |> System.Threading.Tasks.Task.WaitAll
        while not readCompleted || not sumCompleted do
            System.Threading.Thread.Sleep(0) 
    else
        failwith "Failed to open file"
        
    if totalSum <> checkSum then
        failwith "Failed to sum file"
        
    freeBufferV &buffer0
    freeBufferV &buffer1
    
    totalFileSize
    
let runFileReadAndSumOverlapped (fileName : string) =
    let checkSum = -1100072108409525139L
    let openAllocateAndFileRead bufferSize = fun () ->
        let fileSize = openAllocateAndFileRead fileName (int bufferSize)
        int64 fileSize * 1L<b>
    
    let openAllocateAndSum bufferSize = fun () ->
        let fileSize, _, totalBytesRead = openAllocateAndSum fileName (int bufferSize)
        int64 fileSize * 1L<b>
        
    let openAllocateAndSumOverlappedMailboxProcessor bufferSize = fun () ->
        let fileSize = openAllocateAndSumOverlappedMailboxProcessor fileName bufferSize checkSum
        int64 fileSize * 1L<b>
        
    let openAllocateAndSumOverlappedMailboxProcessorImmediate bufferSize = fun () ->
        let fileSize = openAllocateAndSumOverlappedMailboxProcessorImmediate fileName bufferSize checkSum
        int64 fileSize * 1L<b>
        
    let openAllocateAndSumOverlappedChannel bufferSize = fun () ->
        let fileSize = openAllocateAndSumOverlappedChannel fileName bufferSize checkSum
        int64 fileSize * 1L<b>
        
    let openAllocateAndSumOverlappedChannelImmediate bufferSize = fun () ->
        let fileSize = openAllocateAndSumOverlappedChannelImmediate fileName bufferSize checkSum
        int64 fileSize * 1L<b>
    
    let functions = [|
        "openAllocateAndFileRead", openAllocateAndFileRead
        "openAllocateAndSum", openAllocateAndSum
        "openAllocateAndSumOverlappedMailboxProcessor", openAllocateAndSumOverlappedMailboxProcessor
        "openAllocateAndSumOverlappedMailboxProcessorImmediate", openAllocateAndSumOverlappedMailboxProcessorImmediate
        "openAllocateAndSumOverlappedChannel", openAllocateAndSumOverlappedChannel
        "openAllocateAndSumOverlappedChannelNewThread", openAllocateAndSumOverlappedChannelImmediate |]
    
    let allResults = Dictionary<string, ResizeArray<UInt64 * RepetitionsResult>>()
    let initialBufferSize = 256UL * 1024UL
    let gigaByte = 1024UL * 1024UL * 1024UL
    let bufferSizes = seq {
        let mutable bufferSize = initialBufferSize
        while bufferSize <= gigaByte do
            yield bufferSize
            bufferSize <- bufferSize * 2UL
    }
    
    for name, f in functions do
        if not (allResults.ContainsKey name) then
            allResults.Add(name, ResizeArray())
            
        for bufferSize in bufferSizes do
            printfn $"\n--- {name} and bufferSize of {bufferSize/1024UL} KB ---\n"
            let results = Repetition.repeat true 10L<s> (f bufferSize)
            Repetition.print results
            allResults[name].Add(bufferSize, results)
        
    let dataPoints =
        [| for KeyValue(name, results) in allResults do
            let points = 
                [| for size, result in results do
                       let cpuFreq = result.CPUFreq
                       let result = result.Min
                       let seconds = Timing.secondsFromCpuTime cpuFreq result.CPUTime
                       let bandwidth = Timing.bandwidth result.BytesCount seconds
                       let size = bytesToKilobytes (int64 size * 1L<b>)

                       size, bandwidth |]
                |> Seq.ofArray
                
            name, points |]

    Print.showMultiLineChart "Read File and Sum Overlapped" dataPoints
    
    dataPoints
    |> Array.collect (fun (name, points) ->
        points
        |> Seq.map (fun (s, bw) -> $"{name},{s},{bw}")
        |> Seq.toArray)
    |> fun lines -> File.AppendAllLines(@"c:\temp\cache_read_file_and_sum_tests.csv", lines)
