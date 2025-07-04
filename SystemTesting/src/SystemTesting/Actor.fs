module SystemTesting.Actor

open System.Threading
open System.Threading.Tasks
open System.Threading.Channels

/// Start the actor, running the actorFunction in a background task
/// When capacity is reached, the writer will block asynchronously
let start<'t> (capacity: int) (actorFunction : Channel<_> -> Task) =
    let options = BoundedChannelOptions(capacity, FullMode = BoundedChannelFullMode.Wait, SingleReader = true, SingleWriter = false)
    let channel = Channel.CreateBounded<'t>(options)
    let t =
        backgroundTask {
            try
                do! actorFunction channel
            with
            | ex -> printfn $"{ex}"
         }
    channel.Writer

/// Start the actor, running the actorFunction in a new thread
let startImmediate<'t> (capacity: int) (actorFunction : Channel<_> -> Task) =
    let options = BoundedChannelOptions(capacity, FullMode = BoundedChannelFullMode.Wait, SingleReader = true, SingleWriter = false)
    let channel = Channel.CreateBounded<'t>(options)
    let t =  Task.Run(fun () -> actorFunction channel)
    channel.Writer
    
/// Extension methods to make it look more like MailboxProcessor
type ChannelWriter<'t> with

    /// Post a message asynchronously (identical to WriteAsync, but return Task<unit>
    member this.PostAsync(item: 't) =
        task {
            do! this.WriteAsync(item)
        }
        
    /// Post a message and awaits a response asynchronously
    /// The passed lambda takes a function that is used to reply, and can be embedded
    /// in the message sent to the actor
    member this.PostAndReplyAsync<'r>(createMessage: ('r -> unit) -> 't ) =
        task {
            let tcs = TaskCompletionSource<'r>()
            do! this.WriteAsync(createMessage tcs.SetResult)
            return! tcs.Task
        }