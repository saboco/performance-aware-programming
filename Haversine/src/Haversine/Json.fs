module Haversine.Json

open System
open System.Collections.Generic
open System.Text
open Haversine.Calculator
open SystemTesting
open Diagnostics
open Timing
open Calculator

let toJson (coordinates : ((float*float) *(float*float))[]) =
    use _ = new Timer(int64 (coordinates.Length * 4 * sizeof<float>) * 1L<b>)
    
    let sb = StringBuilder()
    
    let appendPropertyWithValue key value (sb : StringBuilder) = sb.Append($"\"{key}\":{value}")
    let appendProperty name (sb : StringBuilder) = sb.Append($"\"{name}\":") 
    let appendSeparator (separator:string) (sb : StringBuilder)  = sb.Append(separator)
    let appendObjectOpening (sb: StringBuilder) = sb.Append("{")
    let appendObjectClosing (sb: StringBuilder) = sb.Append("}")
    let appendArrayOpening (sb: StringBuilder) = sb.Append("[")
    let appendArrayClosing (sb: StringBuilder) = sb.Append("]")
    
    let appendCoordinates coordinates (sb:StringBuilder) =
        let mutable shouldAppendSeparator = false
        
        for (x0,y0),(x1,y1) in coordinates do
            if shouldAppendSeparator then
                appendSeparator "," sb |> ignore 
            else
                shouldAppendSeparator <- true
            
            sb
            |> appendObjectOpening
            |> appendPropertyWithValue "x0" x0
            |> appendSeparator ","
            |> appendPropertyWithValue "y0" y0
            |> appendSeparator ","
            |> appendPropertyWithValue "x1" x1
            |> appendSeparator ","
            |> appendPropertyWithValue "y1" y1
            |> appendObjectClosing 
            |> ignore
            
        sb
    
    sb
    |> appendObjectOpening
    |> appendProperty "pairs"
    |> appendArrayOpening
    |> appendCoordinates coordinates
    |> appendArrayClosing
    |> appendObjectClosing
    |> ignore
    
    sb.ToString()

/// JSON model, it does not handle all cases, ex: null is missing
type JValue =
    | JString of string
    | JNum of float
    | JArray of JValue list
    | JObject of Map<string, JValue>
    | JNull

/// very naif implementation of a json parser
/// the way to go in the optimization is to explore combinator parsers and how it compares in performance with this implementation
let fromJson (json:string) f =
    // use _ = new Timer(int64 json.Length * 1L<b>)
    
    let (|IsNum|_|) (c: char) = 
        if Char.IsDigit(c) || c='-' then Some () else None

    let (|IsFloat|_|) (s:string) =
        match Double.TryParse(s) with
        | true, f -> Some f
        | _ -> None
    
    let rec readJson (json : string) at : int * JValue =
        let readString (json : string) at =
            use _ = new Timer(0L<b>, "readString")
            let mutable i = at
            while i < json.Length && json[i] <> '"' do
                i <- i + 1

            i + 1, JString (json.Substring(at, i - at))
        
        let readNum (json : string) at =
            let mutable i = at
            while i < json.Length && (Char.IsDigit(json[i]) || json[i] = '.' || json[i] = '-'|| json[i]='E') do
                i <- i + 1
                
            i,  json.Substring(at, i - at)
        
        let readJsonArray (jObjects : ResizeArray<JValue>) (json : string) at =
            let mutable i = at
            while i < json.Length && json[i] <> ']' do
                let j, jObject = readJson json i
                jObjects.Add(jObject)
                i <- j
                while i < json.Length && (json[i] = ' ' || json[i] = ',') do
                    i <- i + 1
            
            i + 1, jObjects

        let readKey (json : string) (at : int) =
            let at = json.IndexOf('"', at)
            let indexOfEndOfKey = json.IndexOf(':', at)
            
            let key = if indexOfEndOfKey >=0 then json.Substring(at, indexOfEndOfKey - at) else ""
            let at = if indexOfEndOfKey > 0 then indexOfEndOfKey else at
            at + 1, key.Trim().TrimStart('"').TrimEnd('"')

        let readJsonObject (json : string) at =
            let mutable jObject = Map.empty<string, JValue>
            let mutable i = at
            while i < json.Length && json[i] <> '}' do
                let j, key = readKey json i
                let j, value = readJson json j
                
                if value <> JNull then
                    jObject <- Map.add key value jObject
                i <- j
                while i < json.Length && (json[i] = ' ' || json[i] = ',') do
                    i <- i + 1
            i + 1, JObject jObject

        if at < json.Length then
            let token = json[at]
            match token with
            | '[' -> 
                let jObjects = ResizeArray<JValue>()
                let at, _ = readJsonArray jObjects json (at + 1)
                at, JArray (List.ofSeq jObjects)
            | '{' -> readJsonObject json (at + 1)
            | '"' -> readString json (at + 1)
            | ' ' -> readJson json (at + 1)
            | IsNum ->
                let at, value = readNum json at
                let jsonNum =
                    match value with
                    | IsFloat f -> JNum f
                    | num -> failwithf $"unsupported num format '{num}'"
                at, jsonNum
            | c -> failwithf $"Unexpected token '{c}' at {at} position"
            
        else
            at, JNull

    let at, jObject = readJson json 0
    printfn $"read up to {at} in the file"
    f jObject

let rec foldBackJson fString fBool fNum fNull fArray fObject jValue (cont : 'a -> 'r) : 'r =
    let recurse = foldBackJson fString fBool fNum fNull fArray fObject
    match jValue with
    | JString s -> cont (fString s)
    // | JBool b -> cont (fBool b)
    | JNum n -> cont (fNum n)
    | JArray l -> 
        let newCont innerVal =
            let newInnerVal = fArray innerVal
            cont newInnerVal

        List.fold (fun state jValue acc -> 
            recurse jValue (fun x -> state (x::acc))) 
            newCont 
            l
        |> fun f -> f []
        
    | JObject o ->
        let newCont innerVal =
            let newVal = fObject innerVal
            cont newVal
        
        Map.fold (fun state k jValue acc ->
            recurse jValue (fun x -> state (Map.add k x acc)))
            newCont
            o
        |> fun f -> f Map.empty
    | JNull -> cont fNull

let rec foldBackBackJson fString fBool fNum fNull fArray fObject jValue (cont : 'a -> 'r) : 'r =
    let recurse = foldBackBackJson fString fBool fNum fNull fArray fObject
    match jValue with
    | JString s -> cont (fString s)
    // | JBool b -> cont (fBool b)
    | JNum n -> cont (fNum n)
    | JArray l -> 
        let newCont innerVal =
            let newInnerVal = fArray innerVal
            cont newInnerVal

        List.foldBack (fun jValue state acc -> 
            recurse jValue (fun x -> state (x::acc))) 
            (l |> List.rev)
            newCont 
        |> fun f -> f []
        
    | JObject o ->
        let newCont innerVal =
            let newVal = fObject innerVal
            cont newVal
        
        Map.foldBack (fun k jValue state acc ->
            recurse jValue (fun x -> state (Map.add k x acc)))
            o
            newCont
        |> fun f -> f Map.empty
    | JNull -> cont fNull


type Pairs =
    | Complete of x0: float * y0: float * x1: float * y1: float
    | Partial of float
    | List of ((float * float) * (float * float)) seq
    | NA

let partialToFloat p =
    match p with
    | Partial f -> f
    | pairs -> failwithf $"unexpected case '%A{pairs}"

/// Custom conversion from JsonValue representation to 'client' representation
let toCoordinates (jObject : JValue) =
    let rec loop 
        (fJArray: _ -> Pairs) 
        (fJObject: float -> float -> float -> float -> Pairs)
        (fJFloat: _ -> Pairs) 
        (fJString: _ -> Pairs)
        (fJNull: _ -> Pairs)
        jObject =
        let rec recurse = loop fJArray fJObject fJFloat fJString fJNull
        match jObject with
        | JArray l ->
            let state = ResizeArray<_>()
            for obj in l do
                state.Add(recurse obj)
            fJArray state
        | JObject map ->
            if map.ContainsKey("pairs") then
                recurse map["pairs"]
            else
                let x0 = recurse map["x0"] |> partialToFloat
                let y0 = recurse map["y0"] |> partialToFloat
                let x1 = recurse map["x1"] |> partialToFloat
                let y1 = recurse map["y1"] |> partialToFloat

                fJObject x0 y0 x1 y1
        | JNum f -> fJFloat f
        | JString s -> fJString s 
        | JNull -> fJNull ()
    
    let fJObject x0 y0 x1 y1 = Complete (x0,y0,x1,y1)
    let fJArray (l : Pairs ResizeArray) = 
        
        let l =
            l |> Seq.map (fun p -> 
                match p with
                | Complete (x0,y0,x1,y1) -> ((x0,y0),(x1,y1))
                | c -> failwithf $"unexpected item for array '{c}'" )

        List l
        
    let fJFloat f = Partial f
    let fJString _ = NA
    let fJNull () = NA
    
    match loop fJArray fJObject fJFloat fJString fJNull jObject with
    | List l -> l |> Seq.map (fun ((x0,y0),(x1,y1)) -> { x0=x0;y0=y0;x1=x1;y1=y1}) |> Array.ofSeq
    | c -> failwithf $"Unwrapping result Unexpected case '%A{c}'"

type PairsBuilder ()=
    let pairs = Dictionary<string, float>()
    let coordinates = ResizeArray<Coordinates>() 
    let mutable key = ""
    let mutable waitingValue = false
    
    member this.Key k =
        key <- k
        waitingValue <- true
        this
        
    member this.Value (v : float) =
        if waitingValue then
            waitingValue <- false
            this.Pair(key, v)
        this
        
    member private this.Pair (k:string,v : float) =
          pairs.Add(k,v)
          if pairs.Count = 4 then
              coordinates.Add({
                  x0=pairs["x0"]
                  y0=pairs["y0"]
                  x1=pairs["x1"]
                  y1=pairs["y1"]
              })
              pairs.Clear()
              
    member this.Build () = Array.ofSeq coordinates 
        
let toCoordinates2 (jsonValue : JValue) =
       
    let rec foldJson (builder : PairsBuilder) json =
       match json with
       | JNum n -> builder.Value n
       | JString _ -> builder
       | JNull -> builder
       | JObject o ->
           let folder (builder : PairsBuilder) (k : string) (v : JValue) =
               let newBuilder = builder.Key k
               foldJson newBuilder v
           Map.fold folder builder o
       | JArray arr -> List.fold foldJson builder arr
    
    let builder = foldJson (PairsBuilder()) jsonValue
    builder.Build()
    
let toCoordinates3 (jsonValue : JValue) =
    let unWrap jValue =
        let fString s = s :> obj
        let fNum n = n :> obj
        let fBool b = b :> obj
        let fNull = null
        let fArray arr = arr :> obj
        let fObject o = o :> obj
        
        foldBackJson fString fBool fNum fNull fArray fObject jValue (fun o -> unbox o : Map<string, obj>)
        
    let map = unWrap jsonValue
    
    let pairs : obj list = unbox map["pairs"]
    
    pairs
    |> List.map (fun o -> unbox o |> (fun (m : Map<string, obj>) -> { x0= unbox m["x0"]; y0= unbox m["y0"]; x1=unbox m["x1"]; y1=unbox m["y1"]  }  ))
    |> Array.ofList
          
let toCoordinates4 (jsonValue : JValue) =
    printfn "toCoordinates4"
    let unWrap jValue =
        let fString s = s :> obj
        let fNum n = n :> obj
        let fBool b = b :> obj
        let fNull = null
        let fArray arr = arr :> obj
        let fObject o = o :> obj
        
        foldBackBackJson fString fBool fNum fNull fArray fObject jValue (fun o -> unbox o : Map<string, obj>)
        
    printfn "unwrapping"
    
    let map = unWrap jsonValue
    
    printfn "unwrapped"
    let pairs : obj list = unbox map["pairs"]
    
    pairs
    |> List.map (fun o -> unbox o |> (fun (m : Map<string, obj>) -> { x0= unbox m["x0"]; y0= unbox m["y0"]; x1=unbox m["x1"]; y1=unbox m["y1"]  }  ))
    |> Array.ofList
          
