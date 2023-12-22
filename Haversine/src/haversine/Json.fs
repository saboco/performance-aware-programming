module haversine.Json

open System
open System.Text

let toJson (coordinates : ((float*float) *(float*float))[]) =
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
        
        for ((x0,y0),(x1,y1)) in coordinates do
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

type JsonValue =
    | JsonArray of JsonValue list
    | JsonObject of Map<string, JsonValue>
    | JsonFloat of float
    | JsonString of string
    | JsonEnd

type Pairs =
    | Complete of x0: float * y0: float * x1: float * y1: float
    | Partial of float
    | List of (float * float * float * float) seq
    | NA

let partialToFloat p =
    match p with
    | Partial f -> f
    | pairs -> failwithf $"unexpected case '%A{pairs}"

let toCoordinates (jObject : JsonValue) =
    let rec loop 
        (fJArray: _ -> Pairs) 
        (fJObject: float -> float -> float -> float -> Pairs)
        (fJFloat: _ -> Pairs) 
        (fJString: _ -> Pairs)
        (fJNull: _ -> Pairs)
        (state: ResizeArray<Pairs>)
        jObject =
        let rec recurse state obj = loop fJArray fJObject fJFloat fJString fJNull state obj
        match jObject with
        | JsonArray l ->
            for obj in l do
                state.Add(recurse state obj)
            fJArray state
        | JsonObject map ->
            if map.ContainsKey("pairs") then
                recurse state map["pairs"]
            else
                let x0 = recurse state map["x0"] |> partialToFloat
                let y0 = recurse state map["y0"] |> partialToFloat
                let x1 = recurse state map["x1"] |> partialToFloat
                let y1 = recurse state map["y1"] |> partialToFloat

                fJObject x0 y0 x1 y1
        | JsonFloat f -> fJFloat f
        | JsonString s -> fJString s 
        | JsonEnd -> fJNull ()
   
    let fJObject x0 y0 x1 y1 = Complete (x0,y0,x1,y1)
    let fJArray (l : Pairs ResizeArray) = 
        
        let l =
            l |> Seq.map (fun p -> 
                match p with
                | Complete (x0,y0,x1,y1) -> (x0,y0,x1,y1)
                | c -> failwithf $"unexpected item for array '{c}'" )

        List l

        
    let fJFloat f = Partial f
    let fJString _ = NA
    let fJNull () = NA

    let pairs = ResizeArray<_>()
    match loop fJArray fJObject fJFloat fJString fJNull pairs jObject with
    | List l -> Array.ofSeq l
    | c -> failwithf $"Unwrapping result Unexpected case '%A{c}'"

 
/// very naif implementation of a json parser
/// the way to go in the optimization is to explore combinator parsers and how it compares in performance with this implementation
let fromJson (json:string) =
    let (|IsNum|_|) (c: char) = 
        if Char.IsDigit(c) || c='-' then Some () else None

    let (|IsFloat|_|) (s:string) =
        match Double.TryParse(s) with
        | (true, f) -> Some f
        | _ -> None
    
    let (|IsInteger|_|) (s:string) =
        match Int32.TryParse(s) with
        | (true, v) -> Some v
        | _ -> None
    
    let rec readJson (json : string) at : int * JsonValue =
        let readString (json : string) at =
            // printfn $"read string at={at}"
            let mutable i = at
            while i < json.Length && json[i] <> '"' do
                i <- i + 1

            i + 1, JsonString (json.Substring(at, i - at))
        
        let readNum (json : string) at =
            let mutable i = at
            while i < json.Length && (Char.IsDigit(json[i]) || json[i] = '.' || json[i] = '-'|| json[i]='E') do
                i <- i + 1
                
            i,  json.Substring(at, i - at)
        
        let readJsonArray (jObjects : ResizeArray<JsonValue>) (json : string) at =
            let mutable i = at
            while i < json.Length && json[i] <> ']' do
                let j, jObject = readJson json i
                // printfn $"[ARR] read object {jObject}"
                jObjects.Add(jObject)
                i <- j
                while i < json.Length && (json[i] = ' ' || json[i] = ',') do
                    i <- i + 1
                    
            // printfn $"read ARR with length '{jObjects.Count}"
            i + 1, jObjects

        let readKey (json : string) (at : int) =
            let at = json.IndexOf('"', at)
            let indexOfEndOfKey = json.IndexOf(':', at)
            
            let key = if indexOfEndOfKey >=0 then json.Substring(at, indexOfEndOfKey - at) else ""
            let at = if indexOfEndOfKey > 0 then indexOfEndOfKey else at
            at + 1, key.Trim().TrimStart('"').TrimEnd('"')

        let readJsonObject (json : string) at =
            let mutable jObject = Map.empty<string, JsonValue>
            let mutable i = at
            while i < json.Length && json[i] <> '}' do
                let j, key = readKey json i
                // printfn $"read key={key} starting value at={j}"
                let j, value = readJson json j
                
                if value <> JsonEnd then
                    jObject <- Map.add key value jObject
                // printfn $"read value={value} ending value at={j}"
                i <- j
                while i < json.Length && (json[i] = ' ' || json[i] = ',') do
                    i <- i + 1
            // printfn $"[OBJ] final object {jObject}"
            i + 1, JsonObject jObject

        if at < json.Length then
            let token = json[at]
            // printfn $"json[{at}]={token}"
            match token with
            | '[' -> 
                let jObjects = ResizeArray<JsonValue>()
                let at, _ = readJsonArray jObjects json (at + 1)
                printfn $"read array {at} json"
                at, JsonArray (List.ofSeq jObjects)
            | '{' -> readJsonObject json (at + 1)
            | '"' -> readString json (at + 1)
            | ' ' -> readJson json (at + 1)
            | IsNum ->
                let at, value = readNum json at
                let jsonNum =
                    match value with
                    | IsFloat f -> JsonFloat f
                    | num -> failwithf $"unsupported num format '{num}'"
                at, jsonNum
            | c -> failwithf $"Unexpected token '{c}' at {at} position"
            
        else
            at, JsonEnd

    let at, jObject = readJson json 0
    printfn $"read up to {at} in the file"
    jObject
    |> toCoordinates
