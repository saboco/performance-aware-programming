module haversine.Json

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

