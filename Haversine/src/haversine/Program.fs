module haversine.Program

open System
open haversine

module Test =
    [<CLIMutable>]
    type Coordinates ={
        x0: float
        y0: float
        x1: float
        y1: float }

    [<CLIMutable>]
    type Pairs = {
        paris: Coordinates[]
    }

[<EntryPoint>]
let main (_: string []) =
    let earthRadious = 6372.8
    let random = Random()
    let (sum, coordinates) = Generator.generateCoordinates (random.Next()) 10000 earthRadious  
    let json = Json.toJson coordinates
    Console.WriteLine(json)
    Console.WriteLine($"Expected haversine sum: {sum}")
    
    try
        let _ = System.Text.Json.JsonSerializer.Deserialize<Test.Pairs>(json)
        Console.WriteLine("Generated json is VALID")
    with _ -> Console.WriteLine("WARNING: Generated json is INVALID!")
    
    0
