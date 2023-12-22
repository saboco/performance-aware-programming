module haversine.Program

open System
open System.IO

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

let generateAndSaveCoordinates test =
    let earthRadious = 6372.8
    let random = Random()
    let (sum, coordinates) = Generator.generateCoordinates (random.Next()) 1_000_000 earthRadious  
    let json = Json.toJson coordinates
    
    File.WriteAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\data.json", json)
    Console.WriteLine($"Expected haversine sum: {sum}")
    
    if test then
        try
            let _ = System.Text.Json.JsonSerializer.Deserialize<Test.Pairs>(json)
            Console.WriteLine("Generated json is VALID")
        with _ -> Console.WriteLine("WARNING: Generated json is INVALID!")
    
let readInputAndDeserialize () =
    let json = File.ReadAllText(@"C:\Users\sbotero\Documents\99. [PERSO] Safe to delete\01. Training\Performance Aware Programming\Haversine\input\data.json")
    let coordinates = Json.fromJson json
    printfn $"%A{coordinates}"
    
[<EntryPoint>]
let main (_: string []) =
    
    printfn "Generating data"
    generateAndSaveCoordinates false
    printfn "Deserializing data"
    readInputAndDeserialize ()
    
    
    0
