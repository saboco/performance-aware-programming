module haversine.Generator
open System
open Haversine

let generateCoordinates seed n (radius : float) =
    let rLongInt0 = Random(seed)
    let rLongFractional0 = Random(seed * 2)
    let rLat0 = Random(seed * 3)
    let rLongInt1 = Random(seed)
    let rLongFractional1 = Random(seed * 4)
    let rLat1 = Random(seed * 5)
    let getRandomLatLon (randomLongI : Random) (randomLongF:Random) (randomLat : Random ) =
        let longI = randomLongI.Next(-180, 180)
        let longF = randomLongF.NextDouble()
        let long = (float longI) + longF
        let x = randomLat.NextDouble()
        let latR = degreesFromRadians (Math.Acos(2.0 * x - 1.0))
        let lat = degreesFromRadians latR
        (lat, long)

    let mutable sum = 0.0
    
    let coordinates = 
        [|for _ in 0..n do
            let (lat0, long0) = getRandomLatLon rLongInt0 rLongFractional0 rLat0
            let (lat1, long1) = getRandomLatLon rLongInt1 rLongFractional1 rLat1
            
            sum <- referenceHaversine long0 lat0 long1 lat1 radius
            (lat0, long0),(lat1,long1) |]
        
    (sum , coordinates)
                

