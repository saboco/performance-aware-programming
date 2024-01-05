module haversine.Generator
open System
open Haversine
open Diagnostics

let generateCoordinates seed n (radius : float) =
    use _ = new Time(int64 (n * 4 * sizeof<float>) * 1L<byte>)    
    let rLongInt0 = Random(seed)
    let rLongFractional0 = Random(seed * 2)
    let rLat0 = Random(seed * 3)
    let rLongInt1 = Random(seed)
    let rLongFractional1 = Random(seed * 4)
    let rLat1 = Random(seed * 5)
    let getRandomLatLon (randomLongI : Random) (randomLongF:Random) (randomLat : Random ) =
        let rec getLat () =
            let x = randomLat.NextDouble()
            let lat = degreesFromRadians (Math.Acos(2.0 * x - 1.0))
            if Math.Abs(lat) > 90 then
                getLat()
            else
                lat
        
        let longI = randomLongI.Next(-180, 180)
        let longF = randomLongF.NextDouble()
        let long = (float longI) + longF
        let lat = getLat()
        (lat, long)

    let mutable sum = 0.0
    
    let coordinates = 
        [|for _ in 1..n do
            let lat0, long0 = getRandomLatLon rLongInt0 rLongFractional0 rLat0
            let lat1, long1 = getRandomLatLon rLongInt1 rLongFractional1 rLat1
            
            sum <- sum + referenceHaversine long0 lat0 long1 lat1 radius
            (long0, lat0),(long1, lat1) |]
        
    (sum / (float coordinates.Length) , coordinates)