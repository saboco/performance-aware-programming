module haversine.Haversine
open Diagnostics

let square(x: float) = x * x

let piOver180 = 0.01745329251994329577
let inverseOfPiOver190 = 1.0 / piOver180
let radiansFromDegrees (degrees : float) = piOver180 * degrees
let degreesFromRadians (radians : float) = radians * inverseOfPiOver190 

let referenceHaversine (x0:float) (y0 : float) (x1 : float) (y1 : float) (earthRadius: float) =
    
    let lat1 = y0
    let lat2 = y1
    let lon1 = x0
    let lon2 = x1
    
    let dLat = radiansFromDegrees(lat2 - lat1)
    let dLon = radiansFromDegrees(lon2 - lon1)
    let lat1 = radiansFromDegrees(lat1)
    let lat2 = radiansFromDegrees(lat2)
    
    let a = square(sin(dLat/2.0)) + cos(lat1)*cos(lat2)*square(sin(dLon/2.0))
    let c = 2.0*asin(sqrt(a))
    
    earthRadius * c
    
let sumHaversineDistances earthRadius (pairs : ((float*float)*(float*float))[]) =
    use t = new Time("sumHaversineDistances")
    let mutable sum = 0.0
    let sumCoefficient = 1.0 / (float pairs.Length)
    
    for (x0,y0),(x1,y1) in pairs do
        let dist = referenceHaversine x0 y0 x1 y1 earthRadius
        sum <- sum + sumCoefficient * dist
        
    sum