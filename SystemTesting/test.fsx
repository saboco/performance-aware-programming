open System
open System.Security.Cryptography

type PrecisionResult = {
    Max : double
    Min : double
    Name : string
}

let private nextDouble  =
        
    let random = Random()
    let nextDouble lowerBound upperBound : double =
        let rDouble = random.NextDouble()
        let rRangeDouble = rDouble * upperBound + (1.0 - rDouble) * lowerBound 
        rRangeDouble
        
    nextDouble
    
let private generateSamples (referenceFunction : double -> double) (inputs : double [])=
    inputs
    |> Array.map (fun input -> input, referenceFunction input)

let compareSamples (name : string) (fn : double -> double) (samples : (double * double)[])=
    let min = Double.MaxValue
    let max = Double.MinValue
    let folder (min, max) (input, output) =
        let result = fn input
        let diff = result - output
        (Math.Min(min, diff), Math.Max(max, diff))
        
    let min, max = Array.fold folder (min,max) samples
    { Min = min; Max= max; Name=name }
    
let compare (name : string) (referenceFunction : double -> double) (fn : double -> double) (range : double*double) (n : int) =
    let low, hi = range
    let inputs = Array.zeroCreate n
    for i in 0..n-1 do
        let d = nextDouble low hi
        printfn $"{d}"
        inputs[i] <- d
        
    let samples = generateSamples referenceFunction inputs
    
    compareSamples name fn samples

compare "Sin" Math.Sin id (-90, 90.0) 1000
