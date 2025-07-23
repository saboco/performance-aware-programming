open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86

let sqrtCustomWithIntrinsics (x : double) =
    Vector128.CreateScalar x
    |> Sse2.SqrtScalar 
    |> (fun v -> v[0])

sqrtCustomWithIntrinsics 25

1.0 / System.Math.Sqrt 2.0
