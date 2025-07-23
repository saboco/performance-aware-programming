[<RequireQualifiedAccess>]
module SystemTesting.CustomMath
    open System
    open System.Runtime.Intrinsics
    open System.Runtime.Intrinsics.X86
    
    let [<Literal>] Pi64 = 3.14159265358979323846264338327950288419716939937510582097494459230781640628
    let [<Literal>] HalfPi = Pi64/2.0
    
    let fabs (x : double) : double = Math.Abs x
    let fma (x : double) (y : double) (z: double) : double = Math.FusedMultiplyAdd(x, y, z)
    
    let sin (origX : double) =
        let halfPi :double = HalfPi
        let posX : double = Math.Abs(origX);
        let x : double = if posX > halfPi then Pi64 - posX else posX
        
        let x2 = x*x;
        
        let mutable r : double = 0.000000000000002721675717
        r <- fma r x2 -0.000000000000764304326460
        r <- fma r x2 +0.000000000160589418966205
        r <- fma r x2 -0.000000025052106921502713
        r <- fma r x2 +0.000002755731921174349663
        r <- fma r x2 -0.000198412698412133651615
        r <- fma r x2 +0.008333333333333208317595
        r <- fma r x2 -0.166666666666666657414808
        r <- fma r x2 +1.000000000000000000000000
        r <- r * x
        
        let result = if origX < 0 then -1.0 * r else r
        result
        
    let cos (x : double) =
        let result = sin (x + HalfPi)
        result
        
    let sqrt (x : double) =
        Vector128.CreateScalar x
        |> Sse2.SqrtScalar 
        |> (fun v -> v[0])
        
    let asin (origX : double) =
        let needsTransform = origX > 0.7071067811865475244
        let x = if needsTransform then sqrt (1.0 - origX*origX) else origX
        
        let x2 = x*x;
        
        let mutable r = +0.937051490234062756101707
        r <-  fma r x2 -3.490443583255612214344410
        r <-  fma r x2 +6.186819757295904054217317
        r <-  fma r x2 -6.756794510834697398138360
        r <-  fma r x2 +5.081534831018740483443707
        r <-  fma r x2 -2.753168488249635892373135
        r <-  fma r x2 +1.123860218902060115198083
        r <-  fma r x2 -0.335174736401442241451321
        r <-  fma r x2 +0.088305893045829114695877
        r <-  fma r x2 -0.004413816332621807794756
        r <-  fma r x2 +0.013447193221907542334814
        r <-  fma r x2 +0.013776824789416922470431
        r <-  fma r x2 +0.017366257890261350277372
        r <-  fma r x2 +0.022371484508100256821672
        r <-  fma r x2 +0.030381966628286034143303
        r <-  fma r x2 +0.044642856704867270312143
        r <-  fma r x2 +0.075000000004415118270984
        r <-  fma r x2 +0.166666666666650920003434
        r <-  fma r x2 +1.000000000000000000000000
        r <- r * x;
        
        let result = if needsTransform then 1.57079632679489661923 - r else r
        result