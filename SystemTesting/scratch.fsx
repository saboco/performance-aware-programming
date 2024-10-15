let double (from: uint64) ``end`` =
    seq {
        for i in from .. (``end`` / 2UL) do
            i * 2UL
    }

for i in double 0UL 32UL do
    printfn $"* {i}"


sizeof<uint64>

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<DllImport("Bcrypt.dll", CallingConvention = CallingConvention.Cdecl)>]
extern Int32 BCryptGenRandom(int hAlgorithm, byte* pbBuffer, UInt32 cbBuffer, UInt32 dwFlags)


let getMaxOSRandomCount () : UInt64 = 0xffffffffUL

let readOSRandomBytes (count: UInt64) (dest: nativeptr<byte>) =
    let mutable result = false

    if count < getMaxOSRandomCount () then
        let r = BCryptGenRandom(0, dest, uint count, 0x00000002u)
        result <- r = 0

    result

let mutable randomValue = 0UL

let ptrRandomValue =
    NativePtr.toNativeInt &&randomValue |> NativePtr.ofNativeInt<byte>

readOSRandomBytes (uint64 sizeof<UInt64>) ptrRandomValue
randomValue


let inline (~-) (a: uint64) = a - 1UL

for i in 8UL .. - 1UL .. 0UL do
    printfn $"{i}"

open System
open Microsoft.FSharp.NativeInterop

let testReadInt64FromByteArray () =
    let buffer = Array.zeroCreate<byte> 40

    buffer[0] <- 5uy
    buffer[8] <- 4uy
    buffer[16] <- 3uy
    buffer[24] <- 2uy
    buffer[32] <- 1uy

    let data = fixed buffer

    let ptr: nativeptr<int64> =
        data |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<int64>

    for i in 0..4 do
        let number = NativePtr.get<int64> ptr i
        printfn $"n[{i}]={number}"

testReadInt64FromByteArray ()
