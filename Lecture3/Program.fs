open System

let rec boxMueller (random: Random) =
    let innerBoxMueller U1 U2 = seq {
        let fElem = (-2.0 * Math.Log U1) |> Math.Sqrt
        let sElem = 2.0 * Math.PI * U2
        yield fElem * Math.Sin(sElem);
        yield fElem * Math.Cos(sElem);
        yield! boxMueller random
    }
    (random.NextDouble(), random.NextDouble()) ||> innerBoxMueller

let geoBrownMotion n S0 r v t seed =
    let random = Random(seed)
    let randomVariables = Seq.take n (boxMueller random)
    let mapper = (S0, randomVariables) |> Seq.unfold (fun (last, ls) -> 
        if(Seq.isEmpty ls) then None
        else
            let div = float (t / n)
            let exponent = (r - (Math.Pow(v, 2.0) / 2.) * div + v * Math.Sqrt(div) * (Seq.head ls))
            let next = last * Math.Pow(Math.E, exponent)
            Some(last, (next, Seq.tail ls))
    )
    mapper

let getHistoricalVolatility (gbm: seq<float>) t n =
    let R = gbm |> Seq.pairwise |> Seq.map (fun (a, b) -> log(b / a))
    let RAvg = Seq.average R
    sqrt (( n / (t * (n-1.)) ) * (R |> Seq.map (fun r -> pown (r - RAvg) 2) |> Seq.sum))
    

let initPaths N fn seed =
    [for n in 1..N do yield fn (seed + n)]

let writePathsToFile paths (filename: String) =
    let resultStrings = [for path in paths 
        do yield
            [for change in path do yield (change |> string)] |> String.concat(" ")
        ]
    let wr = new IO.StreamWriter(filename)
    resultStrings |> String.concat("\n") |> wr.Write
    wr.Close()  

let checkNormalDist n seed =
    let random = Random(seed)
    let randomVariables = Seq.take n (boxMueller random)
    let resultStrings = [for var in randomVariables do yield (var |> string)]
    let wr = new IO.StreamWriter("to_check.csv")
    resultStrings |> String.concat("\n") |> wr.Write
    wr.Close()

let zad1 N n S0 r vol t seed  =
    let paths = initPaths N (geoBrownMotion n S0 r vol t) seed
    let resultStrings = [for path in paths do yield (Seq.last path |> string) + " " + (getHistoricalVolatility path (float t) (float n) |> string)]
    let writer = new IO.StreamWriter("output.txt")
    resultStrings |> String.concat("\n") |> writer.Write
    writer.Close()

let zad2 seed = 
    let t = 365
    let n = 250
    let paths = initPaths 1000 (geoBrownMotion 250 300.0 0.001 0.01 365) seed
    let resultStrings = [for path in paths do yield (Seq.last path |> string) + " " + (getHistoricalVolatility path (float t) (float n) |> string)]
    let writer = new IO.StreamWriter("zad2.csv")
    resultStrings |> String.concat("\n") |> writer.Write
    writer.Close()

[<EntryPoint>]
let main argv =
    zad1 1000 200 300.0 0.001 0.1 300 1234
    0