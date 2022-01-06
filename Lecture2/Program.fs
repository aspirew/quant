// lista 1 / zad 5
let perm ls =
    let rec getListWithoutWord s l = 
        match l with
            | h::t -> 
                if h.Equals(s) then t
                else h::(getListWithoutWord s t)
            | [] -> l

    let rec idxList i ls = 
        match ls with
            | h::t -> (i, h)::(idxList (i+1) t)
            | [] -> []

    let rec permInner ls =
        [for w in ls do
            match ls with 
                | [e] -> yield [e]
                | _ -> yield! [for others in getListWithoutWord w ls |> permInner do yield w::others]
        ]

    permInner ls |> idxList 1

// zad 1
let rec fold func acc ls = 
    match ls with
        | h::t -> fold func (func acc h) t
        | _ -> acc

let foldBack func ls acc =
    let rec foldBackInner ls acc =
        match ls with
            | h::t -> func h acc |> foldBackInner t
            | [] -> acc

    foldBackInner (List.rev ls) acc


// zad 2

let someParse res =
    match res with
        | None -> 0
        | Some x -> x

let foldFun acc elem =
    match elem with
        | x when x < 0 -> match acc with
                            | None -> Some x
                            | Some v -> elem + v |> Some
        | _ -> match acc with
                | None -> None
                | Some x -> elem + x |> Some    

let sumAheadOwn ls = fold foldFun None ls |> someParse
let sumAheadBuiltIn ls = List.fold foldFun None ls |> someParse

// zad 3

let backFoldFun elem acc = foldFun acc elem
let sumBackOwn ls = foldBack backFoldFun ls None |> someParse
let sumBackBuiltIn ls = List.foldBack backFoldFun ls None |> someParse

// zad 4

let letterCounter ls =

    let removeEmptyElements ls =
        let rec removeHelper ls acc =
            match ls with
                | h::t when System.String.IsNullOrWhiteSpace h -> removeHelper t acc
                | h::t -> h::acc |> removeHelper t
                | _ -> acc
        removeHelper ls []

    let removeWhiteSpaces ls =
        let rec removerHelper ls acc =
            match ls with
                | h::t -> (String.filter (not << System.Char.IsWhiteSpace) h)::acc |> removerHelper t
                | _ -> acc
        removerHelper ls []

    let counter (ls: list<string>) =
        let addPositionsSum len result = result + (len - 1) * len / 2 
        [for word in ls do yield word.Length] |> fold (fun e1 e2 -> e1 + e2) 0 |> addPositionsSum ls.Length

    removeEmptyElements ls |> removeWhiteSpaces |> counter