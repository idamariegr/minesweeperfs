module maps
open System
open fields

let charToField (s : char) : field =
    match s with
        | '.' -> Safe 0
        | '*' -> numbombs <- numbombs+1; Bomb
        | _ -> failwithf "unknown charachter encountered when reading map: %A" s

let fileToChars (path : string) : char[,] =
    array2D(IO.File.ReadLines(path) |> Seq.toArray |> Seq.map(fun x -> x.ToCharArray()))

let incrementSafe (map : field[,]) (r : int) (c : int) =
    let cur = map.[r,c]
    if not (cur = Bomb) then
        let s =
            match cur with
            | Safe x -> Safe (x + 1)
        map.[r,c] <- s

let updateSafes (map : field[,]) (n : int) (m : int) (r : int) (c : int) =
    if r > 0 then // above, middle
        incrementSafe map (r-1) c // upper middle
        if c > 0 then incrementSafe map (r-1) (c-1) // upper left corner
        if c < n-1 then incrementSafe map (r-1) (c+1) // upper right corner
    if r < n-1 then // below, middle
        incrementSafe map (r+1) c // lower middle
        if c > 0 then incrementSafe map (r+1) (c-1) // lower left corner
        if c < n-1 then incrementSafe map (r+1) (c+1) // lower right corner
    if c > 0 then incrementSafe map r (c-1) // left middle
    if c < n-1 then incrementSafe map r (c+1) // right middle

let updateNumsMap(originalMap : field[,]) =
    let n = originalMap |> Array2D.length1
    let m = originalMap |> Array2D.length2
    for r in 0..n-1 do
        for c in 0..m-1 do
            if originalMap.[r,c] = Bomb then
                updateSafes originalMap n m r c

let getFieldMap (lines : char[,]) (n : int) (m : int) : field[,] =
    let newlines = lines |> Array2D.map charToField
    updateNumsMap newlines
    newlines
