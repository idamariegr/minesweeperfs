module maps
open System
open fields
open System.Collections.Generic

let fileToChars (path : string) : char[,] =
    array2D(IO.File.ReadLines(path) |> Seq.toArray |> Seq.map(fun x -> x.ToCharArray()))

let incrementSafe (map : field[,]) (r : int) (c : int) =
    let cur = map.[r,c]
    match cur with
         | Bomb -> ()
         | Safe x -> map.[r,c] <- Safe (x + 1)

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

let updateNumsMap(originalMap : field[,]) ( n : int ) ( m : int ) =
    for r in 0..n-1 do
        for c in 0..m-1 do
            if originalMap.[r,c] = Bomb then
                updateSafes originalMap n m r c

let getFieldMap (lines : char[,]) (n : int) (m : int) : field[,] =
    let newlines = lines |> Array2D.map charToField
    updateNumsMap newlines n m
    newlines

let rec clearUp ( fieldmap : field[,] ) ( secret : char[,] ) ( exposed : char[,])
                ( r : int ) ( c : int )
                ( n : int ) ( m : int )
                ( visited : HashSet<int * int> )
                ( counter : byref<int> ) =
    if r < 0 || r >= n || c < 0 || c >= m then ()
    elif visited.Contains (r, c) then ()
    else
        visited.Add (r, c) |> ignore
        exposed.[r,c] <- secret.[r,c]
        counter <- counter + 1
        match fieldmap.[r,c] with
             | Safe 0 ->
                 for rr in -1..1 do
                     for cc in -1..1 do
                         clearUp fieldmap secret exposed (r+rr) (c+cc) n m visited &counter
             | _ -> ()
